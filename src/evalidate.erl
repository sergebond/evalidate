-module(evalidate).
-include("evalidate.hrl").

-export([
  validate_and_convert/2,
  validate_and_convert/3
]).

-record(state, {
  opts = [],
  parents = []
}).

-export([
  validate_password/1
]).

-spec validate_and_convert(rules(), list()) -> {ok| error, Result :: list()}|no_return().
validate_and_convert(Rules, Data) ->
  validate_and_convert(Rules, Data, []).

validate_and_convert(Rules, Data, Opts) when is_list(Opts) ->
  State = #state{opts = Opts},
  case eutils:get_value(mode, Opts) of
    soft ->
      case catch struct(Rules, Data, State) of
        {error, Result} ->
          {error, Result};
        Result ->
          {ok, Result}
      end;
    _ ->
      struct(Rules, Data, State)
  end.

validate_password(Password) ->
  evv:validate_password(Password).

%% PRIVATE______________________________________________________________________________________________________________
struct([], _, _) -> [];
struct(Rules, Data, State) ->
  case {Rules, Data} of
    %% Rules = [ Rules1..RulesN ],   Data = [ Data1..DataN ]
    {[RList | _], [DList | _]} when is_list(DList), is_list(RList), length(Rules) =:= length(Data) ->
      lists:zipwith(fun(R, D) ->
        rules(R, D, State) end, Rules, Data);

    {_, [DList | _]} when is_list(DList), is_list(Rules) ->
      lists:map(fun(DataSegment) -> rules(Rules, DataSegment, State) end, Data);

    _ ->
      rules(Rules, Data, State)
  end.

rules([], _Data, _State) -> [];
rules(Rule, Data, State) when is_tuple(Rule) -> %% 'rule', 'rule_or', 'rule_and'
  rule(Rule, Data, State);
rules([H | Rules], Data, State) ->
  Res = [rule(H, Data, State) | rules(Rules, Data, State)],
  lists:flatten(Res);
rules(NotValid, _Data, _) ->
  error_mess(?ERR_UNK_RULES(NotValid)).

rule(Rule, Data, State) when is_record(Rule, 'rule') ->
  'keys'(Rule, Data, State);
rule(Rule, Data, State) when is_record(Rule, 'rule_or') ->
  'or'(Rule, Data, State);
rule(Rule, Data, State) when is_record(Rule, 'rule_and') ->
  'and'(Rule, Data, State);
rule(UnknownRule, _, _) ->
  error_mess(?ERR_UNK_RULES(UnknownRule)).

%% ----------------------------OR------------------
'or'(#rule_or{list = List, on_error = ErrorMessage}, Data, State) when is_binary(ErrorMessage) ->
  try 'or'(#rule_or{list = List}, Data, State)
  catch
    throw:{error, _} ->
      throw({error, ErrorMessage})
  end;
'or'(#rule_or{list = List}, Data, State) when is_list(List) ->
  Fun = fun(Cond, Data_) -> struct(Cond, Data_, State) end,
  or_logic(Fun, List, Data);
'or'(#rule_and{list = WrongParams}, _, _) ->
  error_mess(?ERR_WRONG_PARAMS(rule_or, WrongParams)).

%% ---------------------AND-------------------------------
'and'(#rule_and{list = List, on_error = ErrorMessage}, Data, State) when is_binary(ErrorMessage) ->
  try 'and'(#rule_and{list = List}, Data, State)
  catch throw:{error, _} ->
    throw({error, ErrorMessage})
  end;
'and'(#rule_and{list = List}, Data, State) when is_list(List) ->
  struct(List, Data, State);
'and'(#rule_and{list = WrongParams }, _Data, _) ->
  error_mess(?ERR_WRONG_PARAMS(rule_and, WrongParams)).


%%----------------------RULE--------------------------------
'keys'(Rule = #rule{key = none}, Data, State) -> %% Top level rule
  validators(Rule, Data, Data, State);
'keys'(Rule = #rule{key = Keys}, Data, State) when is_list(Keys) ->
  case io_lib:printable_list(Keys) of
    true ->
      presence(Rule#rule{key = Keys}, Data, State); %% Differentiate strings
    false ->
      lists:map(fun(Key) -> presence(Rule#rule{key = Key}, Data, State) end, Keys)
  end;

'keys'(Rule, Data, State) ->
  presence(Rule, Data, State).

presence(Rule = #rule{key = Key, presence = Presence}, Data, State) ->
  ParentKey = get_keyname(Key, State),
  case eutils:get_value(Key, Data) of
    undefined ->
      case Presence of
        {optional, Default} -> {Key, Default};
        required ->
          error_mess(?ERR_KEY_IS_REQUIRED(ParentKey));
        _ -> [] %% optional|deprecated
      end;

    _Value when Presence == deprecated ->
      error_mess(?ERR_KEY_IS_DEPRECATED(Key));

    Value ->
      validators(Rule, Value, Data, State)
  end.

validators(Rule = #rule{validators = V}, Value, Data, State) when V == none; V == [] ->
  nesting(Rule, Value, Data, State);

validators(Rule = #rule{key = Key, validators = Validators, on_validate_error = OnError}, Value, Data, State) ->
  case evv:validate(Validators, Value, Data, State#state.opts) of
    {error, Message} ->
      throw({error, Message});
    {false, ValidationMessage} ->
      ParentKey = get_keyname(Key, State),
      Message = resolve_message(ValidationMessage, OnError, Key, Value),
      throw({error, ?V_ERR_MESSAGE(Message, ParentKey, Value)});
    true ->
      nesting(Rule, Value, Data, State)
  end.

nesting(Rule = #rule{childs = Empty}, Value, Data, _) when Empty == none; Empty == [] ->
  convert(Rule, Value, Data);


nesting(Rule = #rule{key = Key, childs = Childs}, Value, Data, #state{parents = P0} = State) when is_list(Childs) ->
  Parents = [Key | P0],
  convert(Rule, struct(Childs, Value, State#state{parents = Parents}), Data);

nesting(#rule{key = Key}, _Value, _Data, State) ->
  ParentKey = get_keyname(Key, State),
  error_mess(?ERR_WRONG_CHILDS(ParentKey)).

convert(#rule{converter = no_return}, _Value, _Data) ->
  [];

convert(#rule{key = Key, converter = Converter}, Value, Data) ->
  convert(Key, Converter, Value, Data).

%%----------------------------------------------------------------------------------------------------------------------
%%                  CONVERTERS
%%----------------------------------------------------------------------------------------------------------------------
-spec convert(key(), converter(), term(), list()) -> term()|no_return().
convert(Key, Converter, Value, Data) ->

  try
    ConvertedValue = convert_(Converter, Value, Data),
    case Key of
      none ->
        ConvertedValue;
      _ -> {Key, ConvertedValue}
    end
  catch
    error:_Reas ->
      error_mess(?ERR_COULDNT_CONVERT(Value, Key))
  end.

convert_(Converter, Value, Data) ->
  case Converter of
    none -> Value;
    to_int -> eutils:to_int(Value);
    to_list -> eutils:to_str(Value);
    to_atom -> eutils:to_atom(Value);
    to_float -> eutils:to_float(Value);
    to_binary -> eutils:to_bin(Value);
    to_boolean -> eutils:to_boolean(Value);
    filter_duplicates ->
      filter_duplicates(Value);
    {each, Converter1} when is_list(Value) ->
      lists:map(fun(ListElem) -> convert_(Converter1, ListElem, Data) end, Value);
    ConvFun when is_function(ConvFun, 1) ->
      case ConvFun(Value) of
        {error, Message} -> error_mess(Message);
        Res -> Res
      end;
    ConvFun when is_function(ConvFun, 2) -> %%todo tests
      case ConvFun(Value, Data) of
        {error, Message} -> error_mess(Message);
        Res -> Res
      end;
    _ ->
      error_mess(<<"Wrong converter">>)
  end.

%%----------------------------------------------------------------------------------------------------------------------
%%                  ERRORS
%%----------------------------------------------------------------------------------------------------------------------
error_mess(Message) when is_binary(Message) ->
  throw({error, Message}).

resolve_message(Message, none, _Key, _Value) ->
  Message;
resolve_message(_Message, OnError, Key, Value) ->
  resolve_on_error_message(OnError, Key, Value).
resolve_on_error_message(Message0, Key0, Value0) ->
  Message1 = eutils:to_bin(Message0),
  Key = eutils:to_bin(Key0),
  Value = eutils:to_bin(Value0),
  Message2 = binary:replace(Message1, <<"{{key}}">>, ?UNSCRIPTIZE(Key), [global]),
  binary:replace(Message2, <<"{{value}}">>, ?UNSCRIPTIZE(Value), [global]).

%%----------------------------------------------------------------------------------------------------------------------
%%                  INTERNAL
%%----------------------------------------------------------------------------------------------------------------------
%%----------------------COMPLEX CONVERTERS------------------------------------------------------------------------------
%%remove duplicates from_list
filter_duplicates(List) -> filter_duplicates(List, []).
filter_duplicates([], _) -> [];
filter_duplicates([{K, _V} = KV | T], Acc) ->
  case lists:member(K, Acc) of
    false -> [KV | filter_duplicates(T, [K | Acc])];
    true -> filter_duplicates(T, Acc)
  end;
filter_duplicates([H | T], Acc) ->
  case lists:member(H, Acc) of
    false -> [H | filter_duplicates(T, [H | Acc])];
    true -> filter_duplicates(T, Acc)
  end.

%%---------------------HELPERS------------------------------------------------------------------------------------------
or_logic(Fun, Conds, Data) ->
  or_logic(Fun, Conds, Data, []).
or_logic(Fun, [Condition | Conds], Data, ErrorsAcc) ->
  try
    Fun(Condition, Data)
  catch
    {error, Reason} ->
      or_logic(Fun, Conds, Data, [Reason | ErrorsAcc])
  end;
or_logic(_, [], _, AllErrors0) ->
  AllErrors = lists:map(fun(Error) when is_list(Error) -> eutils:get_value(message, Error); (Err) -> Err  end, AllErrors0),

  Message = eutils:bjoin(filter_duplicates(AllErrors), <<" or ">>),
  error_mess(Message).

get_keyname(Key, #state{opts = Opts, parents = Parents}) ->
  case eutils:get_value(parent_key, Opts, true) of
    true ->
      lists:foldl(
        fun
          (K, <<>>) -> eutils:to_bin(K);
          (K, Acc) -> <<Acc/binary, ".", (eutils:to_bin(K))/binary>>
        end,
        <<>>,
        lists:reverse([Key | Parents])
      );
    _ -> Key
  end.
