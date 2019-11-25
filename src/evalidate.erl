-module(evalidate).
-include("evalidate.hrl").
-include("ev_errors.hrl").
-export([
  validate_and_convert/2,
  validate_and_convert/3
]).

-export([
  validation_error_resp/3
]).

-record(state, {
  opts = [],
  parents = []
}).

-export([
%%  size_validator/4,
  validate_password/1
]). %% exporting for evalidate_lib.hrl

-spec validate_and_convert(rules(), list()) -> {ok| error, Result :: list()}|no_return().
validate_and_convert(Rules, Data) ->
  validate_and_convert(Rules, Data, []).

validate_and_convert(Rules, Data, Opts) when is_list(Opts) ->
  State = #state{opts = Opts},
  case eutils:get_value(mode, Opts) of
    soft ->
      case catch process_struct(Rules, Data, State) of
        {error, Result} ->
          {error, Result};
        Result ->
          {ok, Result}
      end;
    _ ->
      process_struct(Rules, Data, State)
  end.

process_struct([], _, _) -> [];
process_struct(Rules, Data, State) ->
  case {Rules, Data} of
    %% Rules = [ Rules1..RulesN ],   Data = [ Data1..DataN ]
    {[RList | _], [DList | _]} when is_list(DList), is_list(RList), length(Rules) =:= length(Data) ->
      lists:zipwith(fun(R, D) ->
        process_rules(R, D, State) end, Rules, Data);

    {_, [DList | _]} when is_list(DList), is_list(Rules) ->
      lists:map(fun(DataSegment) -> process_rules(Rules, DataSegment, State) end, Data);

    {_, D} when is_list(D) ->
      process_rules(Rules, Data, State);
    _ ->
      error_mess(?ERR_MALFORMED_DATA)
  end.

process_rules([], _Data, _State) -> [];
process_rules(Rule, Data, State) when is_tuple(Rule) -> %% 'rule', 'rule_or', 'rule_and'
  process_rule(Rule, Data, State);
process_rules([H | Rules], Data, State) ->
  Res = [process_rule(H, Data, State) | process_rules(Rules, Data, State)],
  lists:flatten(Res);
process_rules(NotValid, _Data, _) ->
  error_mess(?ERR_UNK_RULES(NotValid)).

process_rule(Rule, Data, State) when is_record(Rule, 'rule') ->
  process_keys(Rule, Data, State);
process_rule(Rule, Data, State) when is_record(Rule, 'rule_or') ->
  process_or(Rule, Data, State);
process_rule(Rule, Data, State) when is_record(Rule, 'rule_and') ->
  process_and(Rule, Data, State);
process_rule(UnknownRule, _, _) ->
  error_mess(?ERR_UNK_RULES(UnknownRule)).

%% ----------------------------OR------------------
process_or(#rule_or{list = List, on_error = ErrorMessage}, Data, State) when is_binary(ErrorMessage) ->
  try process_or(#rule_or{list = List}, Data, State)
  catch
    throw:{error, _} ->
      throw({error, ErrorMessage})
  end;
process_or(#rule_or{list = List}, Data, State) when is_list(List) ->
  Fun = fun(Cond, Data_) -> process_struct(Cond, Data_, State) end,
  or_logic(Fun, List, Data);
process_or(#rule_and{list = WrongParams}, _, _) ->
  error_mess(?ERR_WRONG_PARAMS(rule_or, WrongParams)).

%% ---------------------AND-------------------------------
process_and(#rule_and{list = List, on_error = ErrorMessage}, Data, State) when is_binary(ErrorMessage) ->
  try process_and(#rule_and{list = List}, Data, State)
  catch throw:{error, _} ->
    throw({error, ErrorMessage})
  end;
process_and(#rule_and{list = List}, Data, State) when is_list(List) ->
  process_struct(List, Data, State);
process_and(#rule_and{list = WrongParams }, _Data, _) ->
  error_mess(?ERR_WRONG_PARAMS(rule_and, WrongParams)).


%%----------------------RULE--------------------------------
process_keys(Rule = #rule{key = none}, Data, State) -> %% Top level rule
  process_validators(Rule, Data, Data, State);
process_keys(Rule = #rule{key = Keys}, Data, State) when is_list(Keys) ->
  case io_lib:printable_list(Keys) of
    true ->
      process_presence(Rule#rule{key = Keys}, Data, State); %% Differentiate strings
    false ->
      lists:map(fun(Key) -> process_presence(Rule#rule{key = Key}, Data, State) end, Keys)
  end;

process_keys(Rule, Data, State) ->
  process_presence(Rule, Data, State).

process_presence(Rule = #rule{key = Key, presence = Presence}, Data, State) ->
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
      process_validators(Rule, Value, Data, State)
  end.

process_validators(Rule = #rule{validators = V}, Value, Data, State) when V == none; V == [] ->
  process_nesting(Rule, Value, Data, State);

process_validators(Rule = #rule{key = Key, validators = Validators, on_validate_error = OnError}, Value, Data, State) ->
  case evv:validate(Validators, Value, Data, State#state.opts) of
    {error, Message} ->
      throw({error, Message});
    {false, ValidationMessage} ->
      ParentKey = get_keyname(Key, State),
      Message = resolve_message(ValidationMessage, OnError, Key, Value),
      throw({error, validation_error_resp(Message, ParentKey, Value)});
    true ->
      process_nesting(Rule, Value, Data, State)
  end.

process_nesting(Rule = #rule{childs = Empty}, Value, Data, _) when Empty == none; Empty == [] ->
  process_convert(Rule, Value, Data);

process_nesting(Rule = #rule{childs = Childs, presence = optional}, [], Data, _) when Childs =/= none ->
  process_convert(Rule, [], Data);

process_nesting(Rule = #rule{key = Key, childs = Childs}, Value, Data, #state{parents = P0} = State) when is_list(Childs) ->
  Parents = [Key | P0],
  process_convert(Rule, process_struct(Childs, Value, State#state{parents = Parents}), Data);

process_nesting(#rule{key = Key}, _Value, _Data, State) ->
  ParentKey = get_keyname(Key, State),
  error_mess(?ERR_WRONG_CHILDS(ParentKey)).

process_convert(#rule{converter = no_return}, _Value, _Data) ->
  [];

process_convert(#rule{key = Key, converter = Converter}, Value, Data) ->
  convert(Key, Converter, Value, Data).

%%----------------------------------------------------------------------------------------------------------------------
%%                  CONVERTERS
%%----------------------------------------------------------------------------------------------------------------------
-spec convert(key(), converter(), term(), list()) -> term()|no_return().
convert(Key, Converter, Value, Data) ->

  try
    ConvertedValue =
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
          error_mess(?ERR_WRONG_CONVERTER(Key, Value))
      end,
    case Key of
      none ->
        ConvertedValue;
      _ -> {Key, ConvertedValue}
    end
  catch
    error:_Reas ->
      error_mess(?ERR_COULDNT_CONVERT(Value, Key))
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
  Message2 = binary:replace(Message1, <<"{{key}}">>, Key, [global]),
  binary:replace(Message2, <<"{{value}}">>, Value, [global]).

validation_error_resp(Message, Key, Value) ->
  [{message, Message}, {key, Key}, {value, Value}].

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
or_logic(_, [], _, AllErrors) ->
  Message = eutils:bjoin(filter_duplicates(AllErrors), <<" or ">>),
  error_mess(Message).

get_keyname(Key, #state{opts = Opts, parents = Parents}) ->
  case eutils:get_value(parent_key, Opts) of
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

validate_password(Password) ->
  evv:validate_password(Password).