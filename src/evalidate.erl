-module(evalidate).
-include("evalidate.hrl").
-export([
  validate_and_convert/2,
  validate_and_convert/3
]).

-export([size_validator/4]). %% exporting for evalidate_lib.hrl

-spec validate_and_convert( rules(), list()) -> {ok| error, Result :: list()}|no_return().
validate_and_convert(Rules, Data) ->
  validate_and_convert(Rules, Data, []).

validate_and_convert(Rules, Data, Opts) when is_list(Opts) ->
  case eutils:get_value(mode, Opts) of
    undefined -> process_struct(Rules, Data);
    soft ->
      case catch process_struct(Rules, Data) of
        {error, Result} ->
          {error, Result};
        Result ->
          {ok, Result}
      end
  end.

process_struct(Rules, Data) ->
  case {Rules, Data} of
    %% Rules = [ Rules1..RulesN ],   Data = [ Data1..DataN ]
    {[RList|_], [DList|_]} when is_list(DList), is_list(RList), length(Rules) =:= length(Data) ->
      lists:zipwith(fun(R, D) ->
        process_rules(R, D) end, Rules, Data);

    {_, [DList|_]} when is_list(DList), is_list(Rules) ->
      lists:map(fun(DataSegment) ->  process_rules(Rules, DataSegment) end, Data);

    {_, D} when is_list(D) ->
      process_rules(Rules, Data);

    _ ->
      error_mess(<<"Mallformed validation data">>)
  end.

process_rules([], _Data) -> [];
process_rules(Rule, Data) when is_tuple(Rule) ->
  process_rule(Rule, Data);
process_rules([H|Rules], Data) ->
  Res = [process_rule(H, Data)| process_rules(Rules, Data)],
  lists:flatten(Res);
process_rules(NotValid, _Data) ->
  error_mess("unknown rules format '~p'", [NotValid]).

process_rule(Rule, Data) when is_record(Rule, 'rule') ->
  process_keys(Rule, Data);
process_rule(Rule, Data) when is_record(Rule, 'rule_or') ->
  process_or(Rule, Data);
process_rule(Rule, Data) when is_record(Rule, 'rule_and') ->
  process_and(Rule, Data);
process_rule(UnknownRule, _) ->
  error_mess("Unknown validation rule: '~p'", [UnknownRule]).

%% ----------------------------OR------------------
process_or(#rule_or{list = List, on_error = ErrorMessage }, Data) when is_binary(ErrorMessage) ->
  try process_or(#rule_or{list = List}, Data)  catch throw:{error, _} -> throw({error, ErrorMessage})  end;
process_or(#rule_or{list = List }, Data) when is_list(List), length(List) > 1 ->
  Fun = fun(Cond, Data_) -> process_struct(Cond, Data_) end,
  or_logic(Fun, List, Data);
process_or(_, _) ->
  error_mess(<<"'OR' list params is not valid">>).

%% ---------------------AND-------------------------------
process_and(#rule_and{list = List, on_error = ErrorMessage }, Data) when is_binary(ErrorMessage) ->
  try process_and(#rule_and{list = List}, Data)  catch throw:{error, _} -> throw({error, ErrorMessage})  end;
process_and(#rule_and{list = List}, Data) when is_list(List), length(List) > 1 ->
  process_struct(List, Data);
process_and(#rule_and{list = List}, _Data ) ->
  error_mess("Wrong parameters for #rule_and.~nThe length of list must be greater than 1 ~n'~p' ", [List]).


%%----------------------RULE--------------------------------
process_keys(Rule = #rule{key = none}, Data) -> %% Top level rule
  process_nesting(Rule, Data, Data);
process_keys( Rule = #rule{key = Keys}, Data) when is_list(Keys) ->
  case io_lib:printable_list(Keys) of
    true ->
      process_presence(Rule#rule{key = Keys}, Data); %% Differentiate strings
    false ->
      lists:map(fun(Key) ->  process_presence(Rule#rule{key = Key}, Data) end, Keys )
  end;

process_keys(Rule, Data) ->
  process_presence(Rule, Data).

process_presence(Rule = #rule{key = Key, presence = Presence}, Data) ->
  case eutils:get_value(Key, Data) of
    undefined ->
      case Presence of
        {optional, Default} -> {Key, Default};
        required ->
          error_mess("Key '~ts' is required", [Key]);
        _ -> [] %% optional|deprecated
      end;

    _Value when Presence == deprecated ->
      error_mess("Key '~ts' is deprecated", [Key]);

    Value ->
      process_nesting(Rule, Value, Data)
  end.

process_nesting(Rule = #rule{ childs = none}, Value, Data) ->
  process_validators( Rule, Value, Data);

process_nesting( Rule = #rule{childs = Childs}, Value, Data) when is_list(Childs), length(Childs) > 0 ->
  process_validators(Rule, process_struct(Childs, Value), Data);

process_nesting( #rule{key = Key}, _Value, _Data) ->
  error_mess("Wrong childs for key '~ts'", [Key]).

process_validators( Rule = #rule{key = Key, validators = Validators}, Value, Data) when (is_list(Validators) andalso length(Validators) > 0) orelse is_tuple(Validators) ->
  do_validate(Validators, Key, Value, Data),
  process_convert(Rule, Value, Data);

process_validators( Rule = #rule{validators = none}, Value, Data) ->
  process_convert(Rule, Value, Data);

process_validators( #rule{key = Key, validators = V}, _Value, _Data) ->
  error_mess("Wrong validator ~p for key '~ts' ", [V, Key]).

process_convert( #rule{converter = no_return}, _Value, _Data) ->
  [];

process_convert( #rule{key = Key, converter = Converter}, Value, _Data) ->
  convert(Key, Converter, Value).

%%----------------------------------------------------------------------------------------------------------------------
%%                  VALIDATORS
%%----------------------------------------------------------------------------------------------------------------------
do_validate(Validators, Key, Value, Data) when is_list(Validators) ->
  ok =:= lists:foreach(fun(Validator) ->
    validate_(Validator, Key, Value, Data)
                       end, Validators);

do_validate(Validator, Key, Value, Data) when is_tuple(Validator) ->
  validate_(Validator, Key, Value, Data).

-spec validate_(tuple()|function(), key(), term(), list()) -> boolean()|no_return().
validate_(Type, Key, Value, Data) ->
  Result =
    case Type of
      {'or', ListOfConds} when is_list(ListOfConds), length(ListOfConds) > 1 ->
        validate_or(ListOfConds, Key, Value, Data);
      {type, Predefined} when is_atom(Predefined) ->
        validate_type(Predefined, Value);
      {size, {From, To}} when (is_integer(From) orelse From == infinity), (is_integer(To) orelse (To == infinity)) ->
        validate_size(From, To, Value);
      {regexp, Regexp} when is_binary(Regexp) ->
        validate_with_regexp(Regexp, Value);
      {allowed_values, AlowedValues} when is_list(AlowedValues), length(AlowedValues) > 0 ->
        lists:member(Value, AlowedValues) orelse error_mess("Value '~ts' is not allowed for key '~ts'", [Value, Key]);
      {allowed, AlowedValues} when is_list(AlowedValues), length(AlowedValues) > 0 -> %% @todo
        lists:member(Value, AlowedValues) orelse error_mess("Value '~ts' is not allowed for key '~ts'", [Value, Key]);
      {is_equal_to_object_of_other_keys, Keys} ->
        is_equal_to_object_of_other_keys(Value, {Keys, Data});
      Fun when is_function(Fun, 1) ->
        case Fun(Value) of
          Res when is_boolean(Res) -> Res;
          _ -> error_mess("Wrong validation function")
        end;
      _ -> error_mess("Unknown validator '~p'", [Type])
    end,
  Result =:= true orelse error_mess("Value '~ts' is not valid for key '~ts'", [Value, Key]).

validate_or(ListOfConds, Key, Value, Data) ->
  Fun = fun(Conds, Val) -> do_validate(Conds, Key, Val, Data) end,
  or_logic(Fun, ListOfConds, Value).

%% -----------------TYPE VALIDATION-------------------------------------------------------------------------------------
validate_type(binary, Value) ->
  is_binary(Value);
validate_type(list, Value) ->
  is_list(Value);
validate_type(uniq_list, Value) ->
  is_list(Value) andalso is_unique_proplist(Value);
validate_type(tuple, Value) ->
  is_tuple(Value);
validate_type(boolean, Value) ->
  is_boolean(Value);
validate_type(integer, Value) ->
  is_integer(Value);
validate_type(atom, Value) ->
  is_atom(Value);
validate_type(float, Value) ->
  is_float(Value);
validate_type(number, Value) ->
  is_number(Value);
validate_type(list_of_equal_objects, Value) ->
  is_list_of_equal_objects(Value);
validate_type(Type, _) ->
  error_mess("Unknown type validator '~p' ", [Type]).

%%%%-----------------SIZE VALIDATION------------------------------------------------------------------------------------
validate_size(MinSize, MaxSize, Value) when is_binary(Value) ->
  Size = byte_size(Value),
  size_validator(byte_size, MinSize, MaxSize, Size);

validate_size(MinSize, MaxSize, Value) when is_list(Value) ->
  Size = length(Value),
  size_validator(length, MinSize, MaxSize, Size);

validate_size(MinSize, MaxSize, Value) when is_number(Value) ->
  Size = Value,
  size_validator(limit, MinSize, MaxSize, Size).

size_validator(Parameter, MinSize, MaxSize, Size) ->
  case Size of
    Size when MinSize =/= infinity, Size < MinSize -> error_mess("Less than minimum allowed ~ts ~ts", [Parameter, MinSize]);
    Size when MaxSize =/= infinity, Size > MaxSize -> error_mess("More than maximum allowed ~ts ~ts", [Parameter, MaxSize]);
    Size -> true
  end.

%%%%-----------------REGEXP VALIDATION----------------------------------------------------------------------------------
validate_with_regexp(RegExp, Value) when is_binary(Value), is_binary(RegExp) ->
  (re:run(Value, RegExp, [{capture, none}]) =:= match )
    orelse error_mess("Validate with regexp '~ts' failed for value '~ts'", [RegExp, Value]);

validate_with_regexp(_, _) ->
  throw({error, <<"Bad regexp">>}).

%%----------------------------------------------------------------------------------------------------------------------
%%                  CONVERTERS
%%----------------------------------------------------------------------------------------------------------------------
-spec convert(key(), converter(), term() ) -> term()|no_return().
convert(Key, Converter, Value) ->
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
          end ;
        _ ->
          error_mess("Wrong converter for key '~ts' value '~ts'", [Key, Value])
      end,
    case Key of
      none -> ConvertedValue;
      _ -> {Key, ConvertedValue}
    end
  catch
    error:_Reas ->
      error_mess("Couldn't convert value '~ts' for key '~ts' ", [Value, Key])
  end.

%%----------------------------------------------------------------------------------------------------------------------
%%                  ERRORS
%%----------------------------------------------------------------------------------------------------------------------
-spec error_mess(binary()) -> no_return().
error_mess(Message) when is_binary(Message) ->
  throw({error, Message}).
-spec error_mess(binary(), list()) -> no_return().
error_mess( Message, Params) when is_list(Message), is_list(Params) ->
  BinParams = lists:map(
    fun(X) when is_number(X) -> eutils:to_bin(X);
      (X) when is_binary(X) -> X;
      (X) -> unicode:characters_to_binary(io_lib:format("~p", [X])) end, Params),
  ErrString = unicode:characters_to_binary(io_lib:format(Message, BinParams)),
%%  lager:error(ErrString), %% @todo
  throw({error, ErrString}).

%%----------------------------------------------------------------------------------------------------------------------
%%                  INTERNAL
%%----------------------------------------------------------------------------------------------------------------------

%%----------------------COMPLEX VALIDATIONS-----------------------------------------------------------------------------
is_list_of_equal_objects(List) when length(List) =< 1 -> false;
is_list_of_equal_objects(List) ->
  case List of
    [Obj0| Tail] when is_list(Obj0), length(Obj0) > 0 ->
      BasicLength = length(Obj0),
      BasicKeys = [K||{K,_} <- Obj0],
      lists:all(fun(Obj) when is_list(Obj), length(Obj) =:= BasicLength ->
        lists:all(fun(Key) -> lists:keymember(Key, 1, Obj) end, BasicKeys);
        (_) -> false end , Tail);
    _ -> false
  end.

is_unique_proplist(List) when length(List) =< 1 -> true;
is_unique_proplist([]) -> true;
is_unique_proplist([{K, _V}|T]) ->
  case lists:keymember(K, 1, T) of
    false -> is_unique_proplist(T);
    true -> error_mess("Key '~ts' is not unique in list", [K])
  end;
is_unique_proplist([H|T]) ->
  case lists:member(H, T) of
    false -> is_unique_proplist(T);
    true -> error_mess("key '~ts' is not unique in list", [H])
  end.

is_equal_to_object_of_other_keys(List, {Keys, Data}) when is_list(List), is_list(Keys) ->
  lists:all(fun(Key) ->
    AnotherList = eutils:get_value(Key, Data),
    is_list_of_equal_objects([List, AnotherList])
            end, Keys);
is_equal_to_object_of_other_keys(List, {Key, Data}) when is_list(List) ->
  AnotherList = eutils:get_value(Key, Data),
  is_list_of_equal_objects([List, AnotherList]).

%%----------------------COMPLEX CONVERTERS------------------------------------------------------------------------------
%%remove duplicates from_list
filter_duplicates(List) -> filter_duplicates(List, []).
filter_duplicates([], _) -> [];
filter_duplicates([{K, _V} = KV|T], Acc) ->
  case lists:member(K, Acc) of
    false -> [KV | filter_duplicates(T, [K | Acc])];
    true -> filter_duplicates(T, Acc)
  end;
filter_duplicates([H|T], Acc) ->
  case lists:member(H, Acc) of
    false -> [H | filter_duplicates(T, [H | Acc])];
    true -> filter_duplicates(T, Acc)
  end.

%%---------------------HELPERS------------------------------------------------------------------------------------------
%% or logics with errors folding
-spec or_logic((fun((Conds :: list(), Data :: term()) -> no_return()|term())) , Conds :: list(), Data :: term() ) -> no_return()|term().
or_logic(Fun, Conds, Data) ->
  or_logic(Fun, Conds, Data, []).
or_logic(Fun, [Condition|Conds], Data, ErrorsAcc) ->
  try
    Fun(Condition, Data)
  catch
    {error, Reason} ->
      or_logic(Fun, Conds, Data, [Reason|ErrorsAcc])
  end;
or_logic(_, [], _, AllErrors) ->
  Message = eutils:bjoin(filter_duplicates(AllErrors), <<" or ">>),
  error_mess(Message).