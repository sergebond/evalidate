-module(evalidate).
-include("evalidate.hrl").
-export([
  validate_and_convert/2,
  validate_and_convert/3
]).

-export([size_validator/4]). %% exporting for evalidate_lib.hrl

-spec validate_and_convert( rules(), list()) -> {ok| error, Result :: list()}|no_return().
validate_and_convert(Rules, ToValidate) ->
  validate_and_convert(Rules, ToValidate, []).

validate_and_convert(Rules, Data, Opts) ->
  case get_value(mode, Opts) of
    undefined -> process_struct(Rules, Data);
    soft ->
      case catch (process_struct(Rules, Data)) of
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
  lists:flatten(Res).

process_rule(Rule, Data) when is_record(Rule, 'rule') ->
  process_keys(Rule, Data);
process_rule(Rule, Data) when is_record(Rule, 'rule_or') ->
  process_or(Rule, Data);
process_rule(Rule, Data) when is_record(Rule, 'rule_and') ->
  process_and(Rule, Data);
process_rule(UnknownRule, _) ->
  error_mess("Unknown validation rule: ~p", [UnknownRule]).

%% ----------------------------OR------------------
process_or(#rule_or{list = List}, Data) when is_list(List), length(List) > 1 ->
  Fun = fun(Cond, Data_) -> process_struct(Cond, Data_) end,
  or_logic(Fun, List, Data);
process_or(_, _) ->
  error_mess(<<"'OR' list params is not valid">>).

%% ---------------------AND-------------------------------
process_and(#rule_and{list = List}, Data) when is_list(List), length(List) > 1 ->
  process_struct(List, Data);
process_and(#rule_and{list = List}, _Data ) ->
  error_mess("Group list ~p is not valid", [List]).


%%----------------------RULE--------------------------------
process_keys(Rule = #rule{key = none}, Data) -> %% Top level rule
  process_nesting(Rule, Data, Data);
process_keys( Rule = #rule{key = Keys}, Data) when is_list(Keys) ->
  lists:map(fun(Key) ->  process_presence(Rule#rule{key = Key}, Data) end, Keys );
process_keys(Rule, Data) ->
  process_presence(Rule, Data).

process_presence(Rule = #rule{key = Key, presence = Presence}, Data) ->
  case get_value(Key, Data) of
    undefined ->
      case Presence of
        {optional, Default} -> {Key, Default};
        required ->
          error_mess("Key ~p is required", [Key]);
        _ -> [] %% optional|deprecated
      end;

    _Value when Presence =:= deprecated ->
      error_mess("Key ~p is deprecated", [Key]);

    Value ->
      process_nesting(Rule, Value, Data)
  end.

process_nesting(Rule = #rule{ childs = none}, Value, Data) ->
  process_validators( Rule, Value, Data);

process_nesting( Rule = #rule{childs = Childs}, Value, Data) when is_list(Childs), length(Childs) > 0 ->
  process_validators(Rule, process_struct(Childs, Value), Data);

process_nesting( #rule{key = Key}, _Value, _Data) ->
  error_mess("Wrong childs for key ~p", [Key]).

process_validators( Rule = #rule{validators = Validators}, Value, Data) when (is_list(Validators) andalso length(Validators) > 0) orelse is_tuple(Validators) ->
  do_validate(Validators, Value, Data),
  process_convert(Rule, Value, Data);

process_validators( Rule = #rule{validators = none}, Value, Data) ->
  process_convert(Rule, Value, Data);

process_validators( #rule{key = Key}, _Value, _Data) ->
  error_mess("Wrong validator for key ~p ", [Key]).

process_convert( #rule{key = Key, converter = Converter}, Value, _Data) ->
  convert(Key, Converter, Value).

%%----------------------------------------------------------------------------------------------------------------------
%%                  VALIDATORS
%%----------------------------------------------------------------------------------------------------------------------
do_validate(Validators, Value, Data) when is_list(Validators) ->
  ok =:= lists:foreach(fun(Validator) ->
    validate_(Validator, Value, Data)
                       end, Validators);

do_validate(Validator, Value, Data) when is_tuple(Validator) ->
  validate_(Validator, Value, Data).


-spec validate_(term(), term(), list()) -> boolean()|no_return().
validate_(Type, Value, Data) ->
  Result =
    case Type of
      {'or', ListOfConds} when is_list(ListOfConds), length(ListOfConds) > 1 ->
        validate_or(ListOfConds, Value, Data);
      {type, Predefined} when is_atom(Predefined) ->
        validate_type(Predefined, Value);
      {size, {From, To}} when (is_integer(From) orelse From == infinity), (is_integer(To) orelse (To == infinity)) ->
        validate_size(From, To, Value);
      {regexp, Regexp} when is_binary(Regexp) ->
        validate_with_regexp(Regexp, Value);
      {alowed_values, AlowedValues} when is_list(AlowedValues), length(AlowedValues) > 0 ->
        lists:member(Value, AlowedValues) orelse error_mess("Value ~p is not alowed", [Value]);
      {is_equal_to_object_of_other_keys, Keys} ->
        is_equal_to_object_of_other_keys(Value, {Keys, Data});
      Fun when is_function(Fun, 1) ->
        case Fun(Value) of
          Res when is_boolean(Res) -> Res;
          _ -> error_mess("Wrong validation function")
        end;
      _ -> error_mess("Wrong validator ~p", [Type])
    end,
  Result =:= true orelse error_mess("Value ~p is not valid", [Value]).

validate_or(ListOfConds, Value, Data) ->
  Fun = fun(Conds, Val) -> do_validate(Conds, Val, Data) end,
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
  error_mess("Unknown type validator ~p ", [Type]).

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
    Size when MinSize =/= infinity, Size < MinSize -> error_mess("Less than minimum allowed ~p ~p", [Parameter, MinSize]);
    Size when MaxSize =/= infinity, Size > MaxSize -> error_mess("More than maximum allowed ~p ~p", [Parameter, MaxSize]);
    Size -> true
  end.

%%%%-----------------REGEXP VALIDATION----------------------------------------------------------------------------------
validate_with_regexp(RegExp, Value) when is_binary(Value), is_binary(RegExp) ->
  (re:run(Value, RegExp, [{capture, none}]) =:= match )
    orelse error_mess("Validate with regexp ~p failed for value ~p", [RegExp, Value]);

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
        to_int -> to_int(Value);
        to_list -> to_str(Value);
        to_atom -> to_atom(Value);
        to_float -> to_float(Value);
        to_binary -> to_bin(Value);
        to_boolean -> to_boolean(Value);
        filter_duplicates ->
          filter_duplicates(Value);
        ConvFun when is_function(ConvFun, 1) ->
          case ConvFun(Value) of
            {error, Message} -> error_mess(Message);
            Res -> Res
          end ;
        _ ->
          error_mess("Wrong converter for key ~p value ~p", [Key, Value])
      end,
    case Key of
      none -> ConvertedValue;
      _ -> {Key, ConvertedValue}
    end
  catch
    error:_Reas ->
      error_mess("Couldnt convert value ~p for key ~p ", [Value, Key])
  end.

%%----------------------------------------------------------------------------------------------------------------------
%%                  ERRORS
%%----------------------------------------------------------------------------------------------------------------------
-spec error_mess(binary()) -> no_return().
error_mess(Message) when is_binary(Message) ->
  throw({error, Message}).
-spec error_mess(binary(), list()) -> no_return().
error_mess( Message, Params) when is_list(Message), is_list(Params) ->
  ErrString = lists:flatten(io_lib:format(Message, Params)),
%%  lager:error(ErrString), %% @todo
  throw({error, list_to_binary(ErrString)}).

%%----------------------------------------------------------------------------------------------------------------------
%%                  INTERNAL
%%----------------------------------------------------------------------------------------------------------------------

%%------------------TYPE CONVERSION-------------------------------------------------------------------------------------
%% @doc universal converter to binary
-spec to_bin(binary()|list()|integer()|atom()|float()) -> binary().
to_bin(X) when is_binary(X) -> X;
to_bin(X) when is_list(X) -> list_to_binary(X);
to_bin(X) when is_integer(X) -> integer_to_binary(X);
to_bin(X) when is_atom(X) -> atom_to_binary(X, utf8);
to_bin(X) when is_float(X) -> float_to_binary(X, [{decimals, 4}]).

%% @doc universal converter to string(list)
-spec to_str(binary()|list()|integer()|atom()|float()) -> list().
to_str(X) when is_list(X) -> X;
to_str(X) when is_binary(X) -> binary_to_list(X);
to_str(X) when is_integer(X) -> integer_to_list(X);
to_str(X) when is_atom(X) -> atom_to_list(X);
to_str(X) when is_float(X) -> float_to_list(X,[{decimals, 4}]).

%% @doc universal converter to integer
-spec to_int(binary()|list()|integer()|atom()) -> integer().
to_int(X) when is_integer(X) -> X;
to_int(X) when is_binary(X) -> binary_to_integer(X);
to_int(X) when is_list(X) -> list_to_integer(X);
to_int(X) when is_float(X) -> round(X);
to_int(X) when is_atom(X) -> list_to_integer(atom_to_list(X)).

%% @doc universal converter to float
-spec to_float(binary()|list()|float()) -> float().
to_float(X) when is_float(X) -> X;
to_float(X) when is_binary(X) -> binary_to_float(X);
to_float(X) when is_list(X) -> list_to_float(X).

%% @doc universal converter to atom
-spec to_atom(binary()|list()|float()) -> float().
to_atom(X) when is_binary(X) -> binary_to_atom(X, utf8);
to_atom(X) when is_list(X) -> binary_to_atom(list_to_binary(X), utf8).

to_boolean(X) when X =:= <<"true">> orelse X =:= <<"false">> -> binary_to_atom( X , utf8);
to_boolean(X) when X =:= true orelse X =:= false ->  X;
to_boolean(X) when  X =:= "true" orelse X =:= "false" -> binary_to_atom(list_to_binary(X), utf8).

%%----------------------COMPLEX VALIDATIONS-----------------------------------------------------------------------------
is_list_of_equal_objects(List) when length(List) =< 1 -> false;
is_list_of_equal_objects(List) ->
  case List of
    [Obj0| Tail] when is_list(Obj0), length(Obj0) > 0 ->
      BasicLength = length(Obj0),
      BasicKeys = [K||{K,_} <- Obj0],
%%      lager:error("~p ", [BasicKeys]),
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
    true -> error_mess("Key ~p is not unique in list", [K])
  end;
is_unique_proplist([H|T]) ->
  case lists:member(H, T) of
    false -> is_unique_proplist(T);
    true -> error_mess("key ~p is not unique in list", [H])
  end.

is_equal_to_object_of_other_keys(List, {Keys, Data}) when is_list(List), is_list(Keys) ->
  lists:all(fun(Key) ->
    AnotherList = get_value(Key, Data),
    is_list_of_equal_objects([List, AnotherList])
            end, Keys);
is_equal_to_object_of_other_keys(List, {Key, Data}) when is_list(List) ->
  AnotherList = get_value(Key, Data),
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
  Message = binary_join(filter_duplicates(AllErrors), <<" or ">>),
  error_mess(Message).

get_value(Key, List)->
  get_value(Key, List, undefined).
get_value(Key, List, Default)->
  case lists:keyfind(Key, 1, List) of
    {_, Val} -> Val;
    _        -> Default
  end.

-spec binary_join(List :: list(binary()), Separator :: binary()) -> binary().
binary_join(List, Separator) ->
  lists:foldl(fun(Item, Acc) when bit_size(Acc) > 0 ->
    <<Acc/binary, Separator/binary, Item/binary>>;
    (Item, _Acc) -> Item
              end, <<>>, List).