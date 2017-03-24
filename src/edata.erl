-module(edata).
-include("edata.hrl").
-export([
  validate_and_convert/2,
  validate_and_convert/3
]).

-spec validate_and_convert(list(), rules()) -> {ok|error|validate_runtime_error, Result :: list()}|no_return().
validate_and_convert(ToValidate, Rules) ->
  validate_and_convert(ToValidate, Rules, []).

validate_and_convert(Rules, Data, Opts) ->
  case get_value(mode, Opts) of
    undefined -> process_data_struct(Rules, Data);
    soft ->
      case catch (process_data_struct(Rules, Data)) of
        {error, Result} ->
          throw({validate_error, Result});
        Result ->
          {ok, Result}
      end
  end.

process_data_struct(Rules, Data) ->
  case {Rules, Data} of

    {[RList|_], [DList|_]} when is_list(DList), is_list(RList), length(Rules) =:= length(Data) ->
      {Results, _} =
        lists:foldl(
          fun(DataSegment, {Results, [RulesSegment|T]}) ->
            Res = process_branching(RulesSegment, DataSegment),
            {[Res|Results], T}
          end, {[], Rules}, Data),
      Results;

    {_, [DList|_]} when is_list(DList), is_list(Rules) ->
      lists:map(fun(DataSegment) ->
        process_branching(Rules, DataSegment) end, Data);

    {_R, D} when is_list(D) ->
      process_branching(Rules, Data);

    _ ->
      lager:error("Validation rules ~p~n or input data ~p are not valid", [Rules, Data ]),
      error_mess(<<"Mallformed validation data">>)
  end.

process_branching(Rule, Data) when is_tuple(Rule) ->
  process_branching([Rule], Data);

process_branching(Rules, Data) ->
  process_branching(Rules, Data, []).

process_branching([Rule|Rules], Data, Acc) when is_record(Rule, 'rule') ->
  case process(Rule, Data) of
    Res when is_tuple(Res) -> process_branching(Rules, Data, [Res|Acc]); %% @todo переделать
    Res when is_list(Res) -> process_branching(Rules, Data, Res ++ Acc)
  end;

process_branching([Rule|Rules], Data, Acc) when is_record(Rule, 'rule_or') ->
  Res = process_or(Rule, Data),
  process_branching(Rules, Data, Res ++ Acc);

process_branching([Rule|Rules], Data, Acc) when is_record(Rule, 'rule_and') ->
  Res = process_group(Rule, Data),
  process_branching(Rules, Data, Res ++ Acc);

process_branching([], _Data, Acc) ->
  Acc.

%% ----------------------------OR------------------
process_or(#rule_or{list = List}, Data) when is_list(List), length(List) > 1 ->
  Fun = fun(Cond, Data_) -> process_data_struct(Cond, Data_) end,
  or_logic(Fun, List, Data);

process_or(_, _) ->
  error_mess(<<"'OR' list params is not valid">>).

%% ---------------------GROUP-------------------------------
process_group(#rule_and{list = List}, Data) when is_list(List), length(List) > 1 ->
  process_data_struct(List, Data);

process_group(#rule_and{list = List}, _Data ) ->
  error_mess("Group list ~p is not valid", [List]).


%%----------------------RULE--------------------------------
process(Rule, Data) ->
  process(keys, Rule, Data).

process(keys, Rule = #rule{key = none}, Data) -> %% Top level rule
  process(nesting, Rule, Data);

process(keys, Rule = #rule{key = Keys}, Data) when is_list(Keys) ->
  lists:foldl(fun(Key, Acc) ->
    Res = process(presence, Rule#rule{key = Key}, Data),
    case Res of
      [] -> Acc; %% @todo optimize
      _  -> [Res|Acc]
    end  end, [], Keys);

process(keys, Rule, Data) ->
  process(presence, Rule, Data);

process(presence, Rule = #rule{key = Key, presence = Presence}, Data) ->
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
      process(nesting, Rule, Value)
  end;

process(nesting, Rule = #rule{ childs = none}, Value) ->
  process(validators, Rule, Value);

process(nesting, #rule{key = Key, childs = Childs}, Value) when is_list(Childs), length(Childs) > 0 ->
  case Key of
    none ->
      process_data_struct(Childs, Value);
    _ ->
      {Key, process_data_struct(Childs, Value)}
  end;

process(nesting, #rule{key = Key}, _Value) ->
  error_mess("Wrong childs for key ~p", [Key]);

process(validators, Rule = #rule{validators = Validators}, Value) when is_list(Validators), length(Validators) > 0 ->
  do_validate(Validators, Value),
  process(convert, Rule, Value);

process(validators, Rule = #rule{validators = none}, Value) ->
  process(convert, Rule, Value);

process(validators, #rule{key = Key}, _) ->
  error_mess("Wrong validator for key ~p ", [Key]);

process(convert, #rule{key = Key, converter = Converter}, Value) ->
  convert(Key, Converter, Value).

%%----------------------------------------------------------------------------------------------------------------------
%%                  VALIDATORS
%%----------------------------------------------------------------------------------------------------------------------
do_validate(Validators, Value) when is_list(Validators) ->
  lists:map(
    fun(Validator) ->
      case validate_(Validator, Value) of
        false -> error_mess("Value ~p is not valid", [Value]);
        true -> ok
      end
    end, Validators),
  true;

do_validate(Validator, Value) when is_tuple(Validator) ->
  validate_(Validator, Value).

-spec validate_(term(), term()) -> ok|no_return().
validate_(Type, Value) ->
  case Type of
    {'or', ListOfConds} when is_list(ListOfConds), length(ListOfConds) > 1 ->
      validate_or(ListOfConds, Value);
    {type, Predefined} when is_atom(Predefined) ->
      validate_type(Predefined, Value);
    {size, {From, To}} when is_integer(From), is_integer(To) ->
      validate_size(From, To, Value);
    {regexp, Regexp} when is_binary(Regexp) ->
      validate_with_regexp(Regexp, Value);
    {alowed_values, AlowedValues} when is_list(AlowedValues), length(AlowedValues) > 0 ->
      lists:member(Value, AlowedValues) orelse error_mess("Value ~p is not alowed", [Value]);
    Fun when is_function(Fun, 1) -> Fun(Value);
    _ -> error_mess("Wrong validator ~p", [Type])
  end.

validate_or(ListOfConds, Value) ->
  Fun = fun(Conds, Data) -> do_validate(Conds, Data) end,
  or_logic(Fun, ListOfConds, Value).

%% -----------------TYPE VALIDATION-------------------------------------------------------------------------------------
validate_type(binary, Value) ->
  is_binary(Value);
validate_type(list, Value) ->
  is_list(Value);
validate_type(ulist, Value) ->
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
  size_validator( MinSize, MaxSize, Size);

validate_size(MinSize, MaxSize, Value) when is_list(Value) ->
  Size = length(Value),
  size_validator(MinSize, MaxSize, Size);

validate_size(MinSize, MaxSize, Value) when is_number(Value) ->
  Size = Value,
  size_validator(MinSize, MaxSize, Size).

size_validator(MinSize, MaxSize, Size) ->
  case Size of
    Size when Size >= MinSize, Size =< MaxSize -> true;
    Size when Size < MinSize -> error_mess("Less than minimum allowed length ~p", [MinSize]);
    Size -> error_mess("More than maximum allowed length ~p", [MaxSize])
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
          ConvFun(Value);
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
error_mess(Message) when is_binary(Message) ->
  throw({error, Message}).
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
    true -> error_mess("Element ~p is not unique in list", [H])
  end.

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
  error_mess("All variants in OR list are not valid ~n~p", [AllErrors]).

%%-spec recursive_reverse(list()) -> list().
%%recursive_reverse(List) ->
%%  recursive_reverse(List, []).
%%recursive_reverse([H|T], Acc) when is_list(H) ->
%%  recursive_reverse(T, [recursive_reverse(H)|Acc]);
%%recursive_reverse([H|T], Acc) ->
%%  recursive_reverse(T, [H|Acc]);
%%recursive_reverse([], Acc) ->
%%  Acc.

get_value(Key, List)->
  get_value(Key, List, undefined).
get_value(Key, List, Default)->
  case lists:keyfind(Key, 1, List) of
    {_, Val} -> Val;
    _        -> Default
  end.