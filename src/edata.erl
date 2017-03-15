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
  case eutils:get_value(mode, Opts) of
    undefined -> process_data_struct(Rules, Data);
    soft ->
      case catch (process_data_struct(Rules, Data)) of
        {ok, Result} ->
          {ok, Result};
        {error, Result} ->
          throw({validate_error, Result});
        {'EXIT', {Reason, Stacktrace}} ->
          lager:error("Validate Error ~p~nStacktrace is ~p", [Reason, Stacktrace]),
          {validate_runtime_error,  Reason}
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

    {_, [DList|_]} when is_list(DList), length(Rules) =:= 1 ->
      lists:map(fun(DataSegment) ->
        process_branching(Rules, DataSegment) end, Data);

    {_R, D} when is_list(D), length(D) > 0 ->
      process_branching(Rules, Data);

    _ ->
%%      lager:error("Validation rules ~p~nor input data ~p are not valid", [Rules, Data ]),
      error_mess(<<"Mallformed validation data">>)
  end.


process_branching(Rule, Data) when is_tuple(Rule) ->
  process_branching([Rule], Data);

process_branching(Rules, Data) ->
  process_branching(Rules, Data, []).

process_branching([Rule|Rules], Data, Acc) when is_record(Rule, 'rule') ->
  case process(Rule, Data) of
    Res when is_tuple(Res) -> process_branching(Rules, Data, [Res|Acc]);
    Res when is_list(Res) -> process_branching(Rules, Data, Res ++ Acc)
  end;

process_branching([Rule|Rules], Data, Acc) when is_record(Rule, 'rule_or') ->
  Res = process_or(Rule, Data),
  process_branching(Rules, Data, Res ++ Acc);

process_branching([Rule|Rules], Data, Acc) when is_record(Rule, 'group') ->
  Res = process_group(Rule, Data),
  process_branching(Rules, Data, Res ++ Acc);

process_branching([], _Data, Acc) ->
  Acc.

%% ----------------------------OR------------------
process_or(#rule_or{list = List}, Data) when is_list(List), length(List) > 1 ->
  'or'(List, Data);

process_or(_, _) ->
  error_mess(<<"'OR' list params is not valid">>).

'or'([H|T], Data) ->
  try
    process_data_struct(H, Data)
  catch
    _:_Reas -> 'or'(T, Data)
  end;

'or'([], _Data) ->
  error_mess(<<"All variants in OR list are not valid">>).

%% ---------------------GROUP-------------------------------
process_group(#group{list = List}, Data) when is_list(List), length(List) > 1 ->
  process_data_struct(List, Data);

process_group(#group{list = List}, _Data ) ->
  error_mess("Group list ~p is not valid", [List]).


%%----------------------RULE--------------------------------
process(Rule, Data) ->
  process(keys, Rule, Data).

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
  case eutils:get_value(Key, Data) of
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
  {Key, process_data_struct(Childs, Value)};

process(nesting, #rule{key = Key}, _Value) ->
  error_mess("Wrong childs for key ~p", [Key]);

process(validators, Rule = #rule{validators = Validators}, Value) when is_list(Validators), length(Validators) > 0 ->
  lists:map(
    fun(Validator) ->
      case validate_(Validator, Value) of
        false -> error_mess("Value ~p is not valid", [Value]);
        _ -> ok
      end
    end, Validators),
  process(convert, Rule, Value);

process(validators, Rule = #rule{validators = none}, Value) ->
  process(convert, Rule, Value);

process(validators, #rule{key = Key}, _) ->
  error_mess("Wrong validator for key ~p ", [Key]);

process(convert, #rule{key = Key, converter = none}, Value) ->
  {Key, Value};

process(convert, #rule{key = Key, converter = Converter}, Value) ->
  convert(Key, Converter, Value).

%%----------------------------------------------------------------------------------------------------------------------
%%                  VALIDATORS
%%----------------------------------------------------------------------------------------------------------------------
-spec validate_(term(), term()) -> ok|no_return().
validate_(Type, Value) ->
  case Type of
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

%% -----------------TYPE VALIDATION-------------------------------------------------------------------------------------
validate_type(binary_integer, Value) ->
  case catch binary_to_integer(Value) of
    _Int when is_integer(_Int) ->  true;
    _ -> false
  end;
validate_type(binary, Value) ->
  is_binary(Value);
validate_type(list, Value) ->
  is_list(Value);
validate_type(tuple, Value) ->
  is_tuple(Value);
validate_type(boolean, Value) ->
  is_boolean(Value);
validate_type(integer, Value) ->
  is_integer(Value);
validate_type(atom, Value) ->
  is_atom(Value);
validate_type(Type, _) ->
  error_mess("Unknown type validator ~p ", [Type]).

%%%%-----------------SIZE VALIDATION------------------------------------------------------------------------------------
validate_size(MinSize, MaxSize, Value) when is_binary(Value) ->
  Size = byte_size(Value),
  size_validator( MinSize, MaxSize, Size);

validate_size(MinSize, MaxSize, Value) when is_list(Value) ->
  Size = length(Value),
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
convert(Key, Converter, Value) -> %% @todo add not binary vals
  try
    ConvertedValue =
      case Converter of
        to_int -> eutils:to_int(Value);
        to_list -> eutils:to_str(Value);
        to_atom -> eutils:to_atom(Value);
        to_float -> eutils:to_float(Value);
        to_binary -> eutils:to_bin(Value);
        ConvFun when is_function(ConvFun, 1) ->
          ConvFun(Value);
        _ ->
          error_mess("Wrong converter for key ~p value ~p", [Key, Value])
      end,
    {Key, ConvertedValue}
  catch
    error:_Reas ->
      error_mess("Couldnt convert value ~p for key ~p ", [Value, Key])
  end.

%%----------------------------------------------------------------------------------------------------------------------
%%                  ERRORS
%%----------------------------------------------------------------------------------------------------------------------
error_mess(Message) when is_binary(Message) ->
%%  lager:error("~p", [Message]),
  throw({error, Message}).
error_mess( Message, Params) when is_list(Message), is_list(Params) ->
  ErrString = lists:flatten(io_lib:format(Message, Params)),
%%  lager:error(ErrString),
  throw({error, list_to_binary(ErrString)}).

%%----------------------------------------------------------------------------------------------------------------------
%%                  INTERNAL
%%----------------------------------------------------------------------------------------------------------------------
