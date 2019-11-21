-module(evalidate).
-include("evalidate.hrl").
-export([
  validate_and_convert/2,
  validate_and_convert/3
]).

-export([
  size_validator/4,
  validate_password/1
]). %% exporting for evalidate_lib.hrl

-spec validate_and_convert( rules(), list()) -> {ok| error, Result :: list()}|no_return().
validate_and_convert(Rules, Data) ->
  validate_and_convert(Rules, Data, []).

validate_and_convert(Rules, Data, Opts) when is_list(Opts) ->
  case eutils:get_value(mode, Opts) of
    soft ->
      case catch process_struct(Rules, Data, Opts, []) of
        {error, Result} -> {error, Result};
        Result -> {ok, Result}
      end;
    undefined -> process_struct(Rules, Data, Opts, [])
  end.

process_struct([], _, _, _) -> [];
process_struct(Rules, Data, Opts, Parents) ->
  case {Rules, Data} of
    %% Rules = [ Rules1..RulesN ],   Data = [ Data1..DataN ]
    {[RList|_], [DList|_]} when is_list(DList), is_list(RList), length(Rules) =:= length(Data) ->
      lists:zipwith(fun(R, D) ->
        process_rules(R, D, Opts, Parents) end, Rules, Data);

    {_, [DList|_]} when is_list(DList), is_list(Rules) ->
      lists:map(fun(DataSegment) ->  process_rules(Rules, DataSegment, Opts, Parents) end, Data);

    {_, D} when is_list(D) ->
      process_rules(Rules, Data, Opts, Parents);
    _ ->
      error_mess(<<"Mallformed validation data">>)
  end.

process_rules([], _Data, _, _) -> [];
process_rules(Rule, Data, Opts, Parents) when is_tuple(Rule) -> %% 'rule', 'rule_or', 'rule_and'
  process_rule(Rule, Data, Opts, Parents);
process_rules([H|Rules], Data, Opts, Parents) ->
  Res = [process_rule(H, Data, Opts, Parents) | process_rules(Rules, Data, Opts, Parents)],
  lists:flatten(Res);
process_rules(NotValid, _Data, _, _) ->
  error_mess("unknown rules format '~p'", [NotValid]).

process_rule(Rule, Data, Opts, Parents) when is_record(Rule, 'rule') ->
  process_keys(Rule, Data, Opts, Parents);
process_rule(Rule, Data, Opts, Parents) when is_record(Rule, 'rule_or') ->
  process_or(Rule, Data, Opts, Parents);
process_rule(Rule, Data, Opts, Parents) when is_record(Rule, 'rule_and') ->
  process_and(Rule, Data, Opts, Parents);
process_rule(UnknownRule, _, _, _) ->
  error_mess("Unknown validation rule: '~p'", [UnknownRule]).

%% ----------------------------OR------------------
process_or(#rule_or{list = List, on_error = ErrorMessage }, Data, Opts, Parents) when is_binary(ErrorMessage) ->
  try process_or(#rule_or{list = List}, Data, Opts, Parents)
  catch
    throw:{error, _} ->
      throw({error, ErrorMessage})
  end;
process_or(#rule_or{list = List }, Data, Opts, Parents) when is_list(List) ->
  Fun = fun(Cond, Data_) -> process_struct(Cond, Data_, Opts, Parents) end,
  or_logic(Fun, List, Data);
process_or(_, _, _, _) ->
  error_mess(<<"'OR' list params is not valid">>).

%% ---------------------AND-------------------------------
process_and(#rule_and{list = List, on_error = ErrorMessage }, Data, Opts, Parents) when is_binary(ErrorMessage) ->
  try process_and(#rule_and{list = List}, Data, Opts, Parents)
  catch throw:{error, _} ->
    throw({error, ErrorMessage})
  end;
process_and(#rule_and{list = List}, Data, Opts, Parents) when is_list(List) ->
  process_struct(List, Data, Opts, Parents);
process_and(#rule_and{list = List}, _Data, _, _) ->
  error_mess("Wrong parameters for #rule_and.~n~p' ", [List]).


%%----------------------RULE--------------------------------
process_keys(Rule = #rule{key = none}, Data, Opts, Parents) -> %% Top level rule
  process_validators(Rule, Data, Data, Opts, Parents);
process_keys( Rule = #rule{key = Keys}, Data, Opts, Parents) when is_list(Keys) ->
  case io_lib:printable_list(Keys) of
    true ->
      process_presence(Rule#rule{key = Keys}, Data, Opts, Parents); %% Differentiate strings
    false ->
      lists:map(fun(Key) ->  process_presence(Rule#rule{key = Key}, Data, Opts, Parents) end, Keys )
  end;

process_keys(Rule, Data, Opts, Parents) ->
  process_presence(Rule, Data, Opts, Parents).

process_presence(Rule = #rule{key = Key, presence = Presence}, Data, Opts, Parents) ->
  ParentKey = handle_parent(Opts, [Key|Parents]),
  case eutils:get_value(Key, Data) of
    undefined ->
      case Presence of
        {optional, Default} -> {Key, Default};
        required ->
          error_mess("Key '~ts' is required", [ParentKey]);
        _ -> [] %% optional|deprecated
      end;

    _Value when Presence == deprecated ->
      error_mess("Key '~ts' is deprecated", [ParentKey]);

    Value ->
      process_validators(Rule, Value, Data, Opts, Parents)
  end.

process_validators( Rule = #rule{validators = V}, Value, Data, Opts, Parents) when V == none; V == [] ->
  process_nesting(Rule, Value, Data, Opts, Parents);

process_validators( Rule = #rule{key = Key, validators = Validators, on_validate_error = OnError}, Value, Data, Opts, Parents) when is_list(Validators) orelse is_tuple(Validators) orelse is_function(Validators, 1)->
  do_validate(Validators, Key, Value, Data, Opts, Parents, OnError),
  process_nesting(Rule, Value, Data, Opts, Parents);

process_validators( #rule{key = Key, validators = V}, _Value, _Data, Opts, Parents) ->
  ParentKey = handle_parent(Opts, [Key|Parents]),
  error_mess("Wrong validator ~p for key '~ts' ", [V, ParentKey]).


process_nesting(Rule = #rule{ childs = none}, Value, Data, _, _) ->
  process_convert( Rule, Value, Data);

process_nesting(Rule = #rule{ childs = Childs, presence = optional}, [], Data, _, _) when Childs =/= none ->
  process_convert( Rule, [], Data);

process_nesting( Rule = #rule{childs = Childs}, Value, Data, Opts, Parents) when is_list(Childs), length(Childs) > 0 ->
  process_convert(Rule, process_struct(Childs, Value, Opts, [Rule#rule.key|Parents]), Data);

process_nesting( #rule{key = Key}, _Value, _Data, Opts, Parents) ->
  ParentKey  = handle_parent(Opts, [Key|Parents]),
  error_mess("Wrong childs for key '~ts'", [ParentKey]).

process_convert( #rule{converter = no_return}, _Value, _Data) ->
  [];

process_convert( #rule{key = Key, converter = Converter}, Value, Data) ->
  convert(Key, Converter, Value, Data).

%%----------------------------------------------------------------------------------------------------------------------
%%                  VALIDATORS
%%----------------------------------------------------------------------------------------------------------------------
do_validate(Validators, Key, Value, Data, Opts, Parents, OnError) when is_list(Validators) ->
  ok =:= lists:foreach(fun(Validator) ->
    validate_(Validator, Key, Value, Data, Opts, Parents, OnError)
                       end, Validators);

do_validate(Validator, Key, Value, Data, Opts, Parents, OnError) -> %% tuple and fun
  validate_(Validator, Key, Value, Data, Opts, Parents, OnError).


validate_(Type, Key, Value, Data, Opts, Parents, OnError) ->
  Result =
    case Type of
      {'or', ListOfConds} when is_list(ListOfConds), length(ListOfConds) > 1 ->
        validate_or(ListOfConds, Key, Value, Data, Opts, Parents, OnError);
      {type, Predefined} when is_atom(Predefined); is_list(Predefined) ->
        validate_type(Predefined, Value);
      {size, {From, To}} when (is_integer(From) orelse From == infinity), (is_integer(To) orelse (To == infinity)) ->
        validate_size(From, To, Value, OnError);
      {regexp, Regexp} when is_binary(Regexp) ->
        validate_with_regexp(Regexp, Value, OnError);
      {allowed_values, AlowedValues} when is_list(AlowedValues), length(AlowedValues) > 0 ->
        lists:member(Value, AlowedValues) orelse error_mess_param("Value '~ts' is not allowed for key '~ts'", [Value, Key], OnError);
      {allowed, AlowedValues} when is_list(AlowedValues), length(AlowedValues) > 0 -> %% @todo
        lists:member(Value, AlowedValues) orelse error_mess_param("Value '~ts' is not allowed for key '~ts'", [Value, Key], OnError);
      {is_equal_to_object_of_other_keys, Keys} ->
        is_equal_to_object_of_other_keys(Value, {Keys, Data});
      Fun when is_function(Fun, 1) ->
        try Fun(Value) of
          Res when is_boolean(Res) -> Res;
          {false, Message} when is_binary(Message) ->
            error_mess(Message);
          _ -> error_mess(<<"Wrong validation function">>)
        catch
          error:_Err -> false;
          throw:{error, Reas} -> throw({error, Reas})
        end;
      Fun when is_function(Fun, 2) ->
        try Fun(Value, Data) of
          Res when is_boolean(Res) -> Res;
          {false, Message} when is_binary(Message) ->
            error_mess(Message);
          _ -> error_mess(<<"Wrong validation function">>)
        catch
          error:_Err -> false;
          throw:{error, Reas} -> throw({error, Reas})
        end;
      _ -> error_mess("Unknown validator '~p'", [Type])
    end,
  ParentKey = handle_parent(Opts, [Key|Parents]),
  Result =:= true orelse error_mess_param("Value '~ts' is not valid for key '~ts'", [Value, ParentKey], OnError).

validate_or(ListOfConds, Key, Value, Data, Opts, Parents, OnError) ->
  Fun = fun(Conds, Val) -> do_validate(Conds, Key, Val, Data, Opts, Parents, OnError) end,
  or_logic(Fun, ListOfConds, Value).

%% -----------------TYPE VALIDATION-------------------------------------------------------------------------------------
validate_type(List, Value) when is_list(List) ->
  lists:any(fun(Type) -> validate_type(Type, Value) end, List);
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
validate_size(MinSize, MaxSize, Value, OnError) when is_binary(Value) ->
  Size =
    case unicode:characters_to_list(Value) of
      List when is_list(List) -> length(List);
      _ -> size(Value)
    end,
  size_validator(byte_size, MinSize, MaxSize, Size, OnError);

validate_size(MinSize, MaxSize, Value, OnError) when is_list(Value) ->
  Size = length(Value),
  size_validator(length, MinSize, MaxSize, Size, OnError);

validate_size(MinSize, MaxSize, Value, OnError) when is_number(Value) ->
  Size = Value,
  size_validator(limit, MinSize, MaxSize, Size, OnError).

size_validator(Parameter, MinSize, MaxSize, Size) ->
  size_validator(Parameter, MinSize, MaxSize, Size, none).
size_validator(Parameter, MinSize, MaxSize, Size, OnError) ->
  case Size of
    Size when MinSize =/= infinity, Size < MinSize -> error_mess_param("Less than minimum allowed ~ts ~ts", [Parameter, MinSize], OnError);
    Size when MaxSize =/= infinity, Size > MaxSize -> error_mess_param("More than maximum allowed ~ts ~ts", [Parameter, MaxSize], OnError);
    Size -> true
  end.

%%%%-----------------REGEXP VALIDATION----------------------------------------------------------------------------------
validate_with_regexp(RegExp, Value, OnError) when is_binary(Value), is_binary(RegExp) ->
  (re:run(Value, RegExp, [{capture, none}]) =:= match )
    orelse error_mess_param("Validate with regexp '~ts' failed for value '~ts'", [RegExp, Value], OnError);

validate_with_regexp(_, _, _) ->
  throw({error, <<"Bad regexp">>}).

%%----------------------------------------------------------------------------------------------------------------------
%%                  CONVERTERS
%%----------------------------------------------------------------------------------------------------------------------
-spec convert(key(), converter(), term(), list() ) -> term()|no_return().
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
          error_mess("Wrong converter for key '~ts' value '~ts'", [Key, Value])
      end,
    case Key of
      none ->
        ConvertedValue;
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
  throw({error, ErrString}).

error_mess_param(Message, Params, none) ->
  error_mess(Message, Params);
error_mess_param(_Message, _Params, OnError) ->
  throw({error, OnError}).

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
    is_equal_to_object_of_other_keys(List, {Key, Data})
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

handle_parent(Opts, Parents)   ->
  case eutils:get_value(parent_key, Opts) of
    true ->
      lists:foldl(
        fun
          (K, <<>>) -> eutils:to_bin(K);
          (K, Acc)  -> <<Acc/binary, ".", (eutils:to_bin(K))/binary>>
        end,
        <<>>,
        lists:reverse(Parents)
      );
    _ ->
      hd(Parents)
  end.

validate_password(Password0) ->
  Password = eutils:to_str(Password0),
  case length(Password) of
    Length when Length < 9 -> throw({error, <<"Password length must be at least 9 characters">>});
    Length when Length > 256 -> throw({error, <<"Password length must be shorter than 256 characters">>});
    _ -> skip
  end,
  4 == (check_lowercase(Password) + check_uppercase(Password) + check_alphanumeric(Password) + check_special(Password))
    orelse throw({error, <<"Password must contain at least one uppercase, one lowercase, one special symbol and one numeric symbol">>}).

check_lowercase(Password) ->
  check(Password,"[a-z]").

check_uppercase(Password) ->
  check(Password,"[A-Z]").

check_alphanumeric(Password) ->
  check(Password,"\\d").

check_special(Password) ->
  check(Password,"[\\\\[!\"#$%&\'()*+,-./:;<=>?@^_`{|}~\\]]").

check(Value, Regexp) ->
  case re:run(eutils:to_str(Value), eutils:to_str(Regexp), [{capture, none}]) of
    match -> 1;
    _ -> 0
  end.