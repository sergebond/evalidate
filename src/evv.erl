-module(evv).
-author("sergeybondarchuk").
-include("ev_errors.hrl").

%%-define(ERR_UNK_VALIDATOR(Unknown),  error_str( << "Unknown validator '~ts'">>, [Unknown]) ).
%%-define(ERR_UNK_TYPE_VALIDATOR(Unknown), error_str(<< "Unknown type validator '~p'">>, [Unknown])).
%%-define(ERR_BAD_REGEXP, <<"Bad regexp">> ).
%%-define(ERR_WRONG_FUN, <<"Wrong validation function">> ).
%%
%%-define(V_ERR_DEFAULT, <<"Value is not valid">>).
%%
%%-define(V_ERR_WRONG_TYPE(Value, Type), error_str(<<"Value '~ts' is not valid. Type of value is not '~p'">>, [Value, Type])).
%%-define(V_ERR_LESS_MIN(Parameter, Min), error_str(<< "Value '~ts' is not valid. Values ~p is less than minimum allowed: ~p">>, [Parameter, Min]) ).
%%-define(V_ERR_MORE_MAX(Parameter, Max), error_str(<< "Value '~ts' not valid. Values ~p is more than maximum allowed: ~p">>, [Parameter, Max]) ).
%%-define(V_ERR_VALUE_NOT_ALLOWED(Value, AllowedValues), error_str(<<"Value '~p' is not valid. Value is not in allowed list ~p">>, [Value, AllowedValues])).
%%-define(V_ERR_VALUE_NOT_VALID_REGEXP(Value, Regexp), error_str(<<"Value '~ts' is not valid. Validation with regexp '~ts' failed">>, [Value, RegExp]) ).

%% API
-export([
  validate/2,
  validate/4,
  validate_password/1,
  error_str/2
]).

-spec validate(term(), term()) -> true | {false, Mess :: binary()}.
validate(Conds, Value) ->
  validate(Conds, Value, [], []).

validate([], _Value, _Data, _Opts) -> true;
validate([H | T], Value, Data, Opts) ->
  case validate(H, Value, Data, Opts) of
    true ->
      validate(T, Value, Data, Opts);
    false ->
      {false, ?V_ERR_DEFAULT};
    {false, ValidationMess} ->
      {false, ValidationMess};
    {error, Mess} ->
      {error, Mess}
  end;
validate({'or', ListOfConds}, Value, Data, Opts) when is_list(ListOfConds) ->
  'or'(ListOfConds, Value, Data, Opts);
validate({type, Predefined}, Value, Data, Opts) ->
  'type'(Predefined, Value, Data, Opts);
validate({size, {From, To}}, Value, _Data, _Opts) when (is_integer(From) orelse From == infinity), (is_integer(To) orelse (To == infinity)) ->
  validate_size(From, To, Value);
validate({regexp, Regexp}, Value, _Data, _Opts) when is_binary(Regexp) ->
  validate_with_regexp(Regexp, Value);
validate({Allowed, AllowedValues}, Value, _Data, _Opts) when Allowed == allowed; Allowed == allowed_values ->
  case lists:member(Value, AllowedValues) of
    true -> true;
    false ->
      {false, ?V_ERR_VALUE_NOT_ALLOWED(Value, AllowedValues)}
  end;
validate({is_equal_to_object_of_other_keys, Keys}, Value, Data, _Opts) ->
  case is_equal_to_object_of_other_keys(Value, {Keys, Data}) of
    true -> true;
    false ->
      {false, ?V_ERR_DEFAULT}
  end;

validate(Fun, Value, _Data, _Opts) when is_function(Fun, 1) ->
  case catch Fun(Value) of
    Res when is_boolean(Res) -> Res;
    {false, Message} when is_binary(Message) ->
      {false, Message};
    {error, Message} ->
      {error, Message};
    _ -> %% TODO log
      {error, ?ERR_WRONG_FUN}
  end;

validate(Fun, Value, Data, _Opts) when is_function(Fun, 2) ->
  case catch Fun(Value, Data) of
    Res when is_boolean(Res) -> Res;
    {false, Message} when is_binary(Message) ->
      {false, Message};
    {error, Message} ->
      {error, Message};
    _ ->  %% TODO log
      {error, ?ERR_WRONG_FUN}
  end;

validate(_Unknown, _Value, _Data, _Opts) ->
  {error, ?ERR_UNK_VALIDATOR(_Unknown)}.


type([], _Value, _Data, _Opts) -> true;
type([Type | T], Value, Data, Opts) ->
  case type(Type, Value, Data, Opts) of
    true ->
      type(T, Value, Data, Opts);
    {false, Mess} ->
      {false, Mess}
  end;
type(Cond, Value, _Data, _Opts) when is_atom(Cond) ->
  case validate_type(Cond, Value) of
    true -> true;
    false ->
      {false, ?V_ERR_WRONG_TYPE(Value, Cond)};
    {error, Error} ->
      {error, Error}
  end;
type(Unknown, _Value, _Data, _Opts) ->
  {error, ?ERR_UNK_VALIDATOR(Unknown)}.

'or'([], _Value, _Data, _Opts) -> false;
'or'([H | T], Value, Data, Opts) ->
  case validate(H, Value, Data, Opts) of
    true -> true;
    {error, Message} ->
      {error, Message};
    {false, _ValidationMess} ->
      'or'(T, Value, Data, Opts)
  end.

%% %%%%%%%%%%%%%%
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
  {error, ?ERR_UNK_TYPE_VALIDATOR(Type)}.

%%----------------------COMPLEX VALIDATIONS-----------------------------------------------------------------------------
is_list_of_equal_objects(List) when length(List) =< 1 -> false;
is_list_of_equal_objects(List) ->
  case List of
    [Obj0 | Tail] when is_list(Obj0), length(Obj0) > 0 ->
      BasicLength = length(Obj0),
      BasicKeys = [K || {K, _} <- Obj0],
      lists:all(fun(Obj) when is_list(Obj), length(Obj) =:= BasicLength ->
        lists:all(fun(Key) -> lists:keymember(Key, 1, Obj) end, BasicKeys);
        (_) -> false end, Tail);
    _ -> false
  end.

is_unique_proplist(List) when length(List) =< 1 -> true;
is_unique_proplist([]) -> true;
is_unique_proplist([{K, _V} | T]) ->
  case lists:keymember(K, 1, T) of
    false -> is_unique_proplist(T);
    true ->
      false
  end;
is_unique_proplist([H | T]) ->
  case lists:member(H, T) of
    false -> is_unique_proplist(T);
    true ->
      false
  end.

is_equal_to_object_of_other_keys(List, {Keys, Data}) when is_list(List), is_list(Keys) ->
  lists:all(fun(Key) ->
    is_equal_to_object_of_other_keys(List, {Key, Data})
            end, Keys);
is_equal_to_object_of_other_keys(List, {Key, Data}) when is_list(List) ->
  AnotherList = eutils:get_value(Key, Data),
  is_list_of_equal_objects([List, AnotherList]).

%%%%-----------------SIZE VALIDATION------------------------------------------------------------------------------------
validate_size(MinSize, MaxSize, Value) when is_binary(Value) ->
  Size =
    case unicode:characters_to_list(Value) of
      List when is_list(List) -> length(List);
      _ -> size(Value)
    end,
  size_validator(byte_size, MinSize, MaxSize, Size);
validate_size(MinSize, MaxSize, Value) when is_list(Value) ->
  Size = length(Value),
  size_validator(length, MinSize, MaxSize, Size);
validate_size(MinSize, MaxSize, Value) when is_number(Value) ->
  Size = Value,
  size_validator(limit, MinSize, MaxSize, Size).
size_validator(Parameter, MinSize, MaxSize, Size) ->
  case Size of
    Size when MinSize =/= infinity, Size < MinSize ->
      {false, ?V_ERR_LESS_MIN(Parameter, MinSize)};
    Size when MaxSize =/= infinity, Size > MaxSize ->
      {false, ?V_ERR_MORE_MAX(Parameter, MaxSize)};
    Size -> true
  end.

%%%%-----------------REGEXP VALIDATION----------------------------------------------------------------------------------
validate_with_regexp(RegExp, Value) when is_binary(Value), is_binary(RegExp) ->
  case re:run(Value, RegExp, [{capture, none}]) of
    match -> true;
    _ ->
      {false, ?V_ERR_VALUE_NOT_VALID_REGEXP(Value, RegExp)}
  end;
validate_with_regexp(_, _) ->
  {error, ?ERR_BAD_REGEXP}.

%% UTILS________________________________________________________________________________________________________________
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
  check(Password, "[a-z]").

check_uppercase(Password) ->
  check(Password, "[A-Z]").

check_alphanumeric(Password) ->
  check(Password, "\\d").

check_special(Password) ->
  check(Password, "[\\\\[!\"#$%&\'()*+,-./:;<=>?@^_`{|}~\\]]").

check(Value, Regexp) ->
  case re:run(eutils:to_str(Value), eutils:to_str(Regexp), [{capture, none}]) of
    match -> 1;
    _ -> 0
  end.

error_str(Message, Params) ->
  BinParams = lists:map(
    fun(X) when is_number(X) -> eutils:to_bin(X);
      (X) when is_binary(X) -> X;
      (X) -> unicode:characters_to_binary(io_lib:format("~p", [X]))
    end, Params),
  unicode:characters_to_binary(io_lib:format(Message, BinParams)).