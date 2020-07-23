-author("sergeybondarchuk").

%% evv.erl______________________________________________________________________________________________________________
-define(ERR_UNK_VALIDATOR(Unknown), evv:error_str(<<"Unknown validator '~ts'">>, [Unknown])).
-define(ERR_UNK_TYPE_VALIDATOR(Unknown), evv:error_str(<<"Unknown type validator '~p'">>, [Unknown])).
-define(ERR_BAD_REGEXP, <<"Bad regexp">>).
-define(ERR_WRONG_FUN, <<"Wrong validation function">>).

-define(V_ERR_DEFAULT, <<"Value is not valid">>).

-define(V_ERR_WRONG_TYPE(Value, Type), evv:error_str(<<"Value '~ts' is not valid. Type of value is not '~s'">>, [?UNSCRIPTIZE(Value), Type])).
-define(V_ERR_LESS_MIN(Parameter, Min), evv:error_str(<<"Value is not valid. Value's ~s is less than minimum allowed: ~s">>, [Parameter, Min])).
-define(V_ERR_MORE_MAX(Parameter, Max), evv:error_str(<<"Value is not valid. Value's ~s is more than maximum allowed: ~s">>, [Parameter, Max])).
-define(V_ERR_VALUE_NOT_ALLOWED(Value, AllowedValues), evv:error_str(<<"Value '~p' is not valid. Value is not in allowed list ~p">>, [?UNSCRIPTIZE(Value), AllowedValues])).
-define(V_ERR_VALUE_NOT_VALID_REGEXP(Value, Regexp), evv:error_str(<<"Value '~ts' is not valid. Validation with regexp '~ts' failed">>, [?UNSCRIPTIZE(Value), RegExp])).

-define(UNSCRIPTIZE(Bin),
  begin case Bin of
  _ when is_binary(Bin) ->
    binary:replace(Bin, [<<"<">>, <<">">>], <<"\\">>, [global, {insert_replaced, 1}]);
  _ -> Bin
  end end).

-define(V_ERR_MESSAGE(Message, Key, Value), [
  {message, Message},
  {key, ?UNSCRIPTIZE(Key)},
  {value, evv:maybe_cut(?UNSCRIPTIZE(Value))}]).

%% evalidate.erl________________________________________________________________________________________________________
-define(ERR_MALFORMED_DATA, <<"Mallformed validation data">>).
-define(ERR_UNK_RULES(NotValid), evv:error_str(<<"Unknown rule(s) '~p'">>, [NotValid])).
-define(ERR_WRONG_PARAMS(For, WrongParams), evv:error_str(<<"Wrong parameters for ~p.~n~p' ">>, [For, WrongParams])).

-define(ERR_KEY_IS_REQUIRED(Key), evv:error_str(<<"Key '~ts' is required">>, [Key])).
-define(ERR_KEY_IS_DEPRECATED(Key), evv:error_str(<<"Key '~ts' is deprecated">>, [Key])).

-define(ERR_WRONG_CHILDS(Key), evv:error_str(<<"Wrong childs for key '~ts'">>, [Key])).

-define(ERR_WRONG_CONVERTER(Key, Value), evv:error_str(<<"Wrong converter for key '~ts' value '~ts'">>, [Key, Value])).
-define(ERR_COULDNT_CONVERT(Value, Key), evv:error_str(<<"Couldn't convert value '~ts' for key '~ts' ">>, [Value, Key])).

-define(ERR_UNSUPPORTED_UNICODE_CONTROL_CODE, <<"Unsupported unicode control code">>).
