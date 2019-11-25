-author("sergeybondarchuk").

%% evv.erl______________________________________________________________________________________________________________
-define(ERR_UNK_VALIDATOR(Unknown), evv:error_str(<<"Unknown validator '~ts'">>, [Unknown])).
-define(ERR_UNK_TYPE_VALIDATOR(Unknown), evv:error_str(<<"Unknown type validator '~p'">>, [Unknown])).
-define(ERR_BAD_REGEXP, <<"Bad regexp">>).
-define(ERR_WRONG_FUN, <<"Wrong validation function">>).

-define(V_ERR_DEFAULT, <<"Value is not valid">>).

-define(V_ERR_WRONG_TYPE(Value, Type), evv:error_str(<<"Value '~ts' is not valid. Type of value is not '~p'">>, [Value, Type])).
-define(V_ERR_LESS_MIN(Parameter, Min), evv:error_str(<<"Value '~ts' is not valid. Values ~p is less than minimum allowed: ~p">>, [Parameter, Min])).
-define(V_ERR_MORE_MAX(Parameter, Max), evv:error_str(<<"Value '~ts' not valid. Values ~p is more than maximum allowed: ~p">>, [Parameter, Max])).
-define(V_ERR_VALUE_NOT_ALLOWED(Value, AllowedValues), evv:error_str(<<"Value '~p' is not valid. Value is not in allowed list ~p">>, [Value, AllowedValues])).
-define(V_ERR_VALUE_NOT_VALID_REGEXP(Value, Regexp), evv:error_str(<<"Value '~ts' is not valid. Validation with regexp '~ts' failed">>, [Value, RegExp])).

%% evalidate.erl________________________________________________________________________________________________________
-define(ERR_MALFORMED_DATA, <<"Mallformed validation data">>).
-define(ERR_UNK_RULES(NotValid), evv:error_str(<<"Unknown rule(s) '~p'">>, [NotValid])).
-define(ERR_WRONG_PARAMS(For, WrongParams), evv:error_str(<<"Wrong parameters for ~p.~n~p' ">>, [For, WrongParams])).

-define(ERR_KEY_IS_REQUIRED(Key), evv:error_str(<<"Key '~ts' is required">>, [Key])).
-define(ERR_KEY_IS_DEPRECATED(Key), evv:error_str(<<"Key '~ts' is deprecated">>, [Key])).

-define(ERR_WRONG_CHILDS(Key), evv:error_str(<<"Wrong childs for key '~ts'">>, [Key])).

-define(ERR_WRONG_CONVERTER(Key, Value), evv:error_str(<<"Wrong converter for key '~ts' value '~ts'">>, [Key, Value]) ).
-define(ERR_COULDNT_CONVERT(Value, Key), evv:error_str(<<"Couldn't convert value '~ts' for key '~ts' ">>, [Value, Key] )).

