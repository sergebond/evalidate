-module(evalidate_SUITE).
-author("srg").

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("evalidate.hrl").
-include("evalidate_lib.hrl").

all() ->
  [
    {group, errors},
    {group, validators},
    {group, converters},
    {group, presence},
    {group, branching},
    {group, nesting},
    {group, data_struct},
    {group, multiple_keys},
    {group, top_level_rules},
    {group, misc},
    {group, evalidate_lib},
    {group, rule_or_and_on_error}
  ].

groups() ->
  [
    {errors,
      [sequence],
      [
        test_validate_error1,
        test_validate_error2,
        test_validate_error3,
        test_validate_error4
      ]},
    {validators,
      [sequence],
      [
        test_type_validators,
        test_type_validators_bad,
        test_size_bad,
        test_size,
        test_regexp,
        test_custom,
        test_alowed,
        test_not_alowed,
        test_several,
        test_validate_or1,
        test_validate_or_error,
        test_validate_is_equal_to_object_of_other_keys,
        test_validate_is_equal_to_object_of_other_keys_bad
      ]},
    {converters,
      [sequence],
      [
        test_converters,
        test_converter_error,
        test_converter_error1
      ]},
    {presence,
      [sequence],
      [
        test_required_optional_default,
        test_required_bad,
        test_deprecated
      ]},
    {branching, [sequence],
      [
        test_group,
        test_or,
        test_or_error
      ]},

    {nesting,
      [sequence],
      [
        test_nesting,
        test_complex_nesting,
        test_complex_nesting_bad,
        test_complex_nesting_with_parent_converter
      ]},

    {data_struct,
      [sequence],
      [
        test_data_struct,
        test_data_struct0,
        test_data_struct1
      ]},
    {multiple_keys,
      [sequence],
      [
        test_multiple_keys
      ]},
    {top_level_rules,
      [sequence],
      [
        test_top_level_rules,
        test_top_level_rules2
      ]},
    {misc,
      [sequence],
      [
        uniq_list_test,
        custom_validation_error_message
      ]},
    {evalidate_lib,
      [sequence],
      [
        v_binary_integer,
        v_url,
        v_binary_numeric
      ]},
    {rule_or_and_on_error, [sequence],
      [
        rule_or_on_error,
        rule_and_on_error
      ]}
  ].

init_per_suite(Config) ->
  Config.

%%----------------------------------------------------------------------------------------------------------------------
%%                  ERRORS
%%----------------------------------------------------------------------------------------------------------------------
test_validate_error1(Config) ->
  Value = <<"123456789">>,
  Rules = [#rule{
    key = <<"Key">>,
    validators = [{type, in2teger}]
  }],
  Res = (catch evalidate:validate_and_convert(Rules, [{<<"Key">>, Value}, {<<"Key1">>, Value}])),
  case Res of
    {error,<<"Unknown type validator '<<\"in2teger\">>' ">>} ->
      ct:pal("Result ~p, Test test_validate_error1 is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_validate_error1 is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_validate_error2(Config) ->
  Value = <<"123456789">>,
  Rules = [#rule{
    key = <<"Key">>,
    validators = [binary]
  }],
  Res = (catch evalidate:validate_and_convert(Rules, [{<<"Key">>, Value}])),
  case Res of
    {error,<<"Unknown validator '<<\"binary\">>'">>} ->
      ct:pal("Result ~p, Test test_validate_error2 is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_validate_error2 is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_validate_error3(Config) ->
  Value = <<"123456789">>,
  Rules = {#rule{
    key = <<"Key">>,
    validators = [binary]
  }},
  Data = [{<<"Key">>, Value}],

  Expected = {error,<<"Unknown validation rule: '<<\"{{rule,<<\\\"Key\\\">>,required,[binary],none,none}}\">>'">>},

  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_validate_error3 is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_validate_error3 is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_validate_error4(Config) ->
  Rules = [#rule{
    key = <<"Key">>,
    validators = [binary]
  }],
  Data = not_list_data,
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    {error,<<"Mallformed validation data">>} ->
      ct:pal("Result ~p, Test test_validate_error4 is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_validate_error4 is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

%%----------------------------------------------------------------------------------------------------------------------
%%                  VALIDATORS
%%----------------------------------------------------------------------------------------------------------------------
test_type_validators(Config) ->
  Rules = [
    #rule{ key = <<"Key">>, validators = [{type, binary}]},
    #rule{ key = <<"list">>, validators = [{type, list}]},
    #rule{ key = <<"tuple">>, validators = [{type, tuple}]},
    #rule{ key = <<"boolean">>, validators = [{type, boolean}]},
    #rule{ key = <<"integer">>, validators = [{type, integer}]},
    #rule{ key = <<"obj_list">>, validators = [{type, list_of_equal_objects}]},
    #rule{ key = <<"unique_list">>, validators = [{type, uniq_list}]},
    #rule{ key = <<"unique_proplist">>, validators = [{type, uniq_list}]}
  ],
  Data = [
    {<<"Key">>, <<"12566554">>},
    {<<"list">>, [1, 2, 3, 4]},
    {<<"tuple">>, {1, 2, 3, 4}},
    {<<"boolean">>, true},
    {<<"integer">>, 1},
    {<<"obj_list">>, [
      [{<<"k1">>, 1}, {<<"k2">>, 2}, {<<"k3">>, 3}],
      [{<<"k2">>, 4}, {<<"k1">>, 4}, {<<"k3">>, 4}],
      [{<<"k3">>, 6}, {<<"k2">>, 2}, {<<"k1">>, 6}]
    ]},
    {<<"unique_list">>, [1,4,7]},
    {<<"unique_proplist">>, [{1, 2}, {2, 3}, {4, 4}]}
  ],
  Expected = Data,
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_type_validators is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_type_validators is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_type_validators_bad(Config) ->
  Rules = [
    #rule_or{ list = [
      #rule{ key = <<"Key">>, validators = [{type, binary}]},
      #rule{ key = <<"list">>, validators = [{type, list}]},
      #rule{ key = <<"tuple">>, validators = [{type, tuple}]},
      #rule{ key = <<"boolean">>, validators = [{type, boolean}]},
      #rule{ key = <<"integer">>, validators = [{type, integer}]},
      #rule{ key = <<"obj_list">>, validators = [{type, list_of_equal_objects}]},
      #rule{ key = <<"unique_list">>, validators = [{type, uniq_list}]},
      #rule{ key = <<"unique_proplist">>, validators = [{type, uniq_list}]}
    ]
    }],
  Data = [
    {<<"Key">>, atom},
    {<<"list">>, {1, 2, 3, not_list}},
    {<<"tuple">>, [not_tuple, 2, 3, 4]},
    {<<"boolean">>, not_boolean},
    {<<"integer">>, <<"not_integer">>},
    {<<"obj_list">>, [
      [{<<"k1">>, 1}, {<<"k2">>, 2}, {<<"k3">>, 3}],
      [{<<"Not_equal_oblject">>, 4}, {<<"k1">>, 4}, {<<"k3">>, 4}],
      [another_not_equal_object]
    ]},
    {<<"unique_list">>, [1,4,7, the_same_key, the_same_key]},
    {<<"unique_proplist">>, [{the_same_key, 2}, {2, 3}, {the_same_key, 4}]}
  ],
  Expected =
    {error,<<"Key 'the_same_key' is not unique in list or key 'the_same_key' is not unique in list or Value '[[{<<\"k1\">>,1},{<<\"k2\">>,2},{<<\"k3\">>,3}],\n [{<<\"Not_equal_oblject\">>,4},{<<\"k1\">>,4},{<<\"k3\">>,4}],\n [another_not_equal_object]]' is not valid for key 'obj_list' or Value 'not_integer' is not valid for key 'integer' or Value 'not_boolean' is not valid for key 'boolean' or Value '[not_tuple,2,3,4]' is not valid for key 'tuple' or Value '{1,2,3,not_list}' is not valid for key 'list' or Value 'atom' is not valid for key 'Key'">>}
  ,Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_type_validators_bad is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_type_validators_bad is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_size_bad(Config) ->
  Value = <<"123456789">>,
  Rules = [#rule{
    key = <<"Key">>,
    validators = [{type, binary}, {size, {10, 100}}]
  }],
  Data = [{<<"Key">>, Value}],
  Expected = {error,<<"Less than minimum allowed byte_size 10">>},

  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_size_bad is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_size_bad is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_size(Config) ->
  Value = <<"123456789">>,
  Rules = [
    #rule{ key = <<"Key">>, validators = [{type, binary}, {size, {9, 9}}]},
    #rule{ key = <<"Key1">>, validators = [{type, list}, {size, {2, 2}}]},
    #rule{ key = <<"Key2">>, validators = [{type, integer}, {size, {-10, 0}}]},
    #rule{ key = <<"Key3">>, validators = [{type, float}, {size, {-1, 9}}]},
    #rule{ key = <<"Key4">>, validators = [{type, float}, {size, {infinity, 9}}]},
    #rule{ key = <<"Key5">>, validators = [{type, binary}, {size, {1, infinity}}]},
    #rule{ key = <<"Key6">>, validators = [{type, integer}, {size, {infinity, 9}}]}
  ],
  Data = [
    {<<"Key">>, Value},
    {<<"Key1">>, [1,2]},
    {<<"Key2">>, -10},
    {<<"Key3">>, -0.4},
    {<<"Key4">>, -888888888888888888888888.4},
    {<<"Key5">>, <<"eruuhvpegru;ghew[ijbpewjbpewjbpejbpiejrbp[jerpbje[erjwrjoppppeprojoooooooooooooooooooooooooorrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkrrrrrrrrrrrrrrr">>},
    {<<"Key6">>, -1111222222222222222222222222222222222222222}
  ],
  Expected = Data,
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_size is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_size is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_regexp(Config) ->
  Rules = [#rule{
    key = <<"Ip">>,
    validators = [{regexp, <<"(\\d{1,3}\\.){3}\\d{1,3}">>}]
  }],
  Data = [{<<"Ip">>, <<"192.168.1.241">>}],
  Expected = Data,
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_regexp is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_regexp is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_custom(Config) ->
  CustomValidator = fun(V) ->
    V =:= <<"123456800">> orelse throw({error, <<"What a fuck are you doing!?">> } ) end,
  Rules = [#rule{
    key = <<"Ip">>,
    validators = [CustomValidator]
  }],
  Data = [{<<"Ip">>, <<"123456800">>}],
  Expected = Data,
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_custom is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_custom is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_custom_bad1(Config) ->
  CustomValidator =
    fun(V) ->
      case V =:= <<"123456800">> of
        false -> {error, <<"What a fuck are you doing!?">> };
        true -> true
      end
    end,
  Rules = [#rule{
    key = <<"Ip">>,
    validators = [CustomValidator]
  }],
  Data = [{<<"Ip">>, <<"not valid">>}],
  try evalidate:validate_and_convert(Rules, Data) of
    Bad ->
      ct:pal("Result ~p, Test test_custom_bad1 is FAILED!!!!!!", [Bad]),
      {fail, Config}
  catch
    {error, <<"What a fuck are you doing!?">>} = Good ->
      ct:pal("Result ~p, Test test_custom_bad1 is OK", [Good]),
      Config;
    T:R ->
      ct:pal("Test test_custom_bad1 is FAILED!!!!!! Error ~p  Reason ~p", [T, R]),
      {fail, Config}
  end.

test_custom_bad2(Config) ->
  CustomValidator = fun(V) ->
    V =:= <<"123456800">> orelse throw({error, <<"What a fuck are you doing!?">> } ) end,
  Rules = [#rule{
    key = <<"Ip">>,
    validators = [CustomValidator]
  }],
  Data = [{<<"Ip">>, <<"1234568">>}],
  Expected = {error, <<"What a fuck are you doing!?">>},
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_custom_bad2 is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_custom_bad2 is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_alowed(Config) ->
  Rules = [#rule{
    key = <<"Ip">>,
    validators = [{allowed_values, [<<"1">>, <<"2">>, 3, 4]}]
  }],
  Data = [{<<"Ip">>, <<"2">>}],
  Expected = Data,
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_alowed is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_aloweds is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_not_alowed(Config) ->
  Rules = [#rule{
    key = <<"Ip">>,
    validators = [{allowed_values, [<<"1">>, <<"2">>, 3, 4]}]
  }],
  Data = [{<<"Ip">>, <<"123456800">>}],
  Expected = {error,<<"Value '123456800' is not allowed for key 'Ip'">>},
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_not_alowed is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_not_alowed  is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_several(Config) ->
  Rules = [#rule{
    key = <<"Ip">>,
    validators = [
      {allowed_values, [<<"192.168.1.241">>, <<"2">>, 3, 4]},
      {regexp, <<"(\\d{1,3}\\.){3}\\d{1,3}">>},
      {type, binary},
      {size, {13, 25}}
    ]
  }],
  Data = [{<<"Ip">>, <<"192.168.1.241">>}],
  Expected = Data,
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_several is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_several is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_validate_or1(Config) ->
  Rules = [
    #rule{ key = <<"key">>, validators = [
      {'or', [
        {allowed_values, [{[]}]},
        [{type, binary}, {size, {5, 12}}]
%%        [{type, binary}, {size, {5, 12}}]
      ]}]},
    #rule{
      key = <<"key1">>,
      validators = [
        {'or', [
          {allowed_values, [ null, <<"null">>, undefined, <<"undefined">>]},
          [{type, binary}, {size, {5, 12}}],
          [{type, binary}, {size, {5, 10}}]
        ]}]
    }
  ],
  Data = [
    {<<"key">>, {[]}},
    {<<"key1">>, null}
  ],
  Expected = Data,
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_validate_or1 is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_validate_or1 is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_validate_or_error(Config) ->
  Rules = [
    #rule{ key = <<"key">>, validators = [
      {'or', [
        [{allowed_values, [null, <<"null">>, undefined, <<"undefined">>]}],
        [{type, binary}, {size, {5, 12}}],
        [{type, binary}, {size, {5, 13}}]
      ]}]},
    #rule{
      key = <<"key1">>,
      validators = [
        {'or', [
          {allowed_values, [ <<"null">>, undefined, <<"undefined">>]},
          [{type, binary}, {size, {5, 12}}],
          [{type, binary}, {size, {5, 10}}]
        ]}]
    }
  ],
  Data = [
    {<<"key">>, <<"192.168.1.241">>},
    {<<"key1">>, null}
  ],
  Expected = {error,<<"Value 'null' is not valid for key 'key1' or Value 'null' is not allowed for key 'key1'">>},
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_validate_or_error is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_validate_or_error is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_validate_is_equal_to_object_of_other_keys(Config) ->
  Rules = [#rule{
    key = <<"extra">>,
    validators = [{is_equal_to_object_of_other_keys, <<"data">>}]},
    #rule{key = <<"extra_type">>},
    #rule{key = <<"data">>},
    #rule{key = <<"data_type">>}
  ],
  Data = [
    {<<"extra">>,
      [{<<"type1">>, <<"create">>}, {<<"type3">>, <<"delete">>}] },
    {<<"extra_type">>,
      [{<<"type1">>, <<"modify">>}, {<<"type2">>, <<"destroy">>}]},
    {<<"data">>,
      [{<<"type1">>, <<"modify">>}, {<<"type3">>, <<"destroy">>}]},
    {<<"data_type">>,
      [{<<"type1">>, <<"create">>}, {<<"type2">>, <<"delete">>}]}
  ],
  Expected = Data,
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_validate_is_equal_to_object_of_other_keys is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_validate_is_equal_to_object_of_other_keys is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_validate_is_equal_to_object_of_other_keys_bad(Config) ->
  Rules = [#rule{
    key = <<"extra">>,
    validators = [{is_equal_to_object_of_other_keys, <<"data">>}]},
    #rule{key = <<"extra_type">>},
    #rule{key = <<"data">>},
    #rule{key = <<"data_type">>}
  ],
  Data = [
    {<<"extra">>,
      [{<<"type1">>, <<"create">>}, {<<"type2">>, <<"delete">>}] },
    {<<"extra_type">>,
      [{<<"type1">>, <<"modify">>}, {<<"type2">>, <<"destroy">>}]},
    {<<"data">>,
      [{<<"type1">>, <<"modify">>}, {<<"type3">>, <<"destroy">>}]},
    {<<"data_type">>,
      [{<<"type1">>, <<"create">>}, {<<"type2">>, <<"delete">>}]}
  ],
  Expected = {error,<<"Value '[{<<\"type1\">>,<<\"create\">>},{<<\"type2\">>,<<\"delete\">>}]' is not valid for key 'extra'">>},
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_validate_is_equal_to_object_of_other_keys_bad is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_validate_is_equal_to_object_of_other_keys_bad is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

%%----------------------------------------------------------------------------------------------------------------------
%%                  CONVERTERS
%%----------------------------------------------------------------------------------------------------------------------
test_converters(Config) ->
  CustomConverter =
    fun(V) when is_binary(V) -> {ok, Res} = inet_parse:address(binary_to_list(V)), Res;
      (V) when is_list(V) -> {ok, Res} = inet_parse:address(V), Res end,
  Rules = [
    #rule{ key = <<"Key1">>, converter = to_int},
    #rule{ key = <<"Key2">>, converter = to_list},
    #rule{ key = <<"Key3">>, converter = to_atom},
    #rule{ key = <<"Key4">>, converter = to_float},
    #rule{ key = <<"Key5">>, converter = CustomConverter},
    #rule{ key = <<"Key6">>, converter = CustomConverter},
    #rule{ key = <<"Key7">>, converter = filter_duplicates},
    #rule{ key = <<"Key8">>, converter = filter_duplicates},
    #rule{ key = <<"Key9">>, converter = to_boolean},
    #rule{ key = <<"Key10">>, converter = no_return }

  ],
  Data = [
    {<<"Key1">>, <<"124545">>},
    {<<"Key2">>, <<"192.168.1.241">>},
    {<<"Key3">>, <<"192.168.1.241">>},
    {<<"Key4">>, <<"1.241">>},
    {<<"Key5">>, <<"192.168.1.241">>},
    {<<"Key6">>, "192.168.1.241"},
    {<<"Key7">>, [1,1,1,1,2,2,2]},
    {<<"Key8">>, [{q, 1}, {w, 2}, {q, 3}, {w, 3}, {w, 4}]},
    {<<"Key9">>, <<"false">>},
    {<<"Key10">>, <<"Nomatterkey">>}
  ],
  Expected = [
    {<<"Key1">>, 124545},
    {<<"Key2">>, "192.168.1.241"},
    {<<"Key3">>, '192.168.1.241'},
    {<<"Key4">>, 1.241},
    {<<"Key5">>, {192,168,1,241}},
    {<<"Key6">>, {192,168,1,241}},
    {<<"Key7">>, [1,2]},
    {<<"Key8">>, [{q, 1}, {w, 2}]},
    {<<"Key9">>, false}
  ],
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_converters is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_converters is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_converter_error(Config) ->
  CustomConverter =
    fun(V) when is_binary(V) -> {ok, Res} = inet_parse:address(binary_to_list(V)), Res;
      (V) when is_list(V) -> {ok, Res} = inet_parse:address(V), Res end,
  Rules = [#rule{ key = <<"Key6">>, converter = CustomConverter}],
  Data = [{<<"Key6">>, "192.168.1,241"}],
  Expected = {error,<<"Couldn't convert value '\"192.168.1,241\"' for key 'Key6' ">>},
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_converter_error is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_converter_error is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_converter_error1(Config) ->
  CustomConverter =
    fun(V) when is_binary(V) -> {ok, Res} = inet_parse:address(binary_to_list(V)), Res;
      (V) when is_list(V) -> {ok, Res} = inet_parse:address(V), Res;
      (_) -> {error, <<"Shit happens!!!">>}
    end,

  Rules = [#rule{ key = <<"Key6">>, converter = CustomConverter}],

  Data = [{<<"Key6">>, '192.168.1,241'}],

  try evalidate:validate_and_convert(Rules, Data) of
    Bad ->
      ct:pal("Result ~p, Test test_converter_error1 is FAILED!!!!!!", [Bad]),
      {fail, Config}
  catch
    {error, <<"Shit happens!!!">>} = Good ->
      ct:pal("Result ~p, test_converter_error1 is OK", [Good]),
      Config;
    T:R ->
      ct:pal("Result ~p, Test test_converter_error1 is FAILED!!!!!! Error ~p  Reason ~p", [T, R]),
      {fail, Config}
  end.

%%----------------------------------------------------------------------------------------------------------------------
%%                  PRESENCE
%%----------------------------------------------------------------------------------------------------------------------
test_required_optional_default(Config) ->
  Rules = [
    #rule{ key = <<"Key1">>, presence = required },
    #rule{ key = <<"Key2">>, presence = optional },
    #rule{ key = <<"Key3">>, presence = optional },
    #rule{ key = <<"Key4">>, presence = { optional, <<"Default value">>} },
    #rule{ key = <<"Key5">>, presence = { optional, <<"Default value">>} }
  ],
%%
%%  ct:pal("~p ", [Rules]),
  Data = [
    {<<"Key1">>, <<"124545">>},
%%    {<<"Key2">>, <<"192.168.1.241">>},
    {<<"Key3">>, <<"192.168.1.241">>},
%%    {<<"Key4">>, <<"1.241">>},
    {<<"Key5">>, <<"192.168.1.241">>}
  ],

  Expected = [
    {<<"Key1">>, <<"124545">>},
%%    {<<"Key2">>, <<"192.168.1.241">>},
    {<<"Key3">>, <<"192.168.1.241">>},
    {<<"Key4">>, <<"Default value">>},
    {<<"Key5">>, <<"192.168.1.241">>}
  ],
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_required_optional_default is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_required_optional_default is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_required_bad(Config) ->
  Rules = [
    #rule{key = <<"Ip1">>, presence = required }
  ],
  Data = [{<<"Ip">>, <<"192.168.1.241">>}],
  Expected = {error,<<"Key 'Ip1' is required">>},
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_required_bad is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_required_bad is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_deprecated(Config) ->
  Rules = [
    #rule{key = <<"Ip1">>, presence = deprecated }
  ],
  Data = [{<<"Ip1">>, <<"192.168.1.241">>}],
  Expected = {error,<<"Key 'Ip1' is deprecated">>},
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_deprecated is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_deprecated is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

%%----------------------------------------------------------------------------------------------------------------------
%%                  BRANCHINNG
%%----------------------------------------------------------------------------------------------------------------------
test_group(Config) ->
  Rules = [
    #rule_and{list = [
      #rule{key = <<"Ip2">>},
      #rule{key = <<"Ip3">>}
    ]},
    #rule_and{list = [
      #rule{key = <<"Ip4">>},
      #rule{key = <<"Ip5">>}]}
  ],
  Data = [
    {<<"Ip1">>, <<"192.168.1.241">>},
    {<<"Ip2">>, <<"192.168.1.241">>},
    {<<"Ip3">>, <<"192.168.1.241">>},
    {<<"Ip4">>, <<"192.168.1.241">>},
    {<<"Ip5">>, <<"192.168.1.241">>}
  ],
  Expected0 = [
%%      [
%%      {<<"Ip1">>, <<"192.168.1.241">>}
    {<<"Ip2">>, <<"192.168.1.241">>},
    {<<"Ip3">>, <<"192.168.1.241">>},
%%      {<<"Ip4">>, <<"192.168.1.241">>},
%%      {<<"Ip5">>, <<"192.168.1.241">>}
%%      ],
%%      [
%%      {<<"Ip1">>, <<"192.168.1.241">>}
%%        {<<"Ip2">>, <<"192.168.1.241">>},
%%        {<<"Ip3">>, <<"192.168.1.241">>}
    {<<"Ip4">>, <<"192.168.1.241">>},
    {<<"Ip5">>, <<"192.168.1.241">>}
%%      ]
  ],
  Expected = Expected0,

  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_group is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_group is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_or(Config) ->
  List = [
    #rule{key = <<"Ip1">>},
    #rule_and{list = [
      #rule{key = <<"Ip2">>},
      #rule{key = <<"Ip3">>}
    ]},
    #rule_and{list = [
      #rule{key = <<"Ip6">>},
      #rule{key = <<"Ip7">>}
    ]}
  ],
  Rules = [
    #rule_or{list = List}
  ],

  Data = [
%%    {<<"Ip1">>, <<"192.168.1.241">>},
    {<<"Ip2">>, <<"192.168.1.241">>},
    {<<"Ip3">>, <<"192.168.1.241">>}
%%    {<<"Ip6">>, <<"192.168.1.241">>},
%%    {<<"Ip7">>, <<"192.168.1.241">>}
  ],
  Expected0 =
    [
%%      {<<"Ip1">>, <<"192.168.1.241">>}
      {<<"Ip2">>, <<"192.168.1.241">>},
      {<<"Ip3">>, <<"192.168.1.241">>}
%%      {<<"Ip4">>, <<"192.168.1.241">>},
%%      {<<"Ip5">>, <<"192.168.1.241">>}
    ],
  Expected = Expected0,
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_or is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_or is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_or_error(Config) ->
  List = [
    #rule{key = <<"Ip1">>},
    #rule_and{list = [
      #rule{key = <<"Ip2">>},
      #rule{key = <<"Ip4">>}
    ]},
    #rule_and{list = [
      #rule{key = <<"Ip6">>},
      #rule{key = <<"Ip7">>}
    ]}
  ],
  Rules = [
    #rule_or{list = List}
  ],

  Data = [
%%    {<<"Ip1">>, <<"192.168.1.241">>},
    {<<"Ip2">>, <<"192.168.1.241">>},
    {<<"Ip3">>, <<"192.168.1.241">>}
%%    {<<"Ip6">>, <<"192.168.1.241">>},
%%    {<<"Ip7">>, <<"192.168.1.241">>}
  ],
  Expected = {error,<<"Key 'Ip6' is required or Key 'Ip4' is required or Key 'Ip1' is required">>},
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_or_error is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_or_error is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

%%----------------------------------------------------------------------------------------------------------------------
%%                  NESTING
%%----------------------------------------------------------------------------------------------------------------------
test_nesting(Config) ->
  NestedRules = [
    #rule{key = <<"NestedIp1">>}
  ],
  Rules = [
    #rule{key = <<"Ip1">>, childs = NestedRules }
  ],
  NestedData = [{<<"NestedIp1">>, <<"192.168.1.241">>}],
  Data = [{<<"Ip1">>, NestedData}],

  Expected = Data,

  Res = (catch evalidate:validate_and_convert(Rules, Data)),

  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_nesting is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_nesting is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_complex_nesting(Config) ->
  NestedLev2 = [
    #rule{key = <<"NestedIp2">>}
  ],
  NestedLev1 = [
    #rule{key = <<"NestedIp1">>, childs = NestedLev2}
  ],
  Rules = [
    #rule{key = <<"Ip1">>, childs = NestedLev1},
    #rule{key = <<"Ip2">>, childs = NestedLev1},
    #rule{key = <<"Ip3">>}

  ],

  NestedData2 = [{<<"NestedIp2">>, <<"192.168.1.241">>}],
  NestedData1 = [{<<"NestedIp1">>, NestedData2}],
  Data = [
    {<<"Ip1">>, NestedData1},
    {<<"Ip2">>, NestedData1},
    {<<"Ip3">>, <<"192.168.1.241">>}
  ],

  Expected = Data,

  Res = (catch evalidate:validate_and_convert(Rules, Data)),

  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_complex_nesting is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_complex_nesting is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_complex_nesting_bad(Config) ->
  NestedLev2 = [
    #rule{key = <<"NestedIp2">>}
  ],
  NestedLev1 = [
    #rule{key = <<"NestedIp1">>, validators = {type, integer}, childs = NestedLev2}
  ],
  Rules = [
    #rule{key = <<"Ip1">>, childs = NestedLev1},
    #rule{key = <<"Ip2">>, childs = NestedLev1},
    #rule{key = <<"Ip3">>}
  ],

  NestedData2 = [{<<"NestedIp2">>, <<"192.168.1.241">>}],
  NestedData1 = [{<<"NestedIp1">>, NestedData2}],
  Data = [
    {<<"Ip1">>, NestedData1},
    {<<"Ip2">>, NestedData1},
    {<<"Ip3">>, <<"192.168.1.241">>}
  ],

  Expected = {error,<<"Value '[{<<\"NestedIp2\">>,<<\"192.168.1.241\">>}]' is not valid for key 'NestedIp1'">>},

  Res = (catch evalidate:validate_and_convert(Rules, Data)),

  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_complex_nesting_bad is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_complex_nesting_bad is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_complex_nesting_with_parent_converter(Config) ->
  CustomConverter =
    fun(X) ->
      list_to_tuple(X)
    end,
  NestedLev2 = [
    #rule{key = <<"NestedIp2">>}
  ],
  NestedLev1 = [
    #rule{key = <<"NestedIp1">>, validators = {type, list}, childs = NestedLev2, converter = CustomConverter }
  ],
  Rules = [
    #rule{key = <<"Ip1">>, childs = NestedLev1},
    #rule{key = <<"Ip2">>, childs = NestedLev1},
    #rule{key = <<"Ip3">>}
  ],

  NestedData2 = [{<<"NestedIp2">>, <<"192.168.1.241">>}],
  NestedData1 = [{<<"NestedIp1">>, NestedData2}],

  Data = [
    {<<"Ip1">>, NestedData1},
    {<<"Ip2">>, NestedData1},
    {<<"Ip3">>, <<"192.168.1.241">>}
  ],

  ExpectedNestedData1 = [{<<"NestedIp1">>, CustomConverter(NestedData2)}],

  ExpectedData = [
    {<<"Ip1">>, ExpectedNestedData1},
    {<<"Ip2">>, ExpectedNestedData1},
    {<<"Ip3">>, <<"192.168.1.241">>}
  ],

  Expected = ExpectedData,

  Res = (catch evalidate:validate_and_convert(Rules, Data)),

  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_complex_nesting_with_parent_converter is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_complex_nesting_with_parent_converter is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.


%%----------------------------------------------------------------------------------------------------------------------
%%                  DATA STRUCT
%%----------------------------------------------------------------------------------------------------------------------
%%
test_data_struct(Config) ->
  Rules0 = [#rule{key = <<"OtherLevel">>}],

  NestedLev2 = [ #rule{key = <<"NestedIp2">>}],
  NestedLev1 = [
    #rule{key = <<"NestedIp1">>, childs = NestedLev2}
  ],

  Rules1 = [
    #rule{key = <<"Ip1">>, childs = NestedLev1},
    #rule{key = <<"Ip2">>, childs = NestedLev1},
    #rule{key = <<"Ip3">>}
  ],

  Rules = [Rules0, Rules1],

  Data0 = [{<<"OtherLevel">>, <<"192.168.1.241">>}],

  NestedData2 = [{<<"NestedIp2">>, <<"192.168.1.241">>}],
  NestedData1 = [{<<"NestedIp1">>, NestedData2}],
  Data1 = [
    {<<"Ip1">>, NestedData1},
    {<<"Ip2">>, NestedData1},
    {<<"Ip3">>, <<"192.168.1.241">>}
  ],

  Data = [Data0, Data1],

  Expected = Data,

%%  ct:pal("Reversed ~p", [Expected]),
  ct:pal("Data ~p", [Data]),

  Res = (catch evalidate:validate_and_convert(Rules, Data)),

  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_data_struct is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_data_struct is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

test_data_struct0(Config) ->
  Rules0 = [#rule{key = <<"OtherLevel">>}],

%%  NestedLev2 = [ #rule{key = <<"NestedIp2">>}],
%%  NestedLev1 = [
%%    #rule{key = <<"NestedIp1">>, childs = NestedLev2}
%%  ],
%%  Rules1 = [
%%    #rule{key = <<"Ip1">>, childs = NestedLev1},
%%    #rule{key = <<"Ip2">>, childs = NestedLev1},
%%    #rule{key = <<"Ip3">>}
%%  ],

  Rules = Rules0,

  Data0 = [{<<"OtherLevel">>, <<"192.168.1.241">>}],

  NestedData2 = [{<<"NestedIp2">>, <<"192.168.1.241">>}],
  NestedData1 = [{<<"NestedIp1">>, NestedData2}],
  Data1 = [
    {<<"Ip1">>, NestedData1},
    {<<"Ip2">>, NestedData1},
    {<<"OtherLevel">>, <<"192.168.1.241">> },
    {<<"Ip3">>, <<"192.168.1.241">>}
  ],

  Data = [Data0, Data0],

%%  Expected = recursive_reverse(Data),
  Expected = Data,
%%  ct:pal("Reversed ~p", [Epected]),
%%  ct:pal("Data ~p", [Data]),

  Res = (catch evalidate:validate_and_convert(Rules, Data)),

  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_data_struct0 is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p,~n Expected ~p Test test_data_struct0 is FAILED!!!!!!", [Res, Expected]),
      {fail, Config}
  end.

test_data_struct1(Config) ->
  Data = [
    {<<"key1">>, <<"test">>},
    {<<"key2">>, [{<<"test">>, 1}]},
    {<<"key3">>, <<"created">>},
    {<<"key4">>, [
      [{<<"type1">>, <<"create">>}, {<<"type2">>, <<"delete">>}],
      [{<<"type1">>, <<"modify">>}, {<<"type2">>, <<"destroy">>}],
      [{<<"type1">>, <<"create">>}, {<<"type2">>, <<"delete">>}],
      [{<<"type1">>, <<"modify">>}, {<<"type2">>, <<"destroy">>}]
    ]}
  ],

  Rules0 = [
    #rule{key = <<"key1">>},
    #rule{key = <<"key2">>, childs = [#rule{key = <<"test">>}], validators = [{type, list}, {size, {1, 3}}]},
    #rule{key = <<"key3">>},
    #rule{key = <<"key4">>,
      childs =
      [#rule{
        key = [<<"type1">>, <<"type2">>],
        validators = [
          {type, binary},
          {allowed_values, [<<"create">>, <<"delete">>, <<"modify">>, <<"destroy">>]}
        ],
        converter = to_atom}
      ]}
  ],

  Expected = [{<<"key1">>,<<"test">>},
    {<<"key2">>,[{<<"test">>,1}]},
    {<<"key3">>,<<"created">>},
    {<<"key4">>,
      [[{<<"type1">>,create},{<<"type2">>,delete}],
        [{<<"type1">>,modify},{<<"type2">>,destroy}],
        [{<<"type1">>,create},{<<"type2">>,delete}],
        [{<<"type1">>,modify},{<<"type2">>,destroy}]]}],

  Rules = Rules0,

  Res = (catch evalidate:validate_and_convert(Rules, Data)),

  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_data_struct1 is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p,~n Expected ~p Test test_data_struct1 is FAILED!!!!!!", [Res, Expected]),
      {fail, Config}
  end.

%%----------------------------------------------------------------------------------------------------------------------
%%                  Multiple keys in rule
%%----------------------------------------------------------------------------------------------------------------------
test_multiple_keys(Config) ->
  Rules = [
    #rule{key = [<<"Key1">>,<<"Key2">>, <<"Key3">>], presence = optional},
    #rule{key = [<<"Key4">>,<<"Key5">>], validators = [{type, integer}]},
    #rule{key = <<"Key6">>, validators = [{type, atom}], presence = {optional, default_atom_value}}
  ],

  Data = [
    {<<"Key1">>, <<"V1">>},
%%    {<<"Key2">>, <<"V2">>},
    {<<"Key3">>, <<"V3">>},
    {<<"Key4">>, 4},
    {<<"Key5">>, 5},
    {<<"Key6">>, atom}
  ],

%%  Expected = recursive_reverse(Data),
  Expected = Data,
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_multiple_keys is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_multiple_keys is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

%%----------------------------------------------------------------------------------------------------------------------
%%                  Top level rules
%%----------------------------------------------------------------------------------------------------------------------
test_top_level_rules(Config) ->
  Rules = [
    #rule{ key = none, validators = [{type, list}, {size, {1, 4}}], converter = filter_duplicates}
  ],
  Data = [
    {<<"Ip1">>, <<"192.168.1.241">>},
%%    {<<"Ip2">>, <<"192.168.1.241">>},
    {<<"Ip3">>, <<"192.168.1.241">>},
    {<<"Ip4">>, <<"192.168.1.241">>}
%%    {<<"Ip4">>, <<"192.168.1.241">>}
%%    {<<"Ip5">>, <<"192.168.1.241">>}
  ],
  Expected = Data,
  Res = (catch evalidate:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_top_level_rules is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_top_level_rules is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.

-record(for_test, {
  currency,
  amount,
  user_id
}).

test_top_level_rules2(Config) ->
  Rules = [
    #rule{ key = <<"currency">>, converter = to_atom},
    #rule{ key = <<"amount">>, converter = to_float},
    #rule{ key = <<"user_id">>, converter = to_int}
  ],
  Converter =
    fun([{<<"currency">>, Currency }, {<<"amount">>, Amount }, {<<"user_id">>, UserId}]) ->
      #for_test{ currency = Currency,
        amount = Amount,
        user_id = UserId }
    end,
  TopRules = [
    #rule{ key = none, validators = [{type, list}, {size, {1, 4}}], childs = Rules, converter = Converter}
  ],
  Data = [
    {<<"currency">>, <<"USD">>},
    {<<"amount">>, <<"12.02">>},
    {<<"user_id">>, <<"12345">>}
  ],
  Expected = [#for_test{currency = 'USD', amount = 12.02, user_id = 12345 }],
  Res = (catch evalidate:validate_and_convert(TopRules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_top_level_rules2 is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_top_level_rules2 is FAILED!!!!!!", [Res]),
      {fail, Config}
  end.


%%----------------------------------------------------------------------------------------------------------------------
%%                  Misc
%%----------------------------------------------------------------------------------------------------------------------
uniq_list_test(Config) ->
  Rules = [
    #rule{ key = <<"type">>, validators = [{type, binary}], presence = optional},
    #rule{ key = [<<"extra">>, <<"extra_type">>, <<"data">>, <<"data_type">>], validators = [{type, uniq_list}], presence = optional}
  ],
  Extra = [{k1, v1}, {k2, v2}, {k3, v3}, {k4, v4}],
  Extra_types = [{k1, v1}, {k2, v2}, {k3, v3}, {k4, v4}],

  Extra_bad = [{k1, v1}, {k2, v2}, {k3, v3}, {k1, v4}],
  Extra_types_bad = [{k1, v1}, {k2, v2}, {k1, v3}, {k4, v4}],

  %% GOOD__________________________
  Data1 = [
    {<<"type">>, <<"some_type">>},
    {<<"extra">>, Extra},
    {<<"extra_type">>, Extra_types}
  ],

  Res1 = (catch evalidate:validate_and_convert(Rules, Data1)),
  ct:pal("Res1 ~p", [Res1]),
  ?assertEqual(Res1, Data1),

  Data2 = [
    {<<"type">>, <<"some_type">>},
    {<<"data">>, Extra},
    {<<"data_type">>, Extra_types}
  ],

  Res2 = (catch evalidate:validate_and_convert(Rules, Data2)),
  ct:pal("Res2 ~p", [Res2]),
  ?assertEqual(Res2, Data2),

  Data3 = [
    {<<"type">>, <<"some_type">>}
  ],

  Res3 = (catch evalidate:validate_and_convert(Rules, Data3)),
  ct:pal("Res3 ~p", [Res3]),
  ?assertEqual(Res3, Data3),


  %% BAD____________________________
  Data10 = [
    {<<"type">>, <<"some_type">>},
    {<<"extra">>, Extra_bad},
    {<<"extra_type">>, Extra_types}
  ],

  Res10 = (catch evalidate:validate_and_convert(Rules, Data10)),
  ct:pal("Res10 ~p", [Res10]),
  ?assertEqual(Res10, {error,<<"Key 'k1' is not unique in list">>}),

  Data11 = [
    {<<"type">>, <<"some_type">>},
    {<<"data">>, Extra},
    {<<"data_type">>, Extra_types_bad}
  ],

  Res11 = (catch evalidate:validate_and_convert(Rules, Data11)),
  ct:pal("Res11 ~p", [Res11]),
  ?assertEqual(Res11, {error,<<"Key 'k1' is not unique in list">>} ),

  %% Other interpretation of rules______________________________________________________________________________________
  Rules1 = [
    #rule{ key = <<"type">>, validators = [{type, binary}], presence = optional},
    #rule_or{list = [
      #rule{ key = [<<"extra">>, <<"extra_type">>], validators = [{type, uniq_list}], presence = required},
      #rule{ key = [<<"data">>, <<"data_type">>], validators = [{type, uniq_list}], presence = required}
    ]}
  ],

  %% GOOD__________________________
  Res21 = (catch evalidate:validate_and_convert(Rules1, Data1)),
  ct:pal("Res21 ~p", [Res21]),
  ?assertEqual(Res21, Data1),


  Res22 = (catch evalidate:validate_and_convert(Rules1, Data2)),
  ct:pal("Res22 ~p", [Res22]),
  ?assertEqual(Res22, Data2),

  Res23 = (catch evalidate:validate_and_convert(Rules1, Data3)),
  ct:pal("Res23 ~p", [Res23]),
  ?assertEqual(Res23,
    {error,<<"Key 'data' is required or Key 'extra' is required">>}),
  %% BAD____________________________
  Res210 = (catch evalidate:validate_and_convert(Rules1, Data10)),
  ct:pal("Res210 ~p", [Res210]),
  ?assertEqual(Res210, {error,<<"Key 'data' is required or Key 'k1' is not unique in list">>}),

  Res211 = (catch evalidate:validate_and_convert(Rules1, Data11)),
  ct:pal("Res211 ~p", [Res211]),
  ?assertEqual(Res211, {error,<<"Key 'k1' is not unique in list or Key 'extra' is required">>} ),
  Config.


custom_validation_error_message(Config) ->
  CustomMessage = {error, <<"Custom validation error message">>},
  RangeValidator = fun(Range) when Range =:= <<"custom">> -> true;
    (Range) ->
      case lists:member(Range, [<<"Last_1_hour">>, <<"Last_6_hours">>,<<"Last_12_hours">>,<<"Last_24_hours">>]) of
        true -> throw(CustomMessage);
        _ -> false
      end end,
  Body1 = [{<<"range">>, <<"Last_12_hours">> }],
  Rules =
    [
      #rule{ key = <<"range">>, validators = [{type, binary}, RangeValidator]}
    ],
  Res1 = evalidate:validate_and_convert(Rules, Body1, [{mode, soft}]),
  ct:pal("Result1 is ~p", [Res1]),
  ?assertEqual(CustomMessage, Res1),

  Body2 = [{<<"range">>, <<"custom">> }],
  Res2 = evalidate:validate_and_convert(Rules, Body2, [{mode, soft}]),
  ct:pal("Result2 is ~p", [Res2]),
  ?assertEqual({ok, Body2}, Res2),
  Config.

v_binary_integer(Config) ->
  Rules1 = [ #rule{ key = <<"binary_integer">>, validators = [?V_BINARY_INTEGER]} ],

  Body = [{<<"binary_integer">>, <<"123456789">>}],
  Res1 = evalidate:validate_and_convert(Rules1, Body, [{mode, soft}]),
  ct:pal("Result1 is ~p", [Res1]),
  ?assertEqual({ok, Body}, Res1),

  Body1 = [{<<"binary_integer">>, <<"zzz123456789">>}],
  Res2 = evalidate:validate_and_convert(Rules1, Body1, [{mode, soft}]),
  ct:pal("Result2 is ~p", [Res2]),
  ?assertEqual({error,<<"Value 'zzz123456789' is not valid for key 'binary_integer'">>}, Res2),

  Body2 = [{<<"binary_integer">>, 123456789}],
  Res3 = evalidate:validate_and_convert(Rules1, Body2, [{mode, soft}]),
  ct:pal("Result3 is ~p", [Res3]),
  ?assertEqual({ok, Body2}, Res3),

  %%%% --------- ?V_BINARY_INTEGER(From, To) with size validating

  Rules2 = [ #rule{ key = <<"binary_integer">>, validators = [?V_BINARY_INTEGER(infinity, 0)]} ],
  Res4 = evalidate:validate_and_convert(Rules2, Body, [{mode, soft}]),
  ct:pal("Result4 is ~p", [Res4]),
  ?assertEqual({error,<<"More than maximum allowed limit 0">>}, Res4),

  Rules3 = [ #rule{ key = <<"binary_integer">>, validators = [?V_BINARY_INTEGER(123456790, infinity)]} ],
  Res5 = evalidate:validate_and_convert(Rules3, Body, [{mode, soft}]),
  ct:pal("Result5 is ~p", [Res5]),
  ?assertEqual({error,<<"Less than minimum allowed limit 123456790">>}, Res5),

  Rules4 = [ #rule{ key = <<"binary_integer">>, validators = [?V_BINARY_INTEGER(123456780, infinity)]} ],
  Res6 = evalidate:validate_and_convert(Rules4, Body, [{mode, soft}]),
  ct:pal("Result6 is ~p", [Res6]),
  ?assertEqual({ok, Body}, Res6),
  Config.

v_url(Config) ->
  Data = [{<<"some_url">>, "https://subdomain.domain/page.com"}],
  Rules = [#rule{key = <<"some_url">>, validators = [?V_URL]}],

  Res = evalidate:validate_and_convert(Rules, Data, [{mode, soft}]),
  ct:pal("Result is ~p", [Res]),
  ?assertEqual({ok, Data}, Res),

  WrongData = [{<<"some_url">>, "htwws://domain/page.com"}],

  Res1 = evalidate:validate_and_convert(Rules, WrongData, [{mode, soft}]),
  ct:pal("Result is ~p", [Res1]),
  ?assertEqual({error,<<"Value '\"htwws://domain/page.com\"' is not valid for key 'some_url'">>}, Res1),
  Config.

v_binary_numeric(Config) ->
  GoodData0 = [{<<"num">>, <<"11">>}],
  GoodData1 = [{<<"num">>, <<"11.03">>}],
  BadData = [{<<"num">>, <<"XVII">>}],

  Rules = [#rule{ key = <<"num">>, validators = [?V_BINARY_NUMERIC]}],

  Res1 = evalidate:validate_and_convert(Rules, GoodData0, [{mode, soft}]),
  ct:pal("Result is ~p", [Res1]),
  ?assertEqual({ok, GoodData0}, Res1),

  Res2 = evalidate:validate_and_convert(Rules, GoodData1, [{mode, soft}]),
  ct:pal("Result2 is ~p", [Res2]),
  ?assertEqual({ok, GoodData1}, Res2),

  Res3 = evalidate:validate_and_convert(Rules, BadData, [{mode, soft}]),
  ct:pal("Result3 is ~p", [Res3]),
  ?assertEqual({error,<<"Value 'XVII' is not valid for key 'num'">>}, Res3),

  
  Rules1 = [#rule{ key = <<"num">>, validators = [?V_BINARY_NUMERIC(infinity, 0)]}],

  Res4 = evalidate:validate_and_convert(Rules1, GoodData1, [{mode, soft}]),
  ct:pal("Result4 is ~p", [Res4]),
  ?assertEqual({error,<<"More than maximum allowed limit 0">>}, Res4),


  Rules2 = [#rule{ key = <<"num">>, validators = [?V_BINARY_NUMERIC( 0, infinity)]}],

  Res5 = evalidate:validate_and_convert(Rules2, GoodData1, [{mode, soft}]),
  ct:pal("Result5 is ~p", [Res5]),
  ?assertEqual({ok, GoodData1}, Res5),

  Config.

rule_or_on_error(Config) ->
  ErrorMess = <<"Some error">>,
  Rules = [
    #rule_or{on_error = ErrorMess, list = [
      #rule{ key = <<"Key">>, validators = [{type, binary}]},
      #rule{ key = <<"list">>, validators = [{type, list}]}
    ]
    }],
  Data = [{<<"Key">>, 1}],

  Expected = {error, ErrorMess},

  Res = (catch evalidate:validate_and_convert(Rules, Data)),

  case Res of
    Expected ->
      ct:pal("Result ~p, Test rule_or_on_error is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test rule_or_on_error is FAILED!!!!!!", [Res]),
      {fail, <<"Fail">>}
  end.

rule_and_on_error(Config) ->
  ErrorMess = <<"Some error">>,
  Rules = [
    #rule_and{on_error = ErrorMess, list = [
      #rule{ key = <<"Key">>, validators = [{type, binary}]},
      #rule{ key = <<"list">>, validators = [{type, list}]}
    ]
    }],
  Data = [{<<"Key">>, 1}],

  Expected = {error, ErrorMess},

  Res = (catch evalidate:validate_and_convert(Rules, Data)),

  case Res of
    Expected ->
      ct:pal("Result ~p, Test rule_and_on_error is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test rule_and_on_error is FAILED!!!!!!", [Res]),
      {fail, <<"Fail">>}
  end.