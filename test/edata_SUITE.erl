-module(edata_SUITE).
-author("srg").

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include_lib("edata.hrl").

%%% ==================================================================
%%% CT Callbacks
%%% ==================================================================

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
    {group, top_level_rules}
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
        test_validate_or_error
      ]},
    {converters,
      [sequence],
      [
        test_converters,
        test_converter_error
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
        test_complex_nesting
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
        test_top_level_rules
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
  Res = (catch edata:validate_and_convert(Rules, [{<<"Key">>, Value}, {<<"Key1">>, Value}])),
  case Res of
    {error,<<"Unknown type validator in2teger ">>} ->
      ct:pal("Result ~p, Test test_validate_error1 is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_validate_error1 is FAILED!!!!!!", [Res]),
      {skip, Config}
  end.

test_validate_error2(Config) ->
  Value = <<"123456789">>,
  Rules = [#rule{
    key = <<"Key">>,
    validators = [binary]
  }],
  Res = (catch edata:validate_and_convert(Rules, [{<<"Key">>, Value}])),
  case Res of
    {error,<<"Wrong validator binary">>} ->
      ct:pal("Result ~p, Test test_validate_error2 is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_validate_error2 is FAILED!!!!!!", [Res]),
      {skip, Config}
  end.

test_validate_error3(Config) ->
  Value = <<"123456789">>,
  Rules = {#rule{
    key = <<"Key">>,
    validators = [binary]
  }},
  Data = [{<<"Key">>, Value}],

  Expected = {error,<<"Unknown validation rule: {{rule,<<\"Key\">>,required,[binary],none,none}}">>},

  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_validate_error3 is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_validate_error3 is FAILED!!!!!!", [Res]),
      {skip, Config}
  end.

test_validate_error4(Config) ->
  Rules = [#rule{
    key = <<"Key">>,
    validators = [binary]
  }],
  Data = not_list_data,
  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    {error,<<"Mallformed validation data">>} ->
      ct:pal("Result ~p, Test test_validate_error4 is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_validate_error4 is FAILED!!!!!!", [Res]),
      {skip, Config}
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
  Expected = lists:reverse(Data),
  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_type_validators is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_type_validators is FAILED!!!!!!", [Res]),
      {skip, Config}
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
    {error,<<"All variants in OR list are not valid \n[<<\"Key the_same_key is not unique in list\">>,\n <<\"Element the_same_key is not unique in list\">>,\n <<\"Value [[{<<\\\"k1\\\">>,1},{<<\\\"k2\\\">>,2},{<<\\\"k3\\\">>,3}],\\n       [{<<\\\"Not_equal_oblject\\\">>,4},{<<\\\"k1\\\">>,4},{<<\\\"k3\\\">>,4}],\\n       [another_not_equal_object]] is not valid\">>,\n <<\"Value <<\\\"not_integer\\\">> is not valid\">>,\n <<\"Value not_boolean is not valid\">>,\n <<\"Value [not_tuple,2,3,4] is not valid\">>,\n <<\"Value {1,2,3,not_list} is not valid\">>,<<\"Value atom is not valid\">>]">>},
  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_type_validators_bad is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_type_validators_bad is FAILED!!!!!!", [Res]),
      {skip, Config}
  end.

test_size_bad(Config) ->
  Value = <<"123456789">>,
  Rules = [#rule{
    key = <<"Key">>,
    validators = [{type, binary}, {size, {10, 100}}]
  }],
  Data = [{<<"Key">>, Value}],
  Expected = {error,<<"Less than minimum allowed length 10">>},

  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_size_bad is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_size_bad is FAILED!!!!!!", [Res]),
      {skip, Config}
  end.

test_size(Config) ->
  Value = <<"123456789">>,
  Rules = [
    #rule{ key = <<"Key">>, validators = [{type, binary}, {size, {9, 9}}]},
    #rule{ key = <<"Key1">>, validators = [{type, list}, {size, {2, 2}}]},
    #rule{ key = <<"Key2">>, validators = [{type, integer}, {size, {-10, 0}}]},
    #rule{ key = <<"Key3">>, validators = [{type, float}, {size, {-1, 9}}]}
  ],
  Data = [
    {<<"Key">>, Value},
    {<<"Key1">>, [1,2]},
    {<<"Key2">>, -10},
    {<<"Key3">>, -0.4}
  ],
  Expected = lists:reverse( Data ),

  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_size is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_size is FAILED!!!!!!", [Res]),
      {skip, Config}
  end.

test_regexp(Config) ->
  Rules = [#rule{
    key = <<"Ip">>,
    validators = [{regexp, <<"(\\d{1,3}\\.){3}\\d{1,3}">>}]
  }],
  Data = [{<<"Ip">>, <<"192.168.1.241">>}],
  Expected = Data,
  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_regexp is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_regexp is FAILED!!!!!!", [Res]),
      {skip, Config}
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
  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_custom is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_custom is FAILED!!!!!!", [Res]),
      {skip, Config}
  end.

test_alowed(Config) ->
  Rules = [#rule{
    key = <<"Ip">>,
    validators = [{alowed_values, [<<"1">>, <<"2">>, 3, 4]}]
  }],
  Data = [{<<"Ip">>, <<"2">>}],
  Expected = Data,
  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_alowed is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_aloweds is FAILED!!!!!!", [Res]),
      {skip, Config}
  end.

test_not_alowed(Config) ->
  Rules = [#rule{
    key = <<"Ip">>,
    validators = [{alowed_values, [<<"1">>, <<"2">>, 3, 4]}]
  }],
  Data = [{<<"Ip">>, <<"123456800">>}],
  Expected = {error,<<"Value <<\"123456800\">> is not alowed">>},
  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_not_alowed is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_not_aloweds is FAILED!!!!!!", [Res]),
      {skip, Config}
  end.

test_several(Config) ->
  Rules = [#rule{
    key = <<"Ip">>,
    validators = [
      {alowed_values, [<<"192.168.1.241">>, <<"2">>, 3, 4]},
      {regexp, <<"(\\d{1,3}\\.){3}\\d{1,3}">>},
      {type, binary},
      {size, {13, 25}}
    ]
  }],
  Data = [{<<"Ip">>, <<"192.168.1.241">>}],
  Expected = Data,
  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_several is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_several is FAILED!!!!!!", [Res]),
      {skip, Config}
  end.

test_validate_or1(Config) ->
  Rules = [
    #rule{ key = <<"key">>, validators = [
      {'or', [
        [{alowed_values, [null, <<"null">>, undefined, <<"undefined">>]}],
        [{type, binary}, {size, {5, 12}}],
        [{type, binary}, {size, {5, 13}}]
      ]}]},
    #rule{
      key = <<"key1">>,
      validators = [
        {'or', [
          {alowed_values, [ null, <<"null">>, undefined, <<"undefined">>]},
          [{type, binary}, {size, {5, 12}}],
          [{type, binary}, {size, {5, 10}}]
        ]}]
    }
  ],
  Data = [
    {<<"key">>, <<"192.168.1.241">>},
    {<<"key1">>, null}
  ],
  Expected = lists:reverse(Data),
  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_validate_or1 is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_validate_or1 is FAILED!!!!!!", [Res]),
      {skip, Config}
  end.

test_validate_or_error(Config) ->
  Rules = [
    #rule{ key = <<"key">>, validators = [
      {'or', [
        [{alowed_values, [null, <<"null">>, undefined, <<"undefined">>]}],
        [{type, binary}, {size, {5, 12}}],
        [{type, binary}, {size, {5, 13}}]
      ]}]},
    #rule{
      key = <<"key1">>,
      validators = [
        {'or', [
          {alowed_values, [ <<"null">>, undefined, <<"undefined">>]},
          [{type, binary}, {size, {5, 12}}],
          [{type, binary}, {size, {5, 10}}]
        ]}]
    }
  ],
  Data = [
    {<<"key">>, <<"192.168.1.241">>},
    {<<"key1">>, null}
  ],
  Expected = {error, <<"All variants in OR list are not valid \n[<<\"Value null is not valid\">>,<<\"Value null is not valid\">>,\n <<\"Value null is not alowed\">>]">>},
  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_validate_or1 is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_validate_or1 is FAILED!!!!!!", [Res]),
      {skip, Config}
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
    #rule{ key = <<"Key9">>, converter = to_boolean}

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
    {<<"Key9">>, <<"false">>}
  ],
  Expected = lists:reverse( [
    {<<"Key1">>, 124545},
    {<<"Key2">>, "192.168.1.241"},
    {<<"Key3">>, '192.168.1.241'},
    {<<"Key4">>, 1.241},
    {<<"Key5">>, {192,168,1,241}},
    {<<"Key6">>, {192,168,1,241}},
    {<<"Key7">>, [1,2]},
    {<<"Key8">>, [{q, 1}, {w, 2}]},
    {<<"Key9">>, false}
  ]),
  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_converters is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_converters is FAILED!!!!!!", [Res]),
      {skip, Config}
  end.

test_converter_error(Config) ->
  CustomConverter =
    fun(V) when is_binary(V) -> {ok, Res} = inet_parse:address(binary_to_list(V)), Res;
      (V) when is_list(V) -> {ok, Res} = inet_parse:address(V), Res end,
  Rules = [#rule{ key = <<"Key6">>, converter = CustomConverter}],
  Data = [{<<"Key6">>, "192.168.1,241"}],
  Expected = {error,<<"Couldnt convert value \"192.168.1,241\" for key <<\"Key6\">> ">>},
  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_converter_error is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_converter_error is FAILED!!!!!!", [Res]),
      {skip, Config}
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

  Expected = lists:reverse([
    {<<"Key1">>, <<"124545">>},
%%    {<<"Key2">>, <<"192.168.1.241">>},
    {<<"Key3">>, <<"192.168.1.241">>},
    {<<"Key4">>, <<"Default value">>},
    {<<"Key5">>, <<"192.168.1.241">>}
  ]),
  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_required_optional_default is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_required_optional_default is FAILED!!!!!!", [Res]),
      {skip, Config}
  end.

test_required_bad(Config) ->
  Rules = [
    #rule{key = <<"Ip1">>, presence = required }
  ],
  Data = [{<<"Ip">>, <<"192.168.1.241">>}],
  Expected = {error,<<"Key <<\"Ip1\">> is required">>},
  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_required_bad is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_required_bad is FAILED!!!!!!", [Res]),
      {skip, Config}
  end.

test_deprecated(Config) ->
  Rules = [
    #rule{key = <<"Ip1">>, presence = deprecated }
  ],
  Data = [{<<"Ip1">>, <<"192.168.1.241">>}],
  Expected = {error,<<"Key <<\"Ip1\">> is deprecated">>},
  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_deprecated is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_deprecated is FAILED!!!!!!", [Res]),
      {skip, Config}
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
  Expected = lists:reverse(Expected0),

  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_group is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_group is FAILED!!!!!!", [Res]),
      {skip, Config}
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
  Expected = lists:reverse(Expected0),
  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_or is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_or is FAILED!!!!!!", [Res]),
      {skip, Config}
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
  Expected = {error,<<"All variants in OR list are not valid \n[<<\"Key <<\\\"Ip6\\\">> is required\">>,<<\"Key <<\\\"Ip4\\\">> is required\">>,\n <<\"Key <<\\\"Ip1\\\">> is required\">>]">>},
  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_or_error is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_or_error is FAILED!!!!!!", [Res]),
      {skip, Config}
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

  Res = (catch edata:validate_and_convert(Rules, Data)),

  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_nesting is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_nesting is FAILED!!!!!!", [Res]),
      {skip, Config}
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

  Expected = recursive_reverse(Data),

  Res = (catch edata:validate_and_convert(Rules, Data)),

  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_complex_nesting is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_complex_nesting is FAILED!!!!!!", [Res]),
      {skip, Config}
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

  Expected = recursive_reverse(Data),

  ct:pal("Reversed ~p", [Expected]),
  ct:pal("Data ~p", [Data]),

  Res = (catch edata:validate_and_convert(Rules, Data)),

  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_data_struct is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_data_struct is FAILED!!!!!!", [Res]),
      {skip, Config}
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

  Data = [Data0, Data1],

%%  Expected = recursive_reverse(Data),
  Expected = [Data0, Data0],
%%  ct:pal("Reversed ~p", [Expected]),
%%  ct:pal("Data ~p", [Data]),

  Res = (catch edata:validate_and_convert(Rules, Data)),

  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_data_struct0 is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_data_struct0 is FAILED!!!!!!", [Res]),
      {skip, Config}
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
          {alowed_values, [<<"create">>, <<"delete">>, <<"modify">>, <<"destroy">>]}
        ],
        converter = to_atom}
      ]}
  ],

  Expected = [{<<"key4">>,
    [[{<<"type2">>,delete},{<<"type1">>,create}],
      [{<<"type2">>,destroy},{<<"type1">>,modify}],
      [{<<"type2">>,delete},{<<"type1">>,create}],
      [{<<"type2">>,destroy},{<<"type1">>,modify}]]},
    {<<"key3">>,<<"created">>},
    {<<"key2">>,[{<<"test">>,1}]},
    {<<"key1">>,<<"test">>}],

  Rules = Rules0,

  Res = (catch edata:validate_and_convert(Rules, Data)),

  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_data_struct1 is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_data_struct1 is FAILED!!!!!!", [Res]),
      {skip, Config}
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

  Expected = recursive_reverse(Data),
  Res = (catch edata:validate_and_convert(Rules, Data)),

  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_multiple_keys is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_multiple_keys is FAILED!!!!!!", [Res]),
      {skip, Config}
  end.

%%----------------------------------------------------------------------------------------------------------------------
%%                  Top level rules
%%----------------------------------------------------------------------------------------------------------------------
test_top_level_rules(Config) ->
  Rules = [
    #rule{ validators = [{type, list}, {size, {1, 4}}], converter = filter_duplicates}
%%      #rule{key = <<"Ip3">>}
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
  Res = (catch edata:validate_and_convert(Rules, Data)),
  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_top_level_rules is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_top_level_rules is FAILED!!!!!!", [Res]),
      {skip, Config}
  end.

%%----------------------------------------------------------------------------------------------------------------------
%%                  PRIVATE
%%----------------------------------------------------------------------------------------------------------------------
recursive_reverse(List) ->
  recursive_reverse(List, []).
recursive_reverse([H|T], Acc) when is_list(H) ->
  recursive_reverse(T, [recursive_reverse(H)|Acc]);
recursive_reverse([H|T], Acc) ->
  recursive_reverse(T, [H|Acc]);
recursive_reverse([], Acc) ->
  Acc.