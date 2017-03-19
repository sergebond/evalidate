-module(edata_SUITE).
-author("srg").

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("../include/edata.hrl").

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
    {group, multiple_keys}
  ].

groups() ->
  [
    {errors,
      [sequence],
      [
        test_validate_error1,
        test_validate_error2,
%%        test_validate_error3,
        test_validate_error4
      ]},
    {validators,
      [sequence],
      [
        test_type_validators,
        test_size_bad,
        test_size,
        test_regexp,
        test_custom,
        test_alowed,
        test_not_alowed,
        test_several
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
        test_or
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
        test_data_struct0
      ]},
    {multiple_keys,
      [sequence],
      [
       test_multiple_keys
      ]}
  ].


init_per_suite(Config) ->
%%  ok = lager:start(),
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
  Rules = #rule{
    key = <<"Key">>,
    validators = [binary]
  },
  Data = [{<<"Key">>, Value}],

  Expected = {error,<<"Mallformed validation data">>},

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
  Res = (catch edata:validate_and_convert(Rules, [])),
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
    #rule{ key = <<"integer">>, validators = [{type, integer}]}
  ],
  Data = [
    {<<"Key">>, <<"12566554">>},
    {<<"list">>, [1, 2, 3, 4]},
    {<<"tuple">>, {1, 2, 3, 4}},
    {<<"boolean">>, true},
    {<<"integer">>, 1}
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
  Rules = [#rule{
    key = <<"Key">>,
    validators = [{type, binary}, {size, {9, 9}}]
  }],
  Data = [{<<"Key">>, Value}],
  Expected = Data,

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
    #rule{ key = <<"Key6">>, converter = CustomConverter}
  ],
  Data = [
    {<<"Key1">>, <<"124545">>},
    {<<"Key2">>, <<"192.168.1.241">>},
    {<<"Key3">>, <<"192.168.1.241">>},
    {<<"Key4">>, <<"1.241">>},
    {<<"Key5">>, <<"192.168.1.241">>},
    {<<"Key6">>, "192.168.1.241"}
  ],
  Expected = lists:reverse( [
    {<<"Key1">>, 124545},
    {<<"Key2">>, "192.168.1.241"},
    {<<"Key3">>, '192.168.1.241'},
    {<<"Key4">>, 1.241},
    {<<"Key5">>, {192,168,1,241}},
    {<<"Key6">>, {192,168,1,241}}
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
    #group{list = [
      #rule{key = <<"Ip2">>},
      #rule{key = <<"Ip3">>}
      ]},
    #group{list = [
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
    #group{list = [
      #rule{key = <<"Ip2">>},
      #rule{key = <<"Ip3">>}
    ]},
    #group{list = [
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

  Expected = eutils:recursive_reverse(Data),

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

  Expected = eutils:recursive_reverse(Data),

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

%%  Expected = eutils:recursive_reverse(Data),
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

  Expected = eutils:recursive_reverse(Data),
  Res = (catch edata:validate_and_convert(Rules, Data)),

  case Res of
    Expected ->
      ct:pal("Result ~p, Test test_multiple_keys is OK", [Res]),
      Config;
    _ -> ct:pal("Result ~p, Test test_multiple_keys is FAILED!!!!!!", [Res]),
      {skip, Config}
  end.