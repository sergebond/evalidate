# evalidate
## library for data validation and conversion

## Features
### Validators
```
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
 Expected = evalidate:validate_and_convert(Rules, Data)),
```
Errors and other use cases you can find in test/evalidate_SUITE.erl. Rules specs is in include/evalidate.hrl

### Converters
```
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
  Expected = catch evalidate:validate_and_convert(Rules, Data))
```

## Data presence
```
  Rules = [
    #rule{ key = <<"Key1">>, presence = required },
    #rule{ key = <<"Key2">>, presence = optional },
    #rule{ key = <<"Key3">>, presence = optional },
    #rule{ key = <<"Key4">>, presence = { optional, <<"Default value">>} },
    #rule{ key = <<"Key5">>, presence = { optional, <<"Default value">>} }
  ],
  Data = [
    {<<"Key1">>, <<"124545">>},
    {<<"Key3">>, <<"192.168.1.241">>},
    {<<"Key5">>, <<"192.168.1.241">>}
  ],
  Expected = [
    {<<"Key1">>, <<"124545">>},
    {<<"Key3">>, <<"192.168.1.241">>},
    {<<"Key4">>, <<"Default value">>},
    {<<"Key5">>, <<"192.168.1.241">>}
  ],
  Expected = evalidate:validate_and_convert(Rules, Data),
```