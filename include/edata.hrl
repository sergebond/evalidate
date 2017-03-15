-type predefined_converter() :: to_int| to_atom| to_binary| to_list| to_float.

-type converter() :: none|
  predefined_converter()|
  fun((V :: term()) -> term()|no_return()).

-type type() :: binary| binary_integer| integer| list| tuple| boolean| atom.

-type presence() :: optional| {optional, Default :: term()}| required| deprecated.

-type validator() :: none|
  predefined_validator()|
  fun((V :: term()) -> boolean()|no_return()).

-type predefined_validator() :: {type, type()}|
  {size, {non_neg_integer(), non_neg_integer()}}|
  {regexp, binary()}|
  {alowed_values, list()}.

-record(rule, {
  key :: binary()| atom()| list(),
  presence = required :: presence(),
  validators = none ::  [validator()],
  converter = none:: converter(),
  childs = none :: none|[term()]
}).

-record(group, {
  list :: rules(),
  on_error :: binary()
}).

-record(rule_or, {
  list :: rules(),
  on_error :: binary()
}).

-type rules() :: [#rule{}| #rule_or{}| #group{}].