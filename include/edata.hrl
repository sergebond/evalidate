-author("srg").

-type predefined_converter() :: to_int| to_atom| to_binary| to_list| to_float| filter_duplicates| to_boolean.

-type converter() :: none|
  predefined_converter()|
  fun((V :: term()) -> term()|no_return()).

-type type() :: binary| integer| list| tuple| boolean| atom| list_of_equal_objects| ulist| {'or', list([[type()]])}.

-type presence() :: optional| {optional, Default :: term()}| required| deprecated.

-type validator() ::
  none|
  predefined_validator()|
  fun((V :: term()) -> boolean()|no_return()).

-type predefined_validator() ::
  {type, type()}|
  {size, {non_neg_integer(), non_neg_integer()}}|
  {regexp, binary()}|
  {alowed_values, list()}.

-type key() :: none| binary()| atom()| list().

-record(rule, {
  key = none:: key(),
  presence = required :: presence(),
  validators = none ::  [validator()],
  converter = none:: converter(),
  childs = none :: none|[term()]
}).

-record(rule_and, {
  list :: rules(),
  on_error :: binary()
}).

-record(rule_or, {
  list :: rules(),
  on_error :: binary()
}).

-type rules() :: [#rule{}| #rule_or{}| #rule_and{}].