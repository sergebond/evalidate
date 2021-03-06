-author("srg").
-include("evalidate_lib.hrl").

-export_type([rules/0]).

-type key() :: none| binary()| atom()| list().

-type presence() :: optional| {optional, Default :: term()}| required| deprecated.

%% Rules
-record(rule, {
  key = none:: key(),
  presence = required :: presence(),
  validators = none :: none| [validator()],
  converter = none:: none| converter(),
  childs = none :: none|[term()],
  on_validate_error = none
}).

%% Logics
-record(rule_and, {
  list :: rules(),
  on_error :: binary()
}).

-record(rule_or, {
  list :: rules(),
  on_error :: binary()
}).

-type rules() :: list(#rule{}| #rule_or{}| #rule_and{}).

%% Converter
-type predefined_converter() :: to_int| to_atom| to_binary| to_list| to_float| filter_duplicates| to_boolean| no_return | {each, converter()}.

-type converter() ::
  predefined_converter()
  |custom_converter().

-type custom_converter() :: fun((V :: term()) -> term()|{error, Reason :: binary()}|no_return()).

%% Validator
-type type() :: binary| integer| list| {list, [validator()]}| tuple| boolean| atom| list_of_equal_objects| uniq_list.

-type predefined_validator() ::
  {type, type()|[type()]}|
  {size, {integer()|infinity, integer()|infinity}}|
  {regexp, binary()}|
  {allowed_values, list()}|
  {allowed, list()}.

-type custom_validator() :: fun((V :: term()) -> boolean()|no_return()).

-type validator() ::
  predefined_validator()
  |custom_validator().