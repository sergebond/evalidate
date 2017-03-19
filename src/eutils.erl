-module(eutils).
%% API
-export([

  to_bin/1,
  to_str/1,
  to_int/1,
  to_float/1,
  to_atom/1,

  recursive_reverse/1,

  get_value/2,
  get_value/3
]).

%% @doc universal converter to binary
-spec to_bin(binary()|list()|integer()|atom()|float()) -> binary().
to_bin(X) when is_binary(X) -> X;
to_bin(X) when is_list(X) -> list_to_binary(X);
to_bin(X) when is_integer(X) -> integer_to_binary(X);
to_bin(X) when is_atom(X) -> atom_to_binary(X, utf8);
to_bin(X) when is_float(X) -> float_to_binary(X, [{decimals, 4}]).

%% @doc universal converter to string(list)
-spec to_str(binary()|list()|integer()|atom()|float()) -> list().
to_str(X) when is_list(X) -> X;
to_str(X) when is_binary(X) -> binary_to_list(X);
to_str(X) when is_integer(X) -> integer_to_list(X);
to_str(X) when is_atom(X) -> atom_to_list(X);
to_str(X) when is_float(X) -> float_to_list(X,[{decimals, 4}]).

%% @doc universal converter to integer
-spec to_int(binary()|list()|integer()|atom()|float()) -> integer().
to_int(X) when is_integer(X) -> X;
to_int(X) when is_binary(X) -> binary_to_integer(X);
to_int(X) when is_list(X) -> list_to_integer(X);
to_int(X) when is_float(X) -> round(X);
to_int(X) when is_atom(X) -> list_to_integer(atom_to_list(X)).

%% @doc universal converter to float
-spec to_float(binary()|list()|float()) -> float().
to_float(X) when is_float(X) -> X;
to_float(X) when is_binary(X) -> binary_to_float(X);
to_float(X) when is_list(X) -> list_to_float(X).

%% @doc universal converter to atom
-spec to_atom(binary()|list()) -> float().
to_atom(X) when is_binary(X) -> binary_to_atom(X, utf8);
to_atom(X) when is_list(X) -> binary_to_atom(list_to_binary(X), utf8).

-spec recursive_reverse(list()) -> list().
recursive_reverse(List) ->
  recursive_reverse(List, []).
recursive_reverse([H|T], Acc) when is_list(H) ->
  recursive_reverse(T, [recursive_reverse(H)|Acc]);
recursive_reverse([H|T], Acc) ->
  recursive_reverse(T, [H|Acc]);
recursive_reverse([], Acc) ->
  Acc.

get_value(Key, List)->
  get_value(Key, List, undefined).
get_value(Key, List, Default)->
  case lists:keyfind(Key, 1, List) of
    {_, Val} -> Val;
    _        -> Default
  end.