-module(eutils).
-author("srg").

%% CONVERTERS
-export([
  to_bin/1,
  to_str/1,
  to_int/1,
  to_float/1,
  to_atom/1,
  to_boolean/1
]).

%% MISC
-export([
  get_value/2
]).

%% BINARIES
-export([
  bjoin/1,
  bjoin/2
]).

%%------------------TYPE CONVERSION-------------------------------------------------------------------------------------
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
-spec to_int(binary()|list()|integer()|atom()) -> integer().
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
-spec to_atom(binary()|list()|float()) -> float().
to_atom(X) when is_binary(X) -> binary_to_atom(X, utf8);
to_atom(X) when is_list(X) -> binary_to_atom(list_to_binary(X), utf8).

to_boolean(X) when X =:= <<"true">> orelse X =:= <<"false">> -> binary_to_atom( X , utf8);
to_boolean(X) when X =:= true orelse X =:= false ->  X;
to_boolean(X) when  X =:= "true" orelse X =:= "false" -> binary_to_atom(list_to_binary(X), utf8).

%%  MISC
%%______________________________________________________________________________________________________________________
get_value(Key, List)->
  get_value(Key, List, undefined).
get_value(Key, List, Default)->
  case lists:keyfind(Key, 1, List) of
    {_, Val} -> Val;
    _        -> Default
  end.

%%  BINARIES
%%______________________________________________________________________________________________________________________
-spec bjoin(List :: list(binary())) -> binary().
bjoin([])  -> <<>>;
bjoin([H|T]) -> << H/bitstring, (bjoin(T))/bitstring >>.

bjoin([H|[]], _Sep) when is_binary(H) ->
  <<H/bitstring >>;
bjoin([H|T], Sep) when is_binary(H) ->
  << H/bitstring, Sep/bitstring, (bjoin(T, Sep))/bitstring >>.
