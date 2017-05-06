-author("srg").

-define(V_BINARY_INTEGER,
  fun(Binary) when is_binary(Binary) ->
    try binary_to_integer(Binary) of
      _ -> true
    catch
      _:_ -> false
    end;
    (Integer) when is_integer(Integer) -> true;
    (_) -> false end).

-define(V_BINARY_INTEGER(From, To),
  fun(Binary) when is_binary(Binary) ->
    try binary_to_integer(Binary) of
      Integer ->
        evalidate:size_validator(From, To, Integer)
    catch
      _:_ -> false
    end;
    (Integer) when is_integer(Integer) ->
      evalidate:size_validator(From, To, Integer);
    (_) -> false end).

-define(V_URL, %% @todo Non optimal
  fun(Url) when is_binary(Url) ->
    case http_uri:parse(binary_to_list(Url)) of
      {ok, _SomeRes} -> true;
      {error, _} -> false
    end;
    (Url) when is_list(Url) ->
      case http_uri:parse(Url) of
        {ok, _SomeRes} -> true;
        {error, _} -> false
      end;
    (_) -> false
  end).