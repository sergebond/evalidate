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
        evalidate:size_validator(limit ,From, To, Integer)
    catch
      _:_ -> false
    end;
    (Integer) when is_integer(Integer) ->
      evalidate:size_validator(limit, From, To, Integer);
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

-define(V_BINARY_NUMERIC,
  fun(Binary) when is_binary(Binary) ->
    try binary_to_float(Binary) of
      _ -> true
    catch
      _:_ ->
        try binary_to_integer(Binary) of
         _ -> true
        catch _:_-> false
        end
    end;
    (Number) when is_number(Number) -> true;
    (_) -> false end).

-define(V_BINARY_NUMERIC(From, To),
  fun(Binary) when is_binary(Binary) ->
    try binary_to_float(Binary) of
      Float ->
        evalidate:size_validator(limit, From, To, Float)
      catch
        _:_ ->
          try binary_to_integer(Binary) of
            Integer ->
              evalidate:size_validator(limit, From, To, Integer)
          catch
            _:_-> false
          end
    end;
    (Number) when is_number(Number) ->
      evalidate:size_validator(limit, From, To, Number);
    (_) -> false end).

-define(V_BINARY_BOOLEAN,
  fun(MaybeBool) when is_boolean(MaybeBool) -> true;
    (MaybeBool) when MaybeBool == <<"true">>; MaybeBool == <<"false">> -> true;
    (MaybeBool) when MaybeBool == "true"; MaybeBool == "false" -> true;
    (_) -> false
  end).

-define(ELEMENTS_IN(AllowedList),
  fun(ElementsToValidate) ->
    lists:all( fun(Y) -> lists:member(Y, AllowedList)
               end, ElementsToValidate )
  end ).

-define(V_ARRAY,
  fun
    ([]) -> true;
    ([El|_]) when not is_tuple(El) -> true;
    (_) -> false
  end).

-define(V_OBJECT,
  fun
    ([]) -> true;
    ({[]}) -> true; %% Empty object from jiffy decode
    ([Tuple|_]) when is_tuple(Tuple) -> true;
    (_) -> false
  end).