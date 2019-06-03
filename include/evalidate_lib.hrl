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

-define(V_URL,
  fun(Url) ->
    case http_uri:parse(eutils:to_str(Url)) of
      {ok, _SomeRes} -> true;
      {error, _} -> false
    end
  end).

-define(V_EMAIL, fun(Email) ->
  Regexp = "^(|(([A-Za-z0-9]+_+)|([A-Za-z0-9]+\\-+)|([A-Za-z0-9]+\\.+)|([A-Za-z0-9]+\\++))*[A-Za-z0-9]+@((\\w+\\-+)|(\\w+\\.))*\\w{1,63}\\.[a-zA-Z]{2,8})$",
  nomatch /= re:run(eutils:to_str(Email), Regexp)
                 end).

-define(V_PASSWORD, fun
  evalidate:validate_password/1).

-define(V_UUID_V4, fun(Arg) ->
  Regexp = "^[0-9A-F]{8}-[0-9A-F]{4}-4[0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}$",
  nomatch /= re:run(eutils:to_str(Arg), Regexp, [caseless])
                   end).

-define(V_UUID_V1, fun(Arg) ->
  Regexp = "[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}",
  nomatch /= re:run(eutils:to_str(Arg), Regexp, [caseless])
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

-define(
   V_UTF8,
   fun(Bin) ->
       case unicode:characters_to_binary(Bin, utf8, utf8) of
         Bin -> true;
         _   -> false
       end
   end
  ).
