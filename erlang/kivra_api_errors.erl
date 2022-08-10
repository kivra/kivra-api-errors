%%%_* Module declaration ======================================================
-module(kivra_api_errors).

%%%_* Exports =================================================================
-export([load/0, from_code/1, from_code/2]).

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
-spec load() -> ok | {error, atom()}.
load() ->
  File = filename:join([code:priv_dir(?MODULE), <<"api-errors.json">>]),
  case file:read_file(File) of
    {ok, Json} ->
      maps:fold(fun(Code, LongShort, ok) ->
        HTTPStatus = binary_to_integer(binary:part(Code, 0, 3)),
        Payload    = LongShort#{<<"code">> => Code},
        persistent_term:put(key(Code), {HTTPStatus, Payload})
      end, ok, jiffy:decode(Json, [return_maps]));
    Error ->
      Error
  end.

-spec from_code(binary() | pos_integer()) -> {ok, {200..599, map()}} | {error, notfound}.
from_code(ErrorCode) when is_integer(ErrorCode) ->
  from_code(integer_to_binary(ErrorCode));
from_code(ErrorCode) ->
  lookup(ErrorCode).

-spec from_code(binary() | pos_integer(), binary()) -> {ok, {200..599, map()}} | {error, notfound}.
from_code(ErrorCode, LongMessage) when is_integer(ErrorCode) ->
  from_code(integer_to_binary(ErrorCode), LongMessage);
from_code(ErrorCode, LongMessage) ->
  case from_code(ErrorCode) of
    {ok, {HTTPStatus, Payload}} ->
      {ok, {HTTPStatus, Payload#{<<"long_message">> => LongMessage}}};
    {error, notfound} = Error ->
      Error
  end.

%%%_* Private -----------------------------------------------------------------
lookup(ErrorCode) ->
  case persistent_term:get(key(ErrorCode), undefined) of
    undefined             -> {error, notfound};
    {HTTPStatus, Payload} -> {ok, {HTTPStatus, Payload}}
  end.

key(ErrorCode) ->
  {?MODULE, ErrorCode}.

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_error_ok_test() ->
  ok       = load(),
  Expected = #{ <<"code">> => <<"40000">>
              , <<"short_message">> => <<"Bad Request">>
              , <<"long_message">> => <<"The server cannot or will not process the request due to an apparent client error">> },
  ?assertEqual({ok, {400, Expected}}, from_code(<<"40000">>)),
  ?assertEqual({ok, {400, Expected}}, from_code(40000)).

notfound_test() ->
  ok = load(),
  ?assertEqual({error, notfound}, from_code(<<"60000">>)).

-endif.
