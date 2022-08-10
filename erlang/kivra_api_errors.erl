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
      maps:fold(fun(K, V, ok) ->
        persistent_term:put(key(K), V)
      end, ok, jiffy:decode(Json, [return_maps]));
    Error ->
      Error
  end.

-spec from_code(binary()) -> {ok, {200..599, map()}} | {error, notfound}.
from_code(ErrorCode) ->
  case lookup(ErrorCode) of
    {ok, ErrorDefinition} ->
      HTTPStatus = binary_to_integer(binary:part(ErrorCode, 0, 3)),
      {ok, {HTTPStatus, ErrorDefinition#{<<"code">> => ErrorCode}}};
    {error, notfound} = Error ->
      Error
  end.

-spec from_code(binary(), binary()) -> {ok, {200..599, map()}} | {error, notfound}.
from_code(ErrorCode, LongMessage) ->
  case from_code(ErrorCode) of
    {ok, {HTTPStatus, ErrorDefinition}} ->
      {ok, {HTTPStatus, ErrorDefinition#{<<"long_message">> => LongMessage}}};
    {error, notfound} = Error ->
      Error
  end.

%%%_* Private -----------------------------------------------------------------
lookup(ErrorCode) ->
  case persistent_term:get(key(ErrorCode), undefined) of
    undefined       -> {error, notfound};
    ErrorDefinition -> {ok, ErrorDefinition}
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
  ?assertEqual({ok, {400, Expected}}, from_code(<<"40000">>)).

notfound_test() ->
  ok = load(),
  ?assertEqual({error, notfound}, from_code(<<"60000">>)).

-endif.
