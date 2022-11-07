%%%_* Module declaration ======================================================
-module(kivra_api_errors).

%%%_* Exports =================================================================
-export([load/0, from_code/1, from_code/2]).

%%%_* Types ===================================================================
-type status_code() :: 400..599.
-type payload_map() :: #{binary() := binary()}.
-type payload_kv()  :: [{binary(), binary()}].
-type config()      :: #{atom() := term()}.

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
-spec load() -> ok | {error, atom()}.
load() ->
  load(#{}).

-spec load(config()) -> ok | {error, atom()}.
load(Config) ->
  File = filename:join([code:priv_dir(?MODULE), <<"api-errors.json">>]),
  case file:read_file(File) of
    {ok, Json} ->
      ReturnMaps = maps:get(return_maps, Config, false),
      maps:fold(fun(Code, LongShort, ok) ->
        HTTPStatus = binary_to_integer(binary:part(Code, 0, 3)),
        Payload    = format(LongShort#{<<"code">> => Code}, ReturnMaps),
        persistent_term:put(key(Code), {HTTPStatus, Payload})
      end, ok, jiffy:decode(Json, [return_maps]));
    Error ->
      Error
  end.

-spec from_code(binary() | pos_integer()) -> {ok, {status_code(), payload_map() | payload_kv()}} | {error, notfound}.
from_code(ErrorCode) when is_integer(ErrorCode) ->
  from_code(integer_to_binary(ErrorCode));
from_code(ErrorCode) ->
  lookup(ErrorCode).

-spec from_code(binary() | pos_integer(), binary()) -> {ok, {status_code(), payload_map() | payload_kv()}} | {error, notfound}.
from_code(ErrorCode, LongMessage) when is_integer(ErrorCode) ->
  from_code(integer_to_binary(ErrorCode), LongMessage);
from_code(ErrorCode, LongMessage) ->
  case from_code(ErrorCode) of
    {ok, {HTTPStatus, Payload}} ->
      {ok, {HTTPStatus, set(<<"long_message">>, LongMessage, Payload)}};
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

set(Field, Value, Payload) when is_map(Payload) ->
  Payload#{Field => Value};
set(Field, Value, Payload) when is_list(Payload) ->
  maps:to_list(set(Field, Value, maps:from_list(Payload))).

format(Payload, true) when is_map(Payload) ->
  Payload;
format(Payload, false) when is_map(Payload) ->
  maps:to_list(Payload).

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_error_map_ok_test() ->
  ok       = load(#{return_maps => true}),
  Expected = #{ <<"code">> => <<"40000">>
              , <<"short_message">> => <<"Bad Request">>
              , <<"long_message">> => <<"The server cannot or will not process the request due to an apparent client error">> },
  ?assertEqual({ok, {400, Expected}}, from_code(<<"40000">>)),
  ?assertEqual({ok, {400, Expected}}, from_code(40000)).

get_error_map_overwrite_long_message_ok_test() ->
  ok       = load(#{return_maps => true}),
  Expected = #{ <<"code">> => <<"40000">>
              , <<"short_message">> => <<"Bad Request">>
              , <<"long_message">> => <<"Long Message">> },
  ?assertEqual({ok, {400, Expected}}, from_code(<<"40000">>, <<"Long Message">>)).

get_error_proplist_ok_test() ->
  ok       = load(),
  Expected = [ {<<"code">>, <<"40000">>}
             , {<<"short_message">>, <<"Bad Request">>}
             , {<<"long_message">>, <<"The server cannot or will not process the request due to an apparent client error">> }],
  Returned = from_code(<<"40000">>),
  ?assertMatch({ok, {400, _}}, Returned),
  {ok, {400, Actual}} = Returned,
  ?assertEqual(maps:from_list(Expected), maps:from_list(Actual)).

get_error_proplist_overwrite_long_message_ok_test() ->
  ok       = load(),
  Expected = [ {<<"code">>, <<"40000">>}
             , {<<"short_message">>, <<"Bad Request">>}
             , {<<"long_message">>, <<"Long Message">> }],
  Returned = from_code(<<"40000">>, <<"Long Message">>),
  ?assertMatch({ok, {400, _}}, Returned),
  {ok, {400, Actual}} = Returned,
  ?assertEqual(maps:from_list(Expected), maps:from_list(Actual)).

notfound_test() ->
  ok = load(),
  ?assertEqual({error, notfound}, from_code(<<"60000">>)).

-endif.
