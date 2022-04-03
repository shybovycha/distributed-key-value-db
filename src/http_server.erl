-module(http_server).

-export([ start/0, stop/0, dispatch/1 ]).

-define(HTTP_OPTS,
  [ { loop, {?MODULE, dispatch} },
    { port, 4000 },
    { name, http_4000 } ]).

start() ->
  { ok, Http } = mochiweb_http:start(?HTTP_OPTS),

  HttpPid = spawn_link(fun () -> loop(Http) end),

  register(http_server, HttpPid),

  DbPid = spawn_link(fun () -> db_supervisor:loop() end),

  register(db_server, DbPid),

  ok.

stop() ->
  http_server ! stop,
  db_server ! stop,

  ok.

dispatch(Req) ->
  case mochiweb_request:get(method, Req) of
    'GET' -> get_value(Req);

    'PUT' -> put_value(Req);

    'DELETE' -> delete_value(Req);

    'POST' -> post_value(Req);

    _ -> method_not_allowed(Req)
  end.

get_value(Req) ->
    % parse query string
    QueryParams = mochiweb_request:parse_qs(Req),

    % find the tuple in QueryParams where first (1) element is equal to "key"
    { _, Key } = lists:keyfind("key", 1, QueryParams),

    db_server ! { get, Key, self() },

    receive
        { get, Value } ->
            Headers = [{ "Content-Type", "text/plain" }],

            Res = if
                Value =:= none -> { 204, Headers, "" };

                true -> { 200, Headers, Value }
            end,

            mochiweb_request:respond(Res, Req)
    end,

    ok.

put_value(Req) ->
    % parse query string
    QueryParams = mochiweb_request:parse_qs(Req),

    % find the tuple in QueryParams where first (1) element is equal to "key"
    { _, Key } = lists:keyfind("key", 1, QueryParams),

    % value is taken from request body
    Value = mochiweb_request:recv_body(Req),

    db_server ! { set, Key, Value },

    mochiweb_request:respond({ 201, [], "201 Created\r\n" }, Req),

    ok.

delete_value(Req) ->
    % parse query string
    QueryParams = mochiweb_request:parse_qs(Req),

    % find the tuple in QueryParams where first (1) element is equal to "key"
    { _, Key } = lists:keyfind("key", 1, QueryParams),

    db_server ! { delete, Key },

    mochiweb_request:respond({ 200, [], "200 OK\r\n" }, Req),

    ok.

post_value(Req) ->
    Path = mochiweb_request:get(path, Req),

    if
        Path =:= "/add_node" ->
            % parse query string
            QueryParams = mochiweb_request:parse_qs(Req),

            % find the tuple in QueryParams where first (1) element is equal to "addr"
            { _, NewNodeAddrStr } = lists:keyfind("addr", 1, QueryParams),

            NewNodeAddr = list_to_atom(NewNodeAddrStr),

            db_server ! { add_node, NewNodeAddr },

            mochiweb_request:respond({ 200, [], "200 OK\r\n" }, Req);

        true ->
            mochiweb_request:respond({ 405, [], "Method not allowed\r\n" }, Req)
    end,

    ok.

method_not_allowed(Req) ->
  Path = mochiweb_request:get(path, Req),
  Method = mochiweb_request:get(method, Req),

  Body = io_lib:format("Method ~s on path ~s is not supported", [ Method, Path ]),

  mochiweb_request:respond({ 405, [], Body }, Req),

  ok.

loop(Http) ->
  receive
    stop ->
      ok = mochiweb_http:stop(Http),
      exit(normal);

    _ -> ignore
  end,

  (?MODULE):loop(Http).
