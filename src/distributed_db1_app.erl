-module(distributed_db1_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    http_server:start(),

    distributed_db1_sup:start().

stop(_State) ->
    ok.
