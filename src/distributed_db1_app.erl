-module(distributed_db1_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % ok = http_server:start(),

    distributed_db1_sup:start_link().

stop(_State) ->
    ok.
