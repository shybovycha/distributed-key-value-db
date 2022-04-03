-module(server).

-export([start/0, stop/0, add_node/1, nodes_available/0, set_value/2, get_value/1, delete_value/1]).

start() ->
    Addr = node(),

    Pid = spawn_link(fun() -> loop() end),

    register(server, Pid),

    io:format("Started server at ~s~n", [Addr]),

    ok.

stop() ->
    server ! stop,

    ok.

add_node(Addr) ->
   server ! { add_node, Addr },

   ok.

nodes_available() ->
   server ! { nodes, self() },

   receive
       { nodes, Nodes } -> Nodes
   end.

set_value(Key, Value) ->
    server ! { set, Key, Value },

    ok.

get_value(Key) ->
    server ! { get, Key, self() },

    receive
        { get, Value } -> Value
    end.

delete_value(Key) ->
    server ! { delete, Key },

    ok.

loop() ->
    loop(sets:new()).

loop(Nodes) ->
    receive
        stop -> exit(normal);

        { add_node, Addr } ->
            % TODO: take the data from any alive node ! get_all
            Pid = spawn(Addr, client, loop, [maps:new()]),

            monitor(process, Pid),

            % TODO: replace Addr with a struct #{ Addr, Pid }
            NewNodes = sets:add_element({ Addr, Pid }, Nodes),

            io:format("Node ~w became online. Monitored nodes: ~w~n", [Addr, sets:to_list(NewNodes)]),

            loop(NewNodes);

        { 'DOWN', MonitorRef, _, { _, Addr }, Reason } ->
            NewNodes = sets:filter(fun({ Addr1, Pid1 }) -> Addr1 =/= Addr end, Nodes),

            io:format("Node ~w went offline. Alive nodes: ~w~n", [Addr, sets:to_list(NewNodes)]),

            loop(NewNodes);

        { nodes, Pid } ->
            Pid ! { nodes, Nodes },

            loop(Nodes);

        { set, Key, Value } ->
            lists:foreach(fun({ Addr, Pid }) -> Pid ! { set, Key, Value } end, sets:to_list(Nodes)),

            loop(Nodes);

        { get, Key, Pid } ->
            lists:foreach(fun({ Addr1, Pid1 }) -> Pid1 ! { get, Key, Pid } end, sets:to_list(Nodes)),

            loop(Nodes);

        { delete, Key } ->
            lists:foreach(fun({ Addr, Pid }) -> Pid ! { delete, Key } end, Nodes),

            loop(Nodes)

    end,

    ok.

