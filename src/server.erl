-module(server).

-export([start/0, stop/0, add_node/1, nodes/0, nodes_alive/0, set/2, get/1, delete/1]).

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

nodes() ->
   server ! { nodes, self() },

   receive
       { nodes, Nodes } -> Nodes
   end.

nodes_alive() ->
   server ! { nodes_alive, self() },

   receive
       { nodes_alive, Nodes } -> Nodes
   end.

set(Key, Value) ->
    server ! { set, Key, Value },

    ok.

get(Key) ->
    server ! { get, Key, self() },

    receive
        { get, Value } -> Value
    end.

delete(Key) ->
    server ! { delete, Key },

    ok.

loop() ->
    loop(sets:new()).

loop(Nodes) ->
    receive
        stop -> exit(normal);

        { add_node, Addr } ->
            AliveNodes = sets:filter(fun({ Addr1, _ }) -> net_adm:ping(Addr1) == pong end, Nodes),

            NoNodes = sets:is_empty(AliveNodes),

            Data = if
                NoNodes -> maps:new();

                true ->
                    [{ _, FirstNodePid }|_] = sets:to_list(AliveNodes),

                    FirstNodePid ! { get_all, self() },

                    receive
                        { get_all, NewData } -> NewData
                    end
            end,

            Pid = spawn(Addr, client, loop, [Data]),

            monitor(process, Pid),

            NewNodes = sets:add_element({ Addr, Pid }, Nodes),

            io:format("Node ~w became online. Monitored nodes: ~w~n", [Addr, sets:to_list(NewNodes)]),

            loop(NewNodes);

        { 'DOWN', _, _, { _, Addr }, _ } ->
            NewNodes = sets:filter(fun({ Addr1, _ }) -> Addr1 =/= Addr end, Nodes),

            io:format("Node ~w went offline. Alive nodes: ~w~n", [Addr, sets:to_list(NewNodes)]),

            loop(NewNodes);

        { nodes, Pid } ->
            Pid ! { nodes, sets:to_list(Nodes) },

            loop(Nodes);

        { nodes_alive, Pid } ->
            AliveNodes = lists:filter(fun({ Addr1, _ }) -> net_adm:ping(Addr1) == pong end, sets:to_list(Nodes)),

            Pid ! { nodes_alive, AliveNodes },

            loop(Nodes);

        { set, Key, Value } ->
            AliveNodes = sets:filter(fun({ Addr1, _ }) -> net_adm:ping(Addr1) == pong end, Nodes),

            NoNodes = sets:is_empty(AliveNodes),

            if
                NoNodes -> loop(AliveNodes);

                true -> lists:foreach(fun({ _, Pid }) -> Pid ! { set, Key, Value } end, sets:to_list(AliveNodes)), loop(AliveNodes)
            end;

        { get, Key, Pid } ->
            AliveNodes = sets:filter(fun({ Addr1, _ }) -> net_adm:ping(Addr1) == pong end, Nodes),

            NoNodes = sets:is_empty(AliveNodes),

            if
                NoNodes -> Pid ! { get, no_nodes }, loop(AliveNodes);

                true ->
                    [{ _, FirstNodeAlivePid }|_] = sets:to_list(AliveNodes),

                    FirstNodeAlivePid ! { get, Key, Pid },

                    loop(AliveNodes)
            end;

        { delete, Key } ->
            AliveNodes = sets:filter(fun({ Addr1, _ }) -> net_adm:ping(Addr1) == pong end, Nodes),

            NoNodes = sets:is_empty(AliveNodes),

            if
                NoNodes -> loop(AliveNodes);

                true -> lists:foreach(fun({ _, Pid }) -> Pid ! { delete, Key } end, sets:to_list(AliveNodes)), loop(AliveNodes)
            end

    end,

    ok.

