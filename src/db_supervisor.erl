-module(db_supervisor).

-export([start/0, stop/0, add_node/1, nodes/0, set/2, get/1, delete/1, loop/0]).

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
    loop([]).

loop(Nodes) ->
    receive
        stop -> ok;

        { add_node, NewNodeAddr } ->
            NewNodePid = spawn(NewNodeAddr, db_worker, loop, [ maps:new() ]),

            monitor(process, NewNodePid),

            NewNodes = lists:append([ { NewNodeAddr, NewNodePid } ], Nodes),

            io:format("Node ~w became online. Monitored nodes: ~w~n", [NewNodeAddr, NewNodes]),

            case Nodes of
                [ { _, FirstNodePid } | _ ] -> FirstNodePid ! { get_all, NewNodePid };

                _ -> ok
            end,

            loop(NewNodes);

        { nodes, Pid } ->
            Pid ! { nodes, Nodes },

            loop(Nodes);

        { set, Key, Value } ->
            lists:foreach(fun ({ _, NodePid }) -> NodePid ! { set, Key, Value } end, Nodes),

            loop(Nodes);

        { get, Key, Pid } ->
            case Nodes of
                [ { _, FirstNodePid } | _ ] -> FirstNodePid ! { get, Key, Pid };

                _ -> Pid ! { get, no_nodes }
            end,

            loop(Nodes);

        { delete, Key } ->
            lists:foreach(fun ({ _, NodePid }) -> NodePid ! { delete, Key } end, Nodes),

            loop(Nodes)

    after
        0 ->
            AliveNodes = lists:filter(fun ({ NodeAddr, _ }) -> net_adm:ping(NodeAddr) == pong end, Nodes),

            loop(AliveNodes)
    end,

    ok.
