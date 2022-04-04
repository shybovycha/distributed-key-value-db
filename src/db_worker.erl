-module(db_worker).

-export([loop/1]).

loop(Data) ->
    receive
        stop ->
            exit(normal);

        { set, Key, Value } ->
            NewData = maps:put(Key, Value, Data),

            loop(NewData);

        { get, Key, Pid } ->
            Value = maps:get(Key, Data, none),

            Pid ! { get, Value },

            loop(Data);

        { delete, Key } ->
            NewData = maps:remove(Key, Data),

            loop(NewData);

        { get_all, Pid } ->
            Pid ! { got_all, Data },

            loop(Data);

        { got_all, NewData } ->
            loop(NewData)

	end.

