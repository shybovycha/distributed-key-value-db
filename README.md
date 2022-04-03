# Distributed DB

A distributed key-value storage in Erlang.

The database runs in a cluster. The cluster needs a supervisor node, represented by `server.erl`.
This node will keep track of all the worker nodes alive and distribute the load between them.

As long as there is at least one worker node alive, the data integrity and operation consistency should be guaranteed.

Supports simple operations:

* `get <key>`
* `set <key> <value>`
* `delete <key>`

## Build

    $ rebar3 compile

## Use

### Supervisor

Start supervisor node with

    $ erl -pa _build/default/lib/distributed_db1/ebin -sname supervisor

This registers a node in a cluster with short name `supervisor`.

Be warned: according to Erlang philosophy, once stick to short / long names -
must keep using them across the cluster, so only use either short names or long names for all nodes in a cluster.

Then, start the supervisor program

```erl
server:start().
```

This should yield the server address - keep a note of it - you'll need it to register worker nodes:

```
(supervisor@machine)1> server:start().
Started server at supervisor@machine
ok
```

Register worker node with `server:add_node/1`:

```erl
server:add_node(subnod1@machine).
```

This will register a node with the address passed to the function as its only argument and will start the worker program on the node machine:

```
(supervisor@machine)2> server:add_node(subnode1@machine).
ok
Node subnode1@machine became online. Monitored nodes: [{subnode1@machine,<8974.89.0>}]
```

More importantly, the supervisor will take the data snapshot from the first alive node and will start the new node with that snapshot, so
the data integrity is preserved.

### Issuing commands

Use `server:set/2`, `server:get/1` and `server:delete/1` to manipulate the data.

#### `server:set/2`

Syntax: `server:set(<key>, <value>).`

Params:

* `key` - key to be used in the data store; any type
* `value` - value to be assigned to the key; any type

Description:

Assigns the value to the key on all the nodes in the cluster.
Overrides the value if it already exists.

#### `server:get/1`

Syntax: `server:get(<key>).`

Params:

* `key` - key to be used in the data store; any type

Description:

Reads the value assined to `<key>` on the first node available in the cluster.

#### `server:delete/1`

Syntax: `server:delete(<key>).`

Params:

* `key` - key to be used in the data store; any type

Description:

Removes all the values assined to `<key>` on all the nodes in the cluster.
