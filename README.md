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

```bash
$ rebar3 compile
```

## Use

### HTTP API

Start the HTTP API with

```bash
$ rebar3 shell --sname supervisor
```

Followed by

```erlang
http_server:start().
```

Alternatively, for startup one can use raw `erl`:

```bash
$ erl -pa _build/default/lib/distributed_db1/ebin -pa _build/default/lib/mochiweb/ebin -sname supervisor
```

This starts a simple HTTP server on port `4000` with the following API:

* `PUT /?key=<key>` - assign the key provided as the `key` query param with the value provided in the request body
* `GET /?key=<key>` - get the value assigned to key provided as the `key` query param
* `DELETE /?key=<key>` - delete all values assigned to key provided as the `key` query param
* `POST /add_node?addr=<addr>` - register the worker node with the address provided as the `addr` query param

### Supervisor

Start supervisor node with

```bash
$ rebar3 shell --sname supervisor
```

This registers a node in a cluster with short name `supervisor`.

Be warned: according to Erlang philosophy, once stick to short / long names -
must keep using them across the cluster, so only use either short names or long names for all nodes in a cluster.

Then, start the supervisor program

```erlang
db_supervisor:start().
```

This should yield the server address - keep a note of it - you'll need it to register worker nodes:

```
(supervisor@machine)1> db_supervisor:start().
Started server at supervisor@machine
ok
```

Register worker node with `db_supervisor:add_node/1`:

```erlang
db_supervisor:add_node(subnod1@machine).
```

This will register a node with the address passed to the function as its only argument and will start the worker program on the node machine:

```
(supervisor@machine)2> db_supervisor:add_node(subnode1@machine).
ok
Node subnode1@machine became online. Monitored nodes: [{subnode1@machine,<8974.89.0>}]
```

More importantly, the supervisor will take the data snapshot from the first alive node and will start the new node with that snapshot, so
the data integrity is preserved.

### Issuing commands

Use `db_supervisor:set/2`, `db_supervisor:get/1` and `db_supervisor:delete/1` to manipulate the data.

#### `db_supervisor:set/2`

Syntax: `db_supervisor:set(<key>, <value>).`

Params:

* `key` - key to be used in the data store; any type
* `value` - value to be assigned to the key; any type

Description:

Assigns the value to the key on all the nodes in the cluster.
Overrides the value if it already exists.

#### `db_supervisor:get/1`

Syntax: `db_supervisor:get(<key>).`

Params:

* `key` - key to be used in the data store; any type

Description:

Reads the value assined to `<key>` on the first node available in the cluster.

#### `db_supervisor:delete/1`

Syntax: `db_supervisor:delete(<key>).`

Params:

* `key` - key to be used in the data store; any type

Description:

Removes all the values assined to `<key>` on all the nodes in the cluster.
