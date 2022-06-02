---
title: "Getting Started with Erlang"
description: ""
project: "riak_kv"
project_version: 2.9.2
menu:
  riak_kv-2.9.2:
    name: "Erlang"
    identifier: "getting_started_erlang"
    weight: 105
    parent: "developing_getting_started"
toc: true
aliases:
  - /riak/2.9.2/dev/taste-of-riak/erlang
  - /riak/kv/2.9.2/dev/taste-of-riak/erlang
---

If you haven't set up a Riak Node and started it, please visit [Running A Cluster]({{<baseurl>}}riak/kv/2.9.2/using/running-a-cluster) first.

To try this flavor of Riak, a working installation of Erlang is
required. You can also use the `erts` Erlang installation that comes
with Riak.

## Client Setup

Download the latest Erlang client from GitHub
([zip](https://github.com/basho/riak-erlang-client/archive/master.zip),
[GitHub repository](https://github.com/basho/riak-erlang-client/)) and
extract it to your working directory.

Next, open the Erlang console with the client library paths included.

```bash
erl -pa CLIENT_LIBRARY_PATH/ebin/ CLIENT_LIBRARY_PATH/deps/*/ebin
```

Now let’s create a link to the Riak node. If you are using a single
local Riak node, use the following to create the link:

```erlang
{ok, Pid} = riakc_pb_socket:start("127.0.0.1", 8087).
```

If you set up a local Riak cluster using the [[five-minute install]]
method, use this code snippet instead:

```erlang
{ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 10017).
```

We are now ready to start interacting with Riak.

## Next Steps

[CRUD Operations]({{<baseurl>}}riak/kv/2.9.2/developing/getting-started/erlang/crud-operations)
