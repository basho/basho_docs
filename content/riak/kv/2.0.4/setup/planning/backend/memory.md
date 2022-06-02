---
title: "Memory"
description: ""
project: "riak_kv"
project_version: "2.0.4"
menu:
  riak_kv-2.0.4:
    name: "Memory"
    identifier: "planning_backend_memory"
    weight: 102
    parent: "planning_choose_backend"
toc: true
aliases:
  - /riak/2.0.4/ops/advanced/backends/memory/
  - /riak/kv/2.0.4/ops/advanced/backends/memory/
---

[config reference]: {{<baseurl>}}riak/kv/2.0.4/configuring/reference
[plan backend multi]: {{<baseurl>}}riak/kv/2.0.4/setup/planning/backend/multi
[glossary vnode]: {{<baseurl>}}riak/kv/2.0.4/learn/glossary/#vnode
[plan backend leveldb]: {{<baseurl>}}riak/kv/2.0.4/setup/planning/backend/leveldb

The Memory storage backend uses in-memory tables to store all data.
This data is never persisted to disk or to any other storage mechanism.
The Memory storage engine is best used for testing Riak clusters or for
storing small amounts of transient state in production systems.

Internally, the Memory backend uses Erlang Ets tables to manage data.
More information can be found in the
[official Erlang documentation](http://www.erlang.org/doc/man/ets.html).

## Enabling the Memory Backend

To enable the memory backend, edit your [configuration files][config reference]
for each Riak node and specify the Memory backend as shown in the following
example:

```riakconf
storage_backend = memory
```

```appconfig
{riak_kv, [
    ...,
    {storage_backend, riak_kv_memory_backend},
    ...
    ]}
```

**Note**: If you *replace* the existing specified backend by removing it
or commenting it out as shown in the above example, data belonging to
the previously specified backend will still be preserved on the
filesystem but will no longer be accessible through Riak unless the
backend is enabled again.

If you require multiple backends in your configuration, please consult
the [Multi backend documentation][plan backend multi].

## Configuring the Memory Backend

The Memory backend enables you to configure two fundamental aspects of
object storage: maximum memory usage per [vnode][glossary vnode]
and object expiry.

### Max Memory

This setting specifies the maximum amount of memory consumed by the
Memory backend. It's important to note that this setting acts on a
*per-vnode basis*, not on a per-node or per-cluster basis. This should
be taken into account when planning for memory usage with the Memory
backend, as the total memory used will be max memory times the number
of vnodes in the cluster.

When the threshold value that you set has been met in a particular
vnode, Riak will begin discarding objects, beginning with the oldest
object and proceeding until memory usage returns below the allowable
threshold.

You can configure maximum memory using the
`memory_backend.max_memory_per_vnode` setting. You can specify
`max_memory_per_vnode` however you'd like, using kilobytes, megabytes,
or even gigabytes.

The following are all possible settings:

```riakconf
memory_backend.max_memory_per_vnode = 500KB
memory_backend.max_memory_per_vnode = 10MB
memory_backend.max_memory_per_vnode = 2GB
```

```appconfig
%% In the app.config-based system, the equivalent setting is max_memory,
%% which must be expressed in megabytes:

{riak_kv, [
    %% storage_backend specifies the Erlang module defining the storage
    %% mechanism that will be used on this node.

    {storage_backend, riak_kv_memory_backend},
    {memory_backend, [
        ...,
            {max_memory, 4096}, %% 4GB in megabytes
        ...
    ]}
```

To determine an optimal max memory setting, we recommend consulting the
documentation on [LevelDB cache size][plan backend leveldb].

### TTL

The time-to-live (TTL) parameter specifies the amount of time an object
remains in memory before it expires. The minimum time is one second.

In the newer, `riak.conf`-based configuration system, you can specify
`ttl` in seconds, minutes, hours, days, etc. The following are all
possible settings:

```riakconf
memory_backend.ttl = 1s
memory_backend.ttl = 10m
memory_backend.ttl = 3h
```

```appconfig
%% In the app.config-based system, the ttl setting must be expressed in
%% seconds:

{memory_backend, [
    %% other settings
        {ttl, 86400}, %% Set to 1 day
    %% other settings
    ]}
```

> **Dynamically Changing `ttl`**
>
> There is currently no way to dynamically change the `ttl` setting for a
bucket or bucket type. The current workaround would be to define
multiple Memory backends using the Multi backend, each with different
`ttl` values. For more information, consult the documentation on the
[Multi][plan backend multi] backend.
