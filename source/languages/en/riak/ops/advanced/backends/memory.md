---
title: Memory
project: riak
version: 1.0.0+
document: tutorials
toc: true
audience: intermediate
keywords: [backends, planning, memory]
prev: "[[LevelDB]]"
up:   "[[Choosing a Backend]]"
next: "[[Multi]]"
interest: false
moved: {
    '1.4.0-': '/tutorials/choosing-a-backend/Memory'
}
---

The Memory storage backend uses in-memory tables to store all data.
This data is never persisted to disk or to any other storage mechanism.
The Memory storage engine is best used for testing Riak clusters or for
storing small amounts of transient state in production systems.

Internally, the Memory backend uses Erlang Ets tables to manage data.
More information can be found in the
[official Erlang documentation](http://www.erlang.org/doc/man/ets.html).

## Enabling the Memory Backend

To enable the memory backend, edit your [[configuration files]] for each
Riak node and specify the Memory backend as shown in the following
example:

```riakconf
storage_backend = memory
```

```erlang
{riak_kv, [
    ...
    {storage_backend, riak_kv_memory_backend},
    ...
]}
```

**Note**: If you *replace* the existing specified backend by removing it
or commenting it out as shown in the above example, data belonging to
the previously specified backend will still preserved on the filesystem,
but will no longer be accessible through Riak unless the backend is
enabled again.

If you require multiple backends in your configuration, please consult
the [[Multi backend documentation|Multi]].

## Configuring the Memory Backend

The Memory backend enables you to configure two fundamental aspects of
object storage: object expiry and maximum memory usage per
[[vnode|Riak Glossary#vnode]].

### Max Memory


If you are using the older, `app.config`-based configuration system, you
can modify the default memory backend behavior by adding a `memory_backend`
subsection to the `riak_kv` section of each node's [[app.config|Configuration Files#app-config]]
using the following settings.


  The amount of memory in megabytes to limit the backend to per vnode. An instance
  of the memory backend is running per vnode on each physical node. Use the
  recommendations in [[LevelDB cache_size|LevelDB#Cache-Size]] in determining this.

```erlang
{riak_kv, [
          %% Storage_backend specifies the Erlang module defining the storage
          %% mechanism that will be used on this node.
          % {storage_backend, riak_kv_bitcask_backend},
          {storage_backend, riak_kv_memory_backend},
          {memory_backend, [
              ...,
                  {max_memory, 4096}, %% 4GB in megabytes
              ...
          ]}
```


### TTL

  The time in seconds before an object expires.

```erlang
{memory_backend, [
        ...,
            {ttl, 86400}, %% 1 Day in seconds
        ...
]}
```

<div class="note"><div class="title">Dynamically Changing ttl</div>
<p>There is currently no way to dynamically change the ttl per bucket. The
current work around would be to define multiple "riak_kv_memory_backends" under
"riak_kv_multi_backend" with different ttl values. For more details read about
the [[Multi Backend|Multi]].</p>
</div>
