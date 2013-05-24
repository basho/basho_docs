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
---

## Overview

The Memory Backend storage engine uses in-memory tables to store all data.
This data is never persisted to disk or any other storage.  The Memory storage
engine is best used for testing Riak clusters or for small amounts of transient
state in production systems.

<div class="note"><div class="title">Memory replaces the Cache backend</div>
<p>The Memory backend is designed to offer you the same functionality as the now
obsolete Cache backend found in pre-1.0 versions of Riak.  The configuration
options for Memory match those of Cache and can be used to make the memory
backend behave similarly to the cache backend.</p>
</div>

## Installing the Memory Backend

Riak ships with the Memory Backend included within the distribution so there is
no separate installation required.

## Enabling and Configuring the Memory Backend

To enable the memory backend, edit
[[app.config|Configuration-Files#app-config]] for each Riak node and modify
the `riak_kv` section by specifying the memory backend as shown in the
following example:

```erlang
{riak_kv, [
           %% Storage_backend specifies the Erlang module defining the storage
           %% mechanism that will be used on this node.
           % {storage_backend, riak_kv_bitcask_backend},
           {storage_backend, riak_kv_memory_backend},

```

Note that if you *replace* the existing specified backend by removing it or
commenting it out as shown in the above example, data belonging to the
previously specified backend will still preserved on the filesystem, but will
no longer be accessible through Riak unless the backend is enabled again.

If you require multiple backends in your configuration, please consult the
[[multi backend documentation|Multi]].

You can modify the default memory backend behavior by adding the following
settings into the `memory_backend` section in your
[[app.config|Configuration-Files#app-config]].

### Max Memory

  The amount of memory in megabytes to limit the backend to per vnode. An instance
  of the memory backend is running per vnode on each physical node. Use the
  recommendations in [[LevelDB cache_size|LevelDB#Cache-Size]] in determining this.

```erlang
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

## Memory Backend Implementation Details

This backend uses the Erlang `ets` tables internally to manage data.
