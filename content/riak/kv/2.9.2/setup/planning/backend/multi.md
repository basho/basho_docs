---
title: "Multi-backend"
description: ""
project: "riak_kv"
project_version: 2.9.2
menu:
  riak_kv-2.9.2:
    name: "Multi-backend"
    identifier: "planning_backend_multi"
    weight: 103
    parent: "planning_choose_backend"
toc: true
aliases:
  - /riak/2.9.2/ops/advanced/backends/multi/
  - /riak/kv/2.9.2/ops/advanced/backends/multi/
---

[concept buckets]: {{<baseurl>}}riak/kv/2.9.2/learn/concepts/buckets
[plan backend bitcask]: {{<baseurl>}}riak/kv/2.9.2/setup/planning/backend/bitcask
[plan backend leveldb]: {{<baseurl>}}riak/kv/2.9.2/setup/planning/backend/leveldb
[plan backend memory]: {{<baseurl>}}riak/kv/2.9.2/setup/planning/backend/memory
[config reference]: {{<baseurl>}}riak/kv/2.9.2/configuring/reference
[usage bucket types]: {{<baseurl>}}riak/kv/2.9.2/developing/usage/bucket-types
[use admin riak-admin cli]: {{<baseurl>}}riak/kv/2.9.2/using/admin/riak-admin

Riak allows you to run multiple backends within a single Riak cluster.
Selecting the Multi backend enables you to use different storage
backends for different [buckets][concept buckets]. Any combination of the three
available backends---[Bitcask][plan backend bitcask], [LevelDB][plan backend leveldb], and [Memory][plan backend memory]---can be used.

## Configuring Multiple Backends

You can set up your cluster to use the Multi backend using Riak's
[configuration files][config reference].

```riakconf
storage_backend = multi
```

```appconfig
{riak_kv, [
    %% ...
    {storage_backend, riak_kv_multi_backend},
    %% ...
]},
```

Remember that you must stop and then re-start each node when you change
storage backends or modify any other configuration.

## Using Multiple Backends

In Riak 2.0 and later, we recommend using multiple backends by applying
them to buckets [using bucket types][usage bucket types]. Assuming that the cluster has already been configured to use the `multi` backend, this process
involves three steps:

1. Creating a bucket type that enables buckets of that type to use the
   desired backends
2. Activating that bucket type
3. Setting up your application to use that type

Let's say that we've set up our cluster to use the Multi backend and we
want to use [LevelDB][plan backend leveldb] and the [Memory][plan backend memory] backend for different sets of data. First, we need to create two bucket types, one which sets the `backend` bucket property to `leveldb` and the other which sets that property to `memory`. All bucket type-related activity is performed through the [`riak-admin`][use admin riak-admin cli] command interface.

We'll call our bucket types `leveldb_backend` and `memory_backend`, but
you can use whichever names you wish.

```bash
riak-admin bucket-type create leveldb_backend '{"props":{"backend":"leveldb"}}'
riak-admin bucket-type create memory_backend '{"props":{"backend":"memory"}}'
```

Then, we must activate those bucket types so that they can be used in
our cluster:

```bash
riak-admin bucket-type activate leveldb_backend
riak-admin bucket-type activate memory_backend
```

Once those types have been activated, any objects stored in buckets
bearing the type `leveldb_backend` will be stored in LevelDB, whereas
all objects stored in buckets of the type `memory_backend` will be
stored in the Memory backend.

More information can be found in our documentation on [using bucket types][usage bucket types].

## Configuring Multiple Backends

Once you've set up your cluster to use multiple backends, you can
configure each backend on its own. All configuration options available
for LevelDB, Bitcask, and Memory are all available to you when using the
Multi backend.

#### Using the Newer Configuration System

If you are using the newer, `riak.conf`-based [configuration system][config reference], you can configure the backends by
prefacing each configuration with `multi_backend`.

Here is an example of the general form for configuring multiple
backends:

```riakconf
multi_backend.$name.$setting_name = setting
```

If you are using, for example, the LevelDB and Bitcask backends and wish
to set LevelDB's `bloomfilter` setting to `off` and the Bitcask
backend's `io_mode` setting to `nif`, you would do that as follows:

```riakconf
multi_backend.leveldb.bloomfilter = off
multi_backend.bitcask.io_mode = nif
```

#### Using the Older Configuration System

If you are using the older, `app.config`-based configuration system,
configuring multiple backends involves adding one or more backend-
specific sections to your `riak_kv` settings (in addition to setting
the `storage_backend` setting to `riak_kv_multi_backend`, as shown
above).

> **Note**: If you are defining multiple file-based backends of the same
type, each of these must have a separate `data_root` directory defined.

While all configuration parameters can be placed anywhere within the
`riak_kv` section of `app.config`, in general we recommend that you
place them in the section containing other backend-related settings to
keep the settings organized.

Below is the general form for your `app.config` file:

```appconfig
{riak_kv, [
    %% ...
    {multi_backend_default, <<"bitcask_mult">>},
    {multi_backend, [
        %% Here's where you set the individual multiplexed backends
        {<<"bitcask_mult">>,  riak_kv_bitcask_backend, [
                         %% bitcask configuration
                         {data_root, "/var/lib/riak/bitcask_mult/"},
                         {config1, ConfigValue1},
                         {config2, ConfigValue2}
        ]},
        {<<"bitcask_expiry_mult">>,  riak_kv_bitcask_backend, [
                         %% bitcask configuration
                         {data_root, "/var/lib/riak/bitcask_expiry_mult/"},
                         {expiry_secs, 86400},
                         {config1, ConfigValue1},
                         {config2, ConfigValue2}
        ]},
        {<<"eleveldb_mult">>, riak_kv_eleveldb_backend, [
                         %% eleveldb configuration
                         {config1, ConfigValue1},
                         {config2, ConfigValue2}
        ]},
        {<<"second_eleveldb_mult">>,  riak_kv_eleveldb_backend, [
                         %% eleveldb with a different configuration
                         {config1, ConfigValue1},
                         {config2, ConfigValue2}
        ]},
        {<<"memory_mult">>,   riak_kv_memory_backend, [
                         %% memory configuration
                         {config1, ConfigValue1},
                         {config2, ConfigValue2}
        ]}
    ]},
    %% ...
]},
```

Note that in each of the subsections of the `multi_backend` setting, the
name of each backend you wish to configure can be anything you would
like. Directly after naming the backend, you must specify which of the
backends corresponds to that name, i.e.  `riak_kv_bitcask_backend`,
`riak_kv_eleveldb_backend`, or `riak_kv_memory_backend`. Once you have
done that, the various configurations for each named backend can be set
as objects in an Erlang list.

## Example Configuration

Imagine that you are using both Bitcask and LevelDB in your cluster, and
you would like storage to default to Bitcask. The following
configuration would create two backend configurations, named
`bitcask_mult` and `leveldb_mult`, respectively, while also setting the
data directory for each backend and specifying that `bitcask_mult` is
the default.

```riakconf
storage_backend = multi

multi_backend.bitcask_mult.storage_backend = bitcask
multi_backend.bitcask_mult.bitcask.data_root = /var/lib/riak/bitcask_mult

multi_backend.leveldb_mult.storage_backend = leveldb
multi_backend.leveldb_mult.leveldb.data_root = /var/lib/riak/leveldb_mult

multi_backend.default = bitcask_mult
```

```appconfig
{riak_kv, [
    %% ...
    {multi_backend_default, <<"bitcask_mult">>},
    {multi_backend, [
        {<<"bitcask_mult", riak_kv_bitcask_backend, [
            {data_root, "/var/lib/riak/bitcask"}
        ]},
        {<<"leveldb_mult", riak_kv_eleveldb_backend, [
            {data_root, "/var/lib/riak/leveldb"}
        ]}
    ]}
    %% ...
]}
```

## Multi Backend Memory Use

Each Riak storage backend has settings for configuring how much memory
the backend can use, e.g. caching for LevelDB or for the entire set of
data for the Memory backend. Each of these backends suggests allocating
up to 50% of available memory for this purpose. When using the Multi
backend, make sure that the sum of all backend memory use is at 50%
or less. For example, using three backends with each set to 50% memory
usage will inevitably lead to memory problems.
