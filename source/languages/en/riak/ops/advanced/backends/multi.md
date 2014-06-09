---
title: Multi
project: riak
version: 1.0.0+
document: tutorials
audience: intermediate
keywords: [backends, planning, multi, leveldb, memory, bitcask]
prev: "[[Memory]]"
up:   "[[Choosing a Backend]]"
interest: false
moved: {
    '1.4.0-': '/tutorials/choosing-a-backend/Multi'
}
---

Riak allows you to run multiple backends within a single Riak cluster.
Selecting the Multi backend for your cluster enables you to use
different storage backends for different buckets. Any combination of
the three available backends--[[Bitcask]], [[LevelDB]], and [[Memory]]--
can be used.

## Configuring Multiple Backends

You can set up your cluster to use the Multi backend using Riak's
[[configuration files]].

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
them to buckets [[using bucket types]]. This process involves three
steps:

1. Creating bucket types that define


## Configuring Multiple Backends

Once you've set up your cluster to use multiple backends, you can
configure each backend on its own. All configuration options available
for LevelDB, Bitcask, and Memory are all available to you when using the
Multi backend.

#### Using the Newer Configuration System

If you are using the newer,
`riak.conf`-based [[configuration system|Configuration Files]], you can 
configure the backends by prefacing each configuration with `multi_backend`.

If you are using, for example, the LevelDB and Bitcask backends and wish
to set LevelDB's `bloomfilter` setting to `off` and the Bitcask
backend's `io_mode` setting to `nif`, you would do that as follows:

```riakconf
multi_backend.leveldb.bloomfilter = off
multi_backend.bitcask.io_mode = nif
```

Below is the more general form for configuring multiple backends:

```riakconf
multi_backend.$name.$setting_name = setting
```

#### Using the Older Configuration System

If you are using the older, `app.config`-based configuration system,
configuring multiple backends involves adding one or more backend-
specific sections to your `riak_kv` settings (in addition to setting
the `storage_backend` setting to `riak_kv_multi_backend`, as shown
above).

**Note**: If you are defining multiple file-based backends of the same
type, each of these must have a separate `data_root` defined.

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

Note that in each of the subsections of the `multi_backend` setting,
the name of each backend you wish to configure can be anything you would
like as long as it ends in `_mult`. Directly after naming the backend,
you must specify which of the backends corresponds to that name, i.e.
`riak_kv_bitcask_backend`, `riak_kv_eleveldb_backend`, or
`riak_kv_memory_backend`. Once you have done that, the various
configurations for each named backend can be set as objects in an Erlang
list.

## Multi Backend Memory Use

Each Riak storage backend has settings for configuring how much memory
the backend can use, e.g. caching for LevelDB or for the entire set of
data for the Memory backend. Each of these backends suggests allocating
up to 50% of available memory for this purpose. When using the Multi
backend, make sure that the sum of all backend memory use is at 50% 
or less. Using, for example, three backends with each set to 50% memory
usage will inevitable lead to memory problems.

## Usage Example
