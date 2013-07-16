---
title: Multi
project: riak
version: 1.0.0+
document: tutorials
toc: true
audience: intermediate
keywords: [backends, planning, multi, leveldb, memory, bitcask]
prev: "[[Memory]]"
up:   "[[Choosing a Backend]]"
interest: false
moved: {
    '1.4.0-': '/tutorials/choosing-a-backend/Multi'
}
---

## Overview

Riak allows you to run multiple backends within a single Riak instance.  This
is very useful for two very different use cases.

  1. You may want to use different backends for different buckets or
  2. You may need to use the same storage engine in different ways for different buckets.

The Multi backend allows you to configure more than one backend at the same time
on a single cluster.

## Installing Multi-Backend Support

Riak ships with Multi backend support included within the distribution so there
is no separate installation required.

## Configuring Multiple Backends

Modify the default behavior by adding these settings in your
[[app.config|Configuration-Files]].  The `multi_backend` configuration must be
within the `riak_kv` section of the `app.config`.

```erlang
%% Riak KV config
{riak_kv, [
    %% ...
    %% Use the Multi Backend
    {storage_backend, riak_kv_multi_backend},
    %% ...
]}
```

Then later anywhere in the `riak_kv` section (but you'll likely want this in the
section with other backend-related information) add a section to configure the
multiple backends.

<div class="info"><div class="title">Organizing Configuration</div><p>While these configuration directives can be placed anywhere within the <tt>riak_kv</tt> section of <tt>app.config</tt>, we recommend that you place them in the section with other backend-related settings to keep the settings organized.</p></div>

```erlang
%% Use bitcask by default
{riak_kv, [
    %% ...
    {multi_backend_default, <<"bitcask_mult">>},
    {multi_backend, [
        %% Here's where you set the individual multiplexed backends
        {<<"bitcask_mult">>,  riak_kv_bitcask_backend, [
                         %% bitcask configuration
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

<div class="note"><div class="title">Multi-Backend Memory Use</div>Each backend
has settings for how much memory the backend can use. It might be for caching,
like in LevelDB, or for the entire set of data, like in the Memory Backend. Each
of these backends suggests allocating up to 50% of available memory for this.
When using Multi Backend, it is important that the sum of all backend memory
use is at 50% or less. Three backends each set to use 50% of available memory
would cause problems.</div>

<div class="note"><div class="title">Multi-Backend settings</div>
Certain settings, such as Bitcask's `merge_window`, are set per-node,
rather than per-backend, and as such must be set in the top level backend
sections of your `app.config`.</div>

Once configured start the Riak cluster.  Riak will use the
`multi_backend_default` for all new bucket storage unless you configure a
bucket to use one of the alternate storage engines.  This can be done using
either the Erlang console or the HTTP interface, both methods simply change the
bucket properties.  Here are two examples:

  - Using the Erlang console
    You can connect to a live node using the Erlang console and directly set
    the bucket properties.

    ```bash
    $ riak attach
    ...
    1> riak_core_bucket:set_bucket(<<"MY_BUCKET">>, [{backend, <<"second_bitcask_mult">>}]).
    ```

  - Using the HTTP REST API
    You can also connect to Riak using the HTTP API and change the bucket
    properties.

    ```
    $ curl -XPUT http://riaknode:8098/buckets/transient_example_bucketname/props \
      -H "Content-Type: application/json" \
      -d '{"props":{"backend":"memory_mult"}}'
    ```

Note that if you change the default bucket storage engine via the app.config
settings, then you will need to restart the node, or nodes, for that change to
take effect.
