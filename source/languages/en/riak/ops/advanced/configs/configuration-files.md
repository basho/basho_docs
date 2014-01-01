---
title: Configuration Files
project: riak
version: 0.10.0+
document: reference
toc: true
audience: intermediate
keywords: [operator, configuration, config]
---
{{#2.0.0-}}
Riak has two configuration files located in `/etc` if you are using a source
install or in `/etc/riak` if you used a binary install. Those files are `app.config` and `vm.args`.

The `app.config` file is used to set various attributes for the node, such as the backend the node will use to store data. The `vm.args` file is used to pass
parameters to the Erlang node, such as the name or cookie of the Erlang node.

## Configuring Your `app.config`

Riak and the Erlang applications it depends on are configured by settings in the `app.config` file, which looks something like this:

```erlang
[
    {riak_core, [
      {ring_state_dir, "data/ring"}
      %% More riak_core settings...
    ]},
    {riak_kv, [
        {storage_backend, riak_kv_bitcask_backend},
        %% More riak_kv settings...
    ]},
    %% Other application configurations...
].
```

Below is a table listing the configurable parameters in `app.config`:

### Protocol Buffers

Parameter | Description | Default | 
:---------|:------------|:--------|
`pb_backlog` | The maximum length to which the queue of pending *simultaneous* Protocol Buffers connections may grow. If set, it must be an integer >= 0. If you anticipate a larger number of connections than the default being simultaneously initialized, set this number to a higher value accordingly. You should adjust this value to meet your anticipated simultaneous connection demand or if experiencing connection resets. | `5` |
`pb` | A list of IP addresses and ports on which Riak's Protocol Buffers interface should listen. | `{"127.0.0.1", 8087 }` |
`pb_ip` | The IP address that the Protocol Buffers interface will bind to.<br /><br />If not set, the PBC interface will not be started. |
`pb_port` | The port to which the Protocol Buffers interface will bind. | `8087` |

### `riak_core` Settings

Parameter | Description | Default (if applicable) | 
:---------|:------------|:------------------------|
`choose_claim_fun` |  |  |
`cluster_name` |  |  |
`default_bucket_props` | See detailed discussion below in the **Default Bucket Properties** section below |  |
`delayed_start` |  |  |
`disable_http_nagle` | Turns off Nagle's algorithm (aka TCP slow-start) for Protocol Buffers connections. This is equivalent to setting the `TCP_NODELAY` option on the socket. | {{#1.3.0+}}`false`{{/1.3.0+}}{{#1.3.0-}}`true`{{/1.3.0-}}) |
`gossip_interval` |  |  |
`handoff_concurrency` |  |  |
`handoff_port` |  |  |
`handoff_ip` |  |  |
`http` |  |  |
`http_logdir` |  |  |
`https` |  |  |
`legacy_vnode_routing` |  |  |
`platform_data_dir` |  |  |
`ring_state_dir` |  |  |
`ring_creation_size` |  |  |
`ssl` |  |  |
`target_n_val` |  |  |
`vnode_management_timer` |  |  |
`wants_claim_fun` |  |  |
`enable_health_checks` |  |  |
`stat_cache_ttl` |  |  |

### `riak_kv` Settings

Settings, yo

### Default Bucket Properties (`default_bucket_props`)

These are properties used for buckets that have not been explicitly defined (as outlined in the HTTP API). They are useful for setting default bucket behavior such as:

```erlang
{default_bucket_props, [
    {n_val,3},
    {allow_mult,false},
    {last_write_wins,false},
    {precommit, []},
    {postcommit, []},
    {chash_keyfun, {riak_core_util, chash_std_keyfun}},
    {linkfun, {modfun, riak_kv_wm_link_walker, mapreduce_linkfun}}
]}
```


## Configuring Your `vm.args`

Parameters for the Erlang node on which Riak runs are set in the `vm.args` file in the `/etc` directory (or `/etc/riak` with a binary install) of the embedded Erlang node. Most of these settings can be left at their defaults until you are ready to tune performance.


{{/2.0.0-}}

{{#2.0.0+}}
Riak has a `riak.conf` configuration file located in `/etc` if you are using a source install or in `/etc/riak` if you used a binary install.

The `riak.conf` file is used to set a wide variety of attributes for the node, from the backend the node will use to store data to the location of SSL-related files to sibling resolution parameters and beyond.
{{/2.0.0+}}