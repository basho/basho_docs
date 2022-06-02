---
tile_supertext: "Configuring:"
title: "V3 Multi-Datacenter Replication"
description: ""
project: "riak_kv"
project_version: "2.0.4"
menu:
  riak_kv-2.0.4:
    name: "V3 Multi-Datacenter"
    identifier: "configuring_v3"
    weight: 109
    parent: "configuring"
toc: true
commercial_offering: true
aliases:
  - /riak/2.0.4/ops/mdc/v3/configuration
  - /riak/kv/2.0.4/ops/mdc/v3/configuration
---

[config reference#advanced]: {{<baseurl>}}riak/kv/2.0.4/configuring/reference/#advanced-configuration
[config v3 ssl#verify-peer]: {{<baseurl>}}riak/kv/2.0.4/configuring/v3-multi-datacenter/ssl/#verifying-peer-certificates

> **Note on the `cluster_mgr` setting**
>
> The `cluster_mgr` setting _must_ be set in order for version 3 replication to run.


The configuration for Multi-Datacenter (MDC) Replication is kept in
both the `riak_core` and `riak_repl` sections of the `app.config`
configuration file. 

If you are using Riak Enterprise version 2.0, configuration is managed
using the `advanced.config` files on
each node. The semantics of the `advanced.config` file are similar to
the formerly used `app.config` file. For more information and for a list
of configurable parameters, see our documentation on [Advanced Configuration][config reference#advanced].

Here is a sample of the syntax:

```advancedconfig
{riak_core, [
    %% Every *node* runs one cluster_mgr
    {cluster_mgr, {"0.0.0.0", 9080 }},
    % ...
]},
{riak_repl, [
    %% Pick the correct data_root for your platform
    %% Debian/Centos/RHEL:
    {data_root, "/var/lib/riak/data/riak_repl"},
    %% Solaris:
    %% {data_root, "/opt/riak/data/riak_repl"},
    %% FreeBSD/SmartOS:
    %% {data_root, "/var/db/riak/riak_repl"},
    {max_fssource_cluster, 5},
    {max_fssource_node, 2},
    {max_fssink_node, 2},
    {fullsync_on_connect, false},
    % ...
]}
```

## Settings

Riak MDC configuration is set using the standard Erlang config file
syntax `{Setting, Value}`. For example, if you wished to set
`fullsync_on_connect` to `false`, you would insert this line into the
`riak_repl` section (appending a comma if you have more settings to
follow):

```advancedconfig
{fullsync_on_connect, false}
```

Once your configuration is set, you can verify its correctness by
running the `riak` command-line tool:

```bash
riak chkconfig
```

## riak_repl Settings

Setting | Options | Default | Description
:-------|:--------|:--------|:-----------
`cluster_mgr` | `{ip_address, port}` | **REQUIRED** | The cluster manager will listen for connections from remote clusters on this `ip_address` and `port`. Every node runs one cluster manager, but only the cluster manager running on the `cluster_leader` will service requests. This can change as nodes enter and leave the cluster. The value is a combination of an IP address (**not hostname**) followed by a port number.
`max_fssource_cluster` | `nodes` (integer) | `5` | The hard limit on the number of workers which will participate in the source cluster during a fullsync replication. This means that if one has configured fullsync for two different clusters, both with a `max_fssource_cluster` of 5, 10 fullsync workers can be in progress. Only affects nodes on the source cluster on which this parameter is defined via the configuration file or command line.
`max_fssource_node` | `nodes` (integer) | `1` | Limits the number of fullsync workers that will be running on each individual node in a source cluster. This is a hard limit for all fullsyncs enabled; additional fullsync configurations will not increase the number of fullsync workers allowed to run on any node. Only affects nodes on the source cluster on which this parameter is defined via the configuration file or command line.
`max_fssink_node` | `nodes` (integer) | `1` | Limits the number of fullsync workers allowed to run on each individual node in a sink cluster.  This is a hard limit for all fullsync sources interacting with the sink cluster. Thus, multiple simultaneous source connections to the sink cluster will have to share the sink nodes number of maximum connections. Only affects nodes on the sink cluster on which this parameter is defined via the configuration file or command line.
`fullsync_on_connect` | `true`, `false` | `true` | Whether to initiate a fullsync on initial connection from the secondary cluster
`data_root` | `path` (string) | `data/riak_repl` | Path (relative or absolute) to the working directory for the replication process
`fullsync_interval` | `minutes` (integer) OR `[{sink_cluster, minutes(integer)}, ...]` | `360` | A single integer value representing the duration to wait in minutes between fullsyncs, or a list of `{"clustername", time_in_minutes}` pairs for each sink participating in fullsync replication.
`rtq_overload_threshold` | `length` (integer) | `2000` | The maximum length to which the realtime replication queue can grow before new objects are dropped. Dropped objects will need to be replicated with a fullsync.
`rtq_overload_recover` | `length` (integer) | `1000` | The length to which the realtime replication queue, in an overload mode, must shrink before new objects are replicated again.
`rtq_max_bytes` | `bytes` (integer) | `104857600` | The maximum size to which the realtime replication queue can grow before new objects are dropped. Defaults to 100MB. Dropped objects will need to be replicated with a fullsync.
`proxy_get` | `enabled`, `disabled` | `disabled` | Enable Riak CS `proxy_get` and block filter.
`rt_heartbeat_interval` | `seconds` (integer) | `15` | A full explanation can be found [below](#heartbeat-settings).
`rt_heartbeat_timeout` | `seconds` (integer) | `15` | A full explanation can be found [below](#heartbeat-settings).


## riak_core Settings

Setting | Options | Default | Description
:-------|:--------|:--------|:-----------
`keyfile` | `path` (string) | `undefined` | Fully qualified path to an ssl `.pem` key file
`cacertdir` | `path` (string) | `undefined` | The `cacertdir` is a fully-qualified directory containing all the CA certificates needed to verify the CA chain back to the root
`certfile` | `path` (string) | `undefined` | Fully qualified path to a `.pem` cert file
`ssl_depth` | `depth` (integer) | `1` | Set the depth to check for SSL CA certs. See [1](#f1).
`ssl_enabled` | `true`, `false` | `false` | Enable SSL communications
`peer_common_name_acl` | `cert` (string) | `"*"` | Verify an SSL peer’s certificate common name. You can provide an ACL as a list of common name *patterns*, and you can wildcard the leftmost part of any of the patterns, so `*.basho.com` would match `site3.basho.com` but not `foo.bar.basho.com` or `basho.com`. See [2](#f2).


## Heartbeat Settings

There are two realtime-replication-related settings in the `riak_repl`
section of `advanced.config` related to the periodic "heartbeat" that is sent
from the source to the sink cluster to verify the sink cluster's
liveness. The `rt_heartbeat_interval` setting determines how often the
heartbeat is sent (in seconds). If a heartbeat is sent and a response is
not received, Riak will wait `rt_heartbeat_timeout` seconds before
attempting to re-connect to the sink; if any data is received from the
sink, even if it is not heartbeat data, the timer will be reset. Setting
`rt_heartbeat_interval` to `undefined` will disable the heartbeat.

One of the consequences of lowering the timeout threshold arises when
connections are working properly but are slow to respond (perhaps due to
heavy load). In this case, shortening the timeout means that Riak may
attempt to re-connect more often that it needs to. On the other hand,
lengthening the timeout will make Riak less sensitive to cases in which
the connection really has been compromised.

1. <a name="f1"></a>SSL depth is the maximum number of non-self-issued
 intermediate certificates that may follow the peer certificate in a valid
 certificate chain. If depth is `0`, the PEER must be signed by the trusted
 ROOT-CA directly; if `1` the path can be PEER, CA, ROOT-CA; if depth is `2`
 then PEER, CA, CA, ROOT-CA and so on.

2. <a name="f2"></a>If the ACL is specified and not the special value `*`,
  peers presenting certificates not matching any of the patterns will not be
  allowed to connect.
  If no ACLs are configured, no checks on the common name are done, except
  as described for [Identical Local and Peer Common Names][config v3 ssl#verify-peer].
