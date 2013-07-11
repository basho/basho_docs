---
title: "Multi Data Center Replication v3 Configuration"
project: riakee
version: 1.3.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, configuration]
---


## Version 3 Replication Configuration

*The `cluster_mgr` variable MUST be set in order for 1.3 Replication to run*

The configuration for replication is kept in the both the `riak_core` and `riak_repl` sections of `etc/app.config`.

```
{riak_core, [
    %% Every *node* runs one cluster_mgr.
    {cluster_mgr, {"0.0.0.0", 9085 }}
    …
]},
{riak_repl, [
    % Pick the correct data_root for your platform
        % Debian/Centos/RHEL:
    {data_root, "/var/lib/riak/data/riak_repl"},
    % Solaris:
    % {data_root, "/opt/riak/data/riak_repl"},
    % FreeBSD/SmartOS:
    % {data_root, "/var/db/riak/riak_repl"},
    {max_fssource_cluster, 5},
    {max_fssource_node, 2},
    {max_fssink_node, 2},
    {fullsync_on_connect, false}
]}
```
* **Note**: One or more percent signs `%` in `app.config` begins a comment that continues to end of line.

### Settings

These settings are configured using the standard erlang config file syntax `{Setting, Value}`. For example, if you wished to set `fullsync_on_connect` to `false`, you would insert this line to the `riak_repl` section (appending a comma if you have more settings to follow):

```
{fullsync_on_connect, false}
```

Once your configuration is set, you can verify its correctness by running the command-line tool:

```
riak chkconfig
```

---
Riak MDC Replication app.config settings, **riak_repl section**

Setting | Options | Default | Description
--------|---------|---------|------------
cluster_mgr | {ip_address, port} | REQUIRED | The cluster manager will listen for connections from remote clusters on this ip and port. Every node runs one CM. The value is a combination of an IP address (**not hostname**) followed by a port number
max_fssource_cluster | nodes(integer) | 5 | The hard limit of fullsync workers that will be running on the source side of a cluster across all nodes on that cluster for a fullsync to a sink cluster. This means if one has configured fullsync for two different clusters, both with a max_fssource_cluster of 5, 10 fullsync workers can be in progress. Only affects nodes on the source cluster on which this parameter is defined via the configuration file or command line
max_fssource_node | nodes(integer) | 1 | Limits the number of fullsync workers that will be running on each individual node in a source cluster.  This is a hard limit for all fullsyncs enabled; additional fullsync configurations will not increase the number of fullsync workers allowed to run on any node. Only affects nodes on the source cluster on which this parameter is defined via the configuration file or command line
max_fssink_node | nodes(integer) | 1 | Limits the number of fullsync workers allowed to run on each individual node in a sink cluster.  This is a hard limit for all fullsync sources interacting with the sink cluster. Thus, multiple simultaneous source connections to the sink cluster will have to share the sink node’s number of maximum connections. Only affects nodes on the sink cluster on which this parameter is defined via the configuration file or command line.
fullsync_on_connect | true, false | true | Whether to initiate a fullsync on initial connection from the secondary cluster
data_root | path(string) | data/<wbr>riak_repl | Path (relative or absolute) to the working directory for the replication process
fullsync_interval | minutes(integer) OR [{sink_cluster, minutes(integer)}, …]|undefined|a single integer value representing the duration to wait in minutes between fullsyncs, or a list of {"clustername", time_in_minutes} pairs for each sink participating in fullsync replication.
proxy_get|enabled,disabled|false|Enable Riak CS proxy_get and block filter.
rt_heartbeat_interval|seconds(integer)|15|A heartbeat message is sent from the source to the sink every `rt_heartbeat_interval` seconds. This feature is only available in Riak Enterprise 1.3.2+.
rt_heartbeat_timeout|seconds(integer)|15|If a heartbeat response is not received in `rt_heartbeat_timeout` seconds, then the source connection exits and will be re-established. This feature is only available in Riak Enterprise 1.3.2+.


---

Riak MDC Replication app.config settings, **riak_core section**

Setting | Options | Default | Description
--------|---------|---------|------------
keyfile | path(string) | undefined | Fully qualified path to an ssl .pem key file
cacertdir | path(string) | undefined | The cacertdir is a fully-qualified directory containing all the CA certificates needed to verify the CA chain back to the root
certfile | path(string) | undefined | Fully qualified path to a .pem cert file
ssl_depth | depth(integer) | 1 | Set the depth to check for SSL CA certs. See <a href="/cookbooks/Multi-Data-Center-Replication-Configuration/#f1" class="riakee">1</a>.
ssl_enabled | true, false | false | Enable SSL communications
peer_common_name_acl | cert(string) | "*" | Verify an SSL peer’s certificate common name. You can provide multiple ACLs as a list of strings, and you can wildcard the leftmost part of the common name, so `*.basho.com` would match `site3.basho.com` but not `foo.bar.basho.com` or `basho.com`. See <a href="/cookbooks/Multi-Data-Center-Replication-Configuration/#f4" class="riakee">2</a>.


1. <a name="f1"></a>SSL depth is the maximum number of non-self-issued intermediate
  certificates that may follow the peer certificate in a valid certification
  path. If depth is `0` the PEER must be signed by the trusted ROOT-CA
  directly; if `1` the path can be PEER, CA, ROOT-CA; if depth is `2` then
  PEER, CA, CA, ROOT-CA and so on.

2. <a name="f4"></a>If the ACL is specified and not the special value `*`,
  certificates not matching any of the rules will not be allowed to connect.
  If no ACLs are configured, no checks on the common name are done.
