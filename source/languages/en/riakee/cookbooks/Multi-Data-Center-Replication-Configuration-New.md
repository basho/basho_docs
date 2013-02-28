---
title: "Multi Data Center Replication: Configuration (Advanced)"
project: riakee
version: 1.3.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, configuration]
---


## Advanced Replication Mode Configuration

*The `cluster_mgr` variable MUST be set in order for 1.3 Replication to run*

The configuration for replication is kept in the both the `riak_core` and `riak_repl` sections of `etc/app.config`.

```
{riak_core, [	
    %% Every *node* runs one cluster_mgr.
    {cluster_mgr, {"0.0.0.0", 9085 }}
    …
]},		
{riak_repl, [
    {max_fssource_cluster, 5},
    {max_fssource_node, 2},
    {max_fssink_node, 2},
    {fullsync_on_connect, false}
]}
```

### Settings

These settings are configured using the standard erlang config file syntax `{Setting, Value}`. For example, if you wished to set `fullsync_on_connect` to `false`, you would insert this line to the `riak_repl` section (appending a comma if you have more settings to follow):

```
{fullsync_on_connect, false}
```

Once your configuration is set, you can verify its correctness by running the command-line tool:

```
riak chkconfig
```

Setting | Options | Default | Description
--------|---------|---------|------------
cluster_mgr | {ip_address, port} | REQUIRED | The cluster manager will listen for connections from remote clusters on this ip and port. Every node runs one CM. The value is a combination of an IP address (**not hostname**) followed by a port number
max_fssource_cluster | nodes(integer) | 5 | The hard limit of fullsync workers that will be running on the source side of a cluster across all nodes on that cluster for a fullsync to a sink cluster. This means if one has configured fullsync for two different clusters, both with a max_fssource_cluster of 5, 10 fullsync workers can be in progress. Only affects nodes on the source cluster on which this parameter is defined via the configuration file or command line
max_fssource_node | nodes(integer) | 1 | Limits the number of fullsync workers that will be running on each individual node in a source cluster.  This is a hard limit for all fullsyncs enabled; additional fullsync configurations will not increase the number of fullsync workers allowed to run on any node. Only affects nodes on the source cluster on which this parameter is defined via the configuration file or command line
max_fssink_node | nodes(integer) | 1 | Limits the number of fullsync workers allowed to run on each individual node in a sink cluster.  This is a hard limit for all fullsync sources interacting with the sink cluster. Thus, multiple simultaneous source connections to the sink cluster will have to share the sink node’s number of maximum connections. Only affects nodes on the sink cluster on which this parameter is defined via the configuration file or command line.
fullsync_on_connect | true, false | true | Whether to initiate a fullsync on initial connection from the secondary cluster
data_root | path(string) | data/<wbr>riak_repl | Path (relative or absolute) to the working directory for the replication process

  
## Default Replication Configuration

The values in this section represent those usable for to configure Default Replication. These configurations are kept in the `riak_repl` section of `etc/app.config`.

```
{riak_repl, [
    {fullsync_on_connect, true},
    {fullsync_interval, 360},
    % Debian/Centos/RHEL:
    {data_root, "/var/lib/riak/data/riak_repl"},
    % Solaris:
    % {data_root, "/opt/riak/data/riak_repl"},
    % FreeBSD/SmartOS:
    % {data_root, "/var/db/riak/riak_repl"},
    {queue_size, 104857600},
    {server_max_pending, 5},
    {client_ack_frequency, 5}
]}
```

### Settings

These settings are configured using the standard erlang config file syntax `{Setting, Value}`. For example, if you wished to set `ssl_enabled` to `true`, you would insert this line to the `riak_repl` section (appending a comma if you have more settings to follow):

```
{ssl_enabled, true}
```

Once your configuration is set, you can verify its correctness by running the command-line tool:

```
riak chkconfig
```

Setting | Options | Default | Description
--------|---------|---------|------------
fullsync_on_connect | true, false | true | Whether to initiate a fullsync on initial connection from the secondary cluster
fullsync_strategies | keylist, syncv1 | [syncv1] | A *list* of full synchronization strategies to be used by replication. Please contact Basho support for more information
fullsync_interval   | mins(integer), disabled | 360 | How often to initiate a full synchronization of data, in minutes. This is measured from the completion of one full-sync operation to the initiation of the next. This setting only applies to the primary cluster (listener). To disable full synchronization, use: `disabled`
keyfile | path(string) | undefined | Fully qualified path to an ssl .pem key file
data_root | path(string) | data/<wbr>riak_repl | Path (relative or absolute) to the working directory for the replication process
cacertdir | path(string) | undefined | The cacertdir is a fully-qualified directory containing all the CA certificates needed to verify the CA chain back to the root
certfile | path(string) | undefined | Fully qualified path to a .pem cert file
queue_size | bytes(integer) | 104857600 (100 MiB) | The size of the replication queue in bytes before the replication leader will drop requests. If requests are dropped, a full_sync will be required. Information about dropped requests is available by using the command `riak-repl status`
server_max_pending | max(integer) | 5 | The maximum number of objects the leader will wait to get an acknowledgement from the remote location before queuing the request
client_ack_frequency | freq(integer) | 5 | The number of requests a leader will handle before sending an acknowledgment to the remote cluster
client_connect_timeout | ms(integer) | 15000 | The number of milliseconds to wait before a client connection timeout occurs
client_retry_timeout | ms(integer) | 30000 | The number of milliseconds to wait before trying to connect after a retry has occured
sndbuf | bytes(integer) | OS dependent | The buffer size for the listener (server) socket measured in bytes
ssl_depth | depth(integer) | 1 | Set the depth to check for SSL CA certs. See <a href="/cookbooks/Multi-Data-Center-Replication-Configuration-New/#f1" class="riakee">1</a>.
ssl_enabled | true, false | false | Enable SSL communications
recbuf | bytes(integer) | OS dependent | The buffer size for the site (client) socket measured in bytes
vnode_gets | true, false | true | If true, repl will do a direct get against the vnode, rather than use a GET finite state machine
shuffle_ring | true, false | true | If true, the ring is shuffled randomly. If false, the ring is traversed in-order. Useful when a sync is restarted to reduce the chance of syncing the same partitions.
diff_batch_size | objects(integer) | 100 | Defines how many fullsync objects to send before waiting for an acknowledgement from the client site
max_get_workers | max(integer) | 100 | The maximum number of get workers spawned for fullsync. Every time a replication difference is found, a GET will be performed to get the actual object to send. See <a href="/cookbooks/Multi-Data-Center-Replication-Configuration-New/#f2" class="riakee">2</a>
max_put_workers | max(integer) | 100 | The maximum number of put workers spawned for fullsync. Every time a replication difference is found, a GET will be performed to get the actual object to send. See <a href="/cookbooks/Multi-Data-Center-Replication-Configuration-New/#f3" class="riakee">3</a>
min_get_workers | min(integer) | 5 | The minimum number of get workers spawned for fullsync. Every time a replication difference is found, a GET will be performed to get the actual object to send. See <a href="/cookbooks/Multi-Data-Center-Replication-Configuration-New/#f2" class="riakee">2</a>
min_put_workers | min(integer) | 5 | The minimum number of put workers spawned for fullsync. Every time a replication difference is found, a GET will be performed to get the actual object to send. See <a href="/cookbooks/Multi-Data-Center-Replication-Configuration-New/#f3" class="riakee">3</a>
peer_common_name_acl | cert(string) | "*" | Verify an SSL peer’s certificate common name. You can provide multiple ACLs as a list of strings, and you can wildcard the leftmost part of the common name, so `*.basho.com` would match `site3.basho.com` but not `foo.bar.basho.com` or `basho.com`. See <a href="/cookbooks/Multi-Data-Center-Replication-Configuration-New/#f4" class="riakee">4</a>


1. <a name="f1"></a>SSL depth is the maximum number of non-self-issued intermediate
  certificates that may follow the peer certificate in a valid certification
  path. If depth is `0` the PEER must be signed by the trusted ROOT-CA
  directly; if `1` the path can be PEER, CA, ROOT-CA; if depth is `2` then
  PEER, CA, CA, ROOT-CA and so on.

2. <a name="f2"></a>Each get worker spawns 2 processes, one for the work, and
  one for the get FSM (an Erlang finite state machine implementation for “get” requests). Be sure you don't run over the maximum number of allowed
  processes in an Erlang VM (check vm.args for a +P property).

3. <a name="f3"></a>Each put worker spawns 2 processes, one for the work, and
  one for the put FSM (an Erlang finite state machine implementation for “put” requests). Be sure you don't run over the maximum number of allowed
  processes in an Erlang VM (check vm.args for a +P property).

4. <a name="f4"></a>If the ACL is specified and not the special value `*`,
  certificates not matching any of the rules will not be allowed to connect.
  If no ACLs are configured, no checks on the common name are done.
