---
title: Configuring Riak CS Multi Data Center
project: riakcs
version: 1.3.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator]
---

Configuration of Multi-Datacenter Replication in Riak CS requires the addition of a new setting to the `app.config` for all Riak CS and Riak Enterprise nodes which are part of the Riak CS cluster.

## Riak Enterprise Configuration

{{#1.4.0+}}
As of Riak release 1.4.0, there are two different MDC replication modes that Riak CS can use to request data from remote clusters. Please see the [[Multi Data Center Replication: Comparison]] doc for more information.

### Replication version 3 configuration

For each Riak node in the cluster, update the `riak_repl` section of `app.config` by appending the `{proxy_get, enabled}` setting as shown in the following example.

```erlang
{riak_repl, [
             {fullsync_on_connect, true},
             {fullsync_interval, 360},
             {data_root, "/var/lib/riak/data/riak_repl"},
             {proxy_get, enabled}
            ]}
```

Version 3 replication requires additional configuration on the **source cluster** via the command line. 

```bash
riak-repl proxy_get enable sink_cluster_name
```

`sink_cluster_name` should be replaced with the name of your configured **sink cluster**. 

See also:

[[Multi Data Center Replication: Upgrading from V2 to V3]]
[[Multi Data Center Replication: Comparison]]
[[Multi Data Center Replication v3 Operations]]

### Replication version 2 configuration
{{/1.4.0+}}
For each Riak node in the cluster, update the `riak_repl` section of `app.config` by appending the `{proxy_get, enabled}` setting as shown in the following example.

```erlang
{riak_repl, [
             {fullsync_on_connect, true},
             {fullsync_interval, 360},
             {data_root, "/var/lib/riak/data/riak_repl"},
             {queue_size, 104857600},
             {server_max_pending, 5},
             {client_ack_frequency, 5},
             {proxy_get, enabled}
            ]}
```

## Riak CS Configuration

For each Riak CS node in the cluster, update the `riak_cs` section of the `app.config` by appending the `{proxy_get, enabled}` setting as shown in the following example.

```erlang
%% Riak CS config
{riak_cs, [
             {cs_ip, "127.0.0.1"},
             {cs_port, 8080 },
             {proxy_get, enabled},
             %% ...
            ]}
```

<div class ="note"><div class="title">Note on restarting Riak nodes</div>
<p>Be sure that you restart cluster nodes in a rolling fashion after making
configuration changes. In particular, after restarting a node, be sure that you wait for Riak's key/value store to become available before restarting the next node. To check the status of <tt>riak_kv</tt> on a node after restarting, execute the following command:</p>
<p><tt>riak-admin wait-for-service riak_kv &lt;target_node&gt;</tt></p>
<p>where &lt;target-node&gt; is the node name of the node based on the
<tt>-name</tt> setting in <tt>vm.args</tt>.</p></div>
