---
title: "Configuring Riak CS Multi-Datacenter"
description: ""
menu:
  riak_cs-2.1.0:
    name: "Configuring"
    identifier: "mdc_config"
    weight: 100
    parent: "mdc_overview"
project: "riak_cs"
project_version: "2.1.0"
lastmod: 2015-10-15T00:00:00-00:00
sitemap:
  priority: 0.2
aliases:
  - /riakcs/2.1.0/cookbooks/configuration/Configuring-MDC/
  - /riak/cs/2.1.0/cookbooks/configuration/Configuring-MDC/
---

{{% note title="Riak CS Enterprise requires a separate download" %}}
Please note that Riak CS Enterprise requires a download separate from the
open-source Riak CS, which will not work in conjunction with Riak Enterprise.
{{% /note %}}

Configuring Multi-Datacenter Replication in Riak CS requires the
addition of a new group of settings to the `app.config` configuration
file for all Riak CS and Riak Enterprise nodes that are part of the Riak
CS cluster.

## Riak Enterprise Configuration

As of Riak release 1.4.0, there are two different MDC replication modes
that Riak CS can use to request data from remote clusters. Please see
the [comparison]({{<baseurl>}}riak/kv/2.1.3/using/reference/multi-datacenter/comparison) doc for more information.

### Replication Version 3 Configuration

For each Riak node in the cluster, update the `mdc.proxy_get` setting in
`riak.conf`, or by appending the `{proxy_get, enabled}` setting to the
`riak_repl` section of the old-style `advanced.config` or `app.config` files,
 as shown in the following example:

```riakconf
mdc.proxy_get = on
```

```advancedconfig
{riak_repl, [
             %% Other configs
             {fullsync_on_connect, true},
             {fullsync_interval, 360},
             {data_root, "/var/lib/riak/data/riak_repl"},
             {proxy_get, enabled}
             %% Other configs
            ]}
```

```appconfig
{riak_repl, [
             %% Other configs
             {fullsync_on_connect, true},
             {fullsync_interval, 360},
             {data_root, "/var/lib/riak/data/riak_repl"},
             {proxy_get, enabled}
             %% Other configs
            ]}
```

Version 3 replication requires additional configuration in the **source
cluster** via the command line.

```bash
riak-repl proxy_get enable <sink_cluster_name>
```

The `sink_cluster_name` should be replaced with the name of your
configured **sink cluster**.

See also:

<!-- * [Upgrading from v2 to v3]({{<baseurl>}}riak/kv/2.1.3/setup/upgrading/multi-datacenter) -->
* [Comparing v2 and v3]({{<baseurl>}}riak/kv/2.1.3/using/reference/multi-datacenter/comparison)
* [Multi-Datacenter Operations]({{<baseurl>}}riak/kv/2.1.3/using/cluster-operations/v3-multi-datacenter)

## Riak CS Configuration

For each Riak CS node in the cluster, update the `riak_cs` section of the
`advanced.config`, or the old-style `app.config` files, by appending the
`proxy_get` setting as shown in the following example:

```advancedconfig
{riak_cs, [
           %% Other configs
           {proxy_get, enabled},
           %% Other configs
          ]}
```

```appconfig
{riak_cs, [
           %% Other configs
           {proxy_get, enabled},
           %% Other configs
          ]}
```

<div class ="note">
<div class="title">Note on restarting Riak nodes</div>
Be sure that you restart cluster nodes in a rolling fashion after making
configuration changes. In particular, after restarting a node, be sure
that you wait for Riak's key/value store to become available before
restarting the next node. To check the status of `riak_kv` on a node
after restarting, execute the following command:

```bash
riak-admin wait-for-service riak_kv <nodename>
```

Replace the `node` variable above with the nodename specified in the
`vm.args` configuration file.
</div>

## Stanchion Configuration

Though there is no specific configuration for [Stanchion]({{<baseurl>}}riak/cs/2.1.0/theory/stanchion), note that
Stanchion should be a single, globally unique process to which every
Riak CS node sends requests, even if there are multiple replicated
sites.  Unlike Riak and Riak CS, Stanchion should run on _only one node
in a given cluster_, perhaps on its own, dedicated hardware if you wish.
Stanchion runs on only one node because it manages strongly consistent
updates to [globally unique entities]({{<baseurl>}}riak/cs/2.1.0/theory/stanchion/#globally-unique-entities) like users and buckets.
