---
title: "Basic Riak KV Configuration"
description: ""
project: "riak_kv"
project_version: 2.9.4
menu:
  riak_kv-2.9.4:
    name: "Basic Configuration"
    identifier: "configuring_basic"
    weight: 100
    parent: "configuring"
toc: true
aliases:
  - /riak/2.9.4/ops/building/configuration/
  - /riak/kv/2.9.4/ops/building/configuration/
---

[config reference]: {{<baseurl>}}riak/kv/2.9.4/configuring/reference
[use running cluster]: {{<baseurl>}}riak/kv/2.9.4/using/running-a-cluster
[use admin riak-admin#member-status]: {{<baseurl>}}riak/kv/2.9.4/using/admin/riak-admin/#member-status
[perf erlang]: {{<baseurl>}}riak/kv/2.9.4/using/performance/erlang
[plan start]: {{<baseurl>}}riak/kv/2.9.4/setup/planning/start
[plan best practices]: {{<baseurl>}}riak/kv/2.9.4/setup/planning/best-practices
[cluster ops backup]: {{<baseurl>}}riak/kv/2.9.4/using/cluster-operations/backing-up
[cluster ops add remove node]: {{<baseurl>}}riak/kv/2.9.4/using/cluster-operations/adding-removing-nodes
[plan backend]: {{<baseurl>}}riak/kv/2.9.4/setup/planning/backend
[plan backend multi]: {{<baseurl>}}riak/kv/2.9.4/setup/planning/backend/multi
[plan backend bitcask]: {{<baseurl>}}riak/kv/2.9.4/setup/planning/backend/bitcask
[usage bucket types]: {{<baseurl>}}riak/kv/2.9.4/developing/usage/bucket-types
[apps replication properties]: {{<baseurl>}}riak/kv/2.9.4/developing/app-guide/replication-properties
[concept buckets]: {{<baseurl>}}riak/kv/2.9.4/learn/concepts/buckets
[concept eventual consistency]: {{<baseurl>}}riak/kv/2.9.4/learn/concepts/eventual-consistency
[perf benchmark]: {{<baseurl>}}riak/kv/2.9.4/using/performance/benchmarking
[perf open files]: {{<baseurl>}}riak/kv/2.9.4/using/performance/open-files-limit
[perf index]: {{<baseurl>}}riak/kv/2.9.4/using/performance
[perf aws]: {{<baseurl>}}riak/kv/2.9.4/using/performance/amazon-web-services
[Cluster Capacity Planning]: {{<baseurl>}}riak/kv/2.9.4/setup/planning/cluster-capacity/#ring-size-number-of-partitions

This document covers the parameters that are commonly adjusted when
setting up a new cluster. We recommend that you also review the detailed
[Configuration Files][config reference] document before moving a cluster into
production.

All configuration values discussed here are managed via the
configuration file on each node, and a node must be restarted for any
changes to take effect.

> **Note**
>
> If you are upgrading to Riak KV version 2.0 or later from an pre-2.0
release, you can use either your old `app.config` configuration file or
the newer `riak.conf` if you wish.
>
> If you have installed Riak KV 2.0 directly, you should use only
`riak.conf`.
>
> More on configuring Riak KV can be found in the [configuration files][config reference]
doc.

We advise that you make as many of the changes below as practical
_before_ joining the nodes together into a cluster. Once your
configuration has been set on each node, follow the steps in [Basic Cluster Setup][use running cluster] to complete the clustering process.

Use [`riak-admin member-status`][use admin riak-admin#member-status]
to determine whether any given node is a member of a cluster.

## Erlang VM Tunings

Prior to building and starting a cluster, there are some
Erlang-VM-related changes that you should make to your configuration
files. If you are using the older, `vm.args`-based Erlang VM tunings,
you should set the following:

```vmargs
+sfwi 500
+scl false
```

If you are using the newer, `riak.conf`-based configuration system, we
recommend the following settings:

```riakconf
erlang.schedulers.force_wakeup_interval = 500
erlang.schedulers.compaction_of_load = false
```

More information can be found in [Erlang VM Tuning][perf erlang].

## Ring Size

The ring size, in Riak parlance, is the number of data partitions that
comprise the cluster. This quantity impacts the scalability and
performance of a cluster and, importantly, **it should be established
before the cluster starts receiving data**.

If the ring size is too large for the number of servers, disk I/O will
be negatively impacted by the excessive number of concurrent databases
running on each server; if the ring size is too small, the servers' other
resources (primarily CPU and RAM) will go underutilized.

See [Cluster Capacity Planning] for more details on choosing a ring size.

The steps involved in changing the ring size depend on whether the
servers (nodes) in the cluster have already been joined together.

### Cluster joined, but no data needs to be preserved

1.  Change the ring creation size parameter by uncommenting it and then
setting it to the desired value, for example 64:

    ```riakconf
    ring_size = 64
    ```

    ```appconfig
    %% In the riak_core section:
    {ring_creation_size, 64}
    ```

2.  Stop all nodes
3.  Remove the ring data file on each node (see [Backing up Riak][cluster ops backup] for the location of this file) 
4.  Start all nodes
5.  Re-add each node to the cluster (see [Adding and Removing Nodes][cluster ops add remove node]) or finish reviewing this document and proceed to [Basic Cluster Setup][use running cluster]

### New servers, have not yet joined a cluster

1.  Change the ring creation size parameter by uncommenting it and then
setting it to the desired value, for example 64:

    ```riakconf
    ring_size = 64
    ```

    ```appconfig
    %% In the riak_core section:
    {ring_creation_size, 64}
    ```

2.  Stop all nodes
3.  Remove the ring data file on each node (see [Backing up Riak][cluster ops backup] for
the location of this file)
4.  Finish reviewing this document and proceed to [Basic Cluster Setup][use running cluster]

### Verifying ring size

You can use the `riak-admin` command can verify the ring size:

```bash
riak-admin status | grep ring
```

Console output:

```
ring_members : ['riak@10.160.13.252']
ring_num_partitions : 8
ring_ownership : <<"[{'riak@10.160.13.252',8}]">>
ring_creation_size : 8
```

If `ring_num_partitions` and `ring_creation_size` do not agree, that
means that the `ring_creation_size` value was changed too late and that
the proper steps were not taken to start over with a new ring.

**Note**: Riak will not allow two nodes with different ring sizes to be
joined into a cluster.

## Backend

Another critical decision to be made is the backend to use. The choice
of backend strongly influences the performance characteristics and
feature set for a Riak environment.

See [Choosing a Backend][plan backend] for a list of supported backends. Each
referenced document includes the necessary configuration bits.

As with ring size, changing the backend will result in all data being
effectively lost, so spend the necessary time up front to evaluate and
benchmark backends.

If still in doubt, consider using the [Multi][plan backend multi] backend for future
flexibility.

If you do change backends from the default ([Bitcask][plan backend bitcask]), make sure you change it across all nodes. It is possible but generally unwise to use different backends on different nodes, as this would limit the
effectiveness of backend-specific features.

## Default Bucket Properties

Bucket properties are also very important factors in Riak's performance
and general behavior. The properties for any individual bucket can be
configured dynamically [using bucket types][usage bucket types], but default values for those properties can be defined in your [configuration files][config reference].

Below is an example of setting `last_write_wins` to `true` and `r` to 3.

```riakconf
buckets.default.last_write_wins = true
buckets.default.r = 3
```

```appconfig
{default_bucket_props, [
    {last_write_wins,true},
    {r,3},
    ...
    ]}
```

For more on bucket properties, we recommend reviewing our docs on
[buckets][concept buckets], [bucket types][usage bucket types], [replication properties][apps replication properties], and [eventual consistency][concept eventual consistency], as well as Basho's five-part blog series, "Understanding Riak's Configurable Behaviors."

* [Part 1](https://riak.com/understanding-riaks-configurable-behaviors-part-1/)
* [Part 2](https://riak.com/riaks-config-behaviors-part-2/)
* [Part 3](https://riak.com/riaks-config-behaviors-part-3/)
* [Part 4](https://riak.com/riaks-config-behaviors-part-4/)
* [Epilogue](https://riak.com/riaks-config-behaviors-epilogue/)

If the default bucket properties are modified in your configuration
files and the node is restarted, any existing buckets will **not** be
directly impacted, although the mechanism described in [HTTP Reset Bucket Properties]({{<baseurl>}}riak/kv/2.9.4/developing/api/http/reset-bucket-props) can be used to force them to pick up the new
defaults.

## System tuning

Please review the following documents before conducting any
[benchmarking][perf benchmark] and/or rolling out a live production
cluster.

* [Open Files Limit][perf open files]
* [System Performance Tuning][perf index]
* [AWS Performance Tuning][perf aws]
* [Configuration Files][config reference]

## Joining the nodes together

Please see [Running A Cluster][use running cluster] for the cluster creation process.

