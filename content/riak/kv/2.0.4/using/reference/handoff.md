---
title: "Handoff Reference"
description: ""
project: "riak_kv"
project_version: "2.0.4"
menu:
  riak_kv-2.0.4:
    name: "Handoff"
    identifier: "managing_ref_handoff"
    weight: 101
    parent: "managing_ref"
toc: true
aliases:
  - /riak/2.0.4/ops/running/handoff/
  - /riak/kv/2.0.4/ops/running/handoff/
---

[cluster ops handoff]: {{<baseurl>}}riak/kv/2.0.4/using/cluster-operations/handoff

Riak is a distributed system built with two essential goals in mind:

* **fault tolerance**, whereby a Riak cluster can withstand node
    failure, network partitions, and other events in a way that does not
    disrupt normal functioning, and
* **scalability**, whereby operators can gracefully add and remove nodes
    to/from a Riak cluster

Both of these goals demand that Riak is able to either temporarily or
permanently re-assign responsibility for portions of the keyspace. That
re-assigning is referred to as **intra-cluster handoff** (or simply
**handoff** in our documentation).

## Types of Handoff

Intra-cluster handoff typically takes one of two forms: **hinted
handoff** and **ownership transfer**.

Hinted handoff occurs when a [vnode]({{<baseurl>}}riak/kv/2.0.4/learn/glossary/#vnode) temporarily takes over responsibility for some data and then returns that data to its original "owner." Imagine a 3-node cluster with nodes A, B, and C. If node C goes offline, e.g. during a network partition, nodes A and B will pick
up the slack, so to speak, assuming responsibility for node C's
operations. When node C comes back online, responsibility will be handed
back to the original vnodes.

Ownership transfer is different because it is meant to be permanent.
It occurs when a [vnode]({{<baseurl>}}riak/kv/2.0.4/learn/glossary/#vnode) no longer belongs to the node on which it's running. This typically happens when the very
makeup of a cluster changes, e.g. when nodes are added or removed from
the cluster. In this case, responsibility for portions of the keyspace
needs to be fundamentally re-assigned.

Both types of handoff are handled automatically by Riak. Operators do
have the option, however, of enabling and disabling handoff on
particular nodes or all nodes and of configuring key aspects of Riak's
handoff behavior. More information can be found below.

## Configuring Handoff

A full listing of configurable parameters can be found in our
[configuration files]({{<baseurl>}}riak/kv/2.0.4/configuring/reference/#intra-cluster-handoff)
document. The sections below provide a more narrative description of
handoff configuration.

### SSL

If you want to encrypt handoff behavior within a Riak cluster, you need
to provide each node with appropriate paths for an SSL certfile (and
potentially a keyfile). The configuration below would designate a
certfile at `/ssl_dir/cert.pem` and a keyfile at `/ssl_dir/key.pem`:

```riakconf
handoff.ssl.certfile = /ssl_dir/cert.pem
handoff.ssl.keyfile = /ssl_dir/key.pem
```

```appconfig
{riak_core, [
    %% Other configs
    {handoff_ssl_options, [
        {certfile, "/ssl_dir/cert.pem"},
        {keyfile, "/ssl_dir/key.pem"}
    ]},
    %% Other configs
]}
```

### Port

You can set the port used by Riak for handoff-related interactions using
the `handoff.port` parameter. The default is 8099. This would change the
port to 9000:

```riakconf
handoff.port = 9000
```

```appconfig
{riak_core, [
    %% Other configs
    {handoff_port, 9000},
    %% Other configs
]}
```

### Background Manager

Riak has an optional background manager that limits handoff activity in
the name of saving resources. The manager can help prevent system
response degradation during times of heavy load, when multiple
background tasks may contend for the same system resources. The
background manager is disabled by default. The following will enable it:

```riakconf
handoff.use_background_manager = on
```

```appconfig
{riak_kv, [
    %% Other configs
    {handoff_use_background_manager, on},
    %% Other configs
]}
```

### Maximum Rejects

If you're using Riak features such as [Riak Search]({{<baseurl>}}riak/kv/2.0.4/developing/usage/search/),
those subsystems can block handoff of primary key/value data, i.e. data
that you interact with via normal reads and writes.

The `handoff.max_rejects` setting enables you to set the maximum
duration that a [vnode]({{<baseurl>}}riak/kv/2.0.4/learn/glossary/#vnode) can be blocked by multiplying the
`handoff.max_rejects` setting by the value of
[`vnode_management_timer`]({{<baseurl>}}riak/kv/2.0.4/configuring/reference/#vnode_management_timer).
Thus, if you set `handoff.max_rejects` to 10 and
`vnode_management_timer` to 5 seconds (i.e. `5s`), non-K/V subsystems
can block K/V handoff for a maximum of 50 seconds. The default for
`handoff.max_rejects` is 6, while the default for
`vnode_management_timer` is `10s`. This would set `max_rejects` to 10:

```riakconf
handoff.max_rejects = 10
```

```appconfig
{riak_kv, [
    %% Other configs
    {handoff_rejected_max, 10},
    %% Other configs
]}
```

### Transfer Limit

You can adjust the number of node-to-node transfers (which includes
handoff) using the `transfer_limit` parameter. The default is 2. Setting
this higher will increase node-to-node communication but at the expense
of higher resource intensity. This would set `transfer_limit` to 5:

```riakconf
transfer_limit = 5
```

```appconfig
{riak_core, [
    %% Other configs
    {handoff_concurrency, 5},
    %% Other configs
]}
```

## Enabling and Disabling Handoff

Handoff can be enabled and disabled in two ways: via configuration or
on the command line.

### Enabling and Disabling via Configuration

You can enable and disable both outbound and inbound handoff on a node
using the `handoff.outbound` and `handoff.inbound` settings,
respectively. Both are enabled by default. The following would disable
both:

```riakconf
handoff.outbound = off
handoff.inbound = off
```

```appconfig
{riak_core, [
    %% Other configs
    {disable_outbound_handoff, true},
    {disable_inbound_handoff, true},
    %% Other configs
]}
```

### Enabling and Disabling Through the Command Line

Check out the [Cluster Operations: Handoff][cluster ops handoff] for steps on enabling and disabling handoff via the command line.
