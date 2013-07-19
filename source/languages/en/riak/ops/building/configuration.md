---
title: Basic Configuration
project: riak
version: 1.3.1+
document: tutorial
toc: true
audience: beginner
keywords: [operators, building, configuration]
---

Riak is extensively configurable to meet various business needs.

This document captures the parameters that are commonly tweaked when
setting up a new cluster, but it is highly advisable to review the
detailed [[Configuration Files]] document before moving a cluster into
production.

All configuration values discussed here are managed via the
`app.config` file on each node, and a node must be restarted for any
changes to take effect.

## Ring size

The ring size in Riak parlance is the number of data partitions which
comprise the cluster. This quantity impacts the scalability and
performance of a cluster, and importantly, **it must be established
before the cluster starts receiving data**.

If the ring size is too large for the number of servers, disk I/O will
be negatively impacted by the excessive number of concurrent databases
running on each server.

If the ring size is too small, the servers' other resources (primarily
CPU and RAM) will go underutilized.

A general rule of thumb is to aim for 10 partitions per server. That
quantity can be higher for high performance bare metal hardware.

The value must be a power of 2, and defaults to 64. As of Riak 1.4,
1024 is a reasonable upper limit, and 8 the lower limit. Since Basho
strongly recommends that a production cluster be no smaller than 5
servers, 64 is an effective lower limit for such an environment, but
that value should be increased if the cluster is expected to grow
significantly.

The steps involved depend on whether the servers (nodes) in the
cluster have already been joined together.

### Cluster already in active use

If you have a cluster in use and need to preserve its data while
resizing the ring, you may either migrate your data to a new cluster
or contact Basho to discuss the possibility of performing a dynamic
ring resizing operation.

### Cluster joined, but no data

1.  Uncomment the following line in the `riak_core` section in
`app.config` on each node and set the appropriate quantity
    %{ring_creation_size, 64},
2.  Stop all nodes
3.  Remove the ring data file on each node (see [[Backing up Riak]] for the location of this file)
4.  Start all nodes
5.  Join each node to the cluster (see [[Adding and Removing Nodes|Adding and Removing Nodes#Add-a-Node-to-an-Existing-Cluster]])

### New servers, have not yet joined a cluster

1.  Uncomment the following line in the `riak_core` section in
`app.config` on each node and set the appropriate quantity
    %{ring_creation_size, 64},
2.  Restart all nodes

## XXXX WHERE THE HECK IS Basic Cluster Setup?

## Backend

The most critical decision to be made is the backend to use. The
choice of backend strongly influences the performance characteristics
and feature set for a Riak environment.

See [[Choosing a Backend]] for a list of supported backends; each
referenced document includes the necessary configuration bits.


## Default bucket properties

Bucket properties are also very important to performance and
behavioral characteristics.

The properties for any individual bucket can be configured
dynamically, but default values for those properties can be defined in
`app.config`, in the `riak_core` section.

Below is an example of setting default bucket properties (admittedly,
in this example to the values which Riak would define to them in the
absence of such a configuration).

```
{default_bucket_props, [
    {n_val,3},
    {r,quorum},
    {w,quorum},
    {allow_mult,false},
    {last_write_wins,false},
    ]}
```

In short: replicate data 3 times, require that 2 of those 3 replicas
respond before any read or write is considered successful, and do not
present conflicting values to the application for resolution.

The `r` and `w` values can be overridden with each request, and few
users need to change `n_val`, but choosing an appropriate value for
`allow_mult` is vital for a robust application.

For more on the implications of these settings, please see
[[Eventual Consistency]], [[Replication]], and the Basho blog series,
"Understanding Riak's Configurable Behaviors":
[[Part 1|http://basho.com/understanding-riaks-configurable-behaviors-part-1/]],
[[Part 2|http://basho.com/riaks-config-behaviors-part-2/]],
[[Part 3|http://basho.com/riaks-config-behaviors-part-3/]],
[[Part 4|http://basho.com/riaks-config-behaviors-part-4/]], and the
[[Epilogue|http://basho.com/riaks-config-behaviors-epilogue/]].


## Active anti-entropy (AAE)

Riak historically has resolved conflicting data when values are read


## Riak Search


## System tuning

Please review the following documents before conducting any
[[benchmarking|Basho Bench]] and/or rolling out a live production
cluster.

* [[Open Files Limit]]
* [[File System Tuning]]
* [[Linux Performance Tuning]]
* [[AWS Performance Tuning]]
* [[Configuration Files]]
