---
title: Managing Strong Consistency
project: riak
version: 2.0.0+
document: guide
audience: advanced
keywords: [operators, strong-consistency]
---

This document is intended for Riak operators looking to configure and
monitor their Riak cluster's [[strong consistency]] subsystem.
Documentation for developers building applications using Riak's strong
consistency feature can be found in [[Using Strong Consistency]], while
a more theoretical treatment can be in found in [[Strong Consistency]].

## Cluster Size

In order to use strong consistency in Riak, **your cluster must consist
of at least three nodes**. If it does not, all strongly consistent
operations will fail. If your cluster is large enough, you can
[[enable strong consistency|Managing Strong
Consistency#Enabling-Strong-Consistency]] on a node-by-node basis; if
your cluster is smaller than three nodes, you will need to [[add
more|Adding and Removing Nodes]].

## Enabling Strong Consistency

Riak's strong consistency subsystem is disabled by default. If your Riak
cluster consists of at least three nodes, you can enable strong
consistency in your [[configuration files]].

```riakconf
strong_consistency = on
```

Remember that you must [[restart your node|riak Command Line]] for
configuration changes to take effect.

For strong consistency requirements to be applied to specific keys,
those keys must be in [[buckets]] bearing a bucket type with the
`consistent` property set to `true`. More information can be found in
[[Using Bucket Types]].

## Fault Tolerance

Strongly consistent operations in Riak are necessarily less highly
available than eventually consistent operations because strongly
consistent operations can only succeed if a **quorum** of object
replicas are currently reachable. A quorum can be expressed as
N / 2 + 1 (or `n_val` / 2 + 1), meaning 4 replicas if N=7, 5 replicas
if N=9, etc. If N=9 and 5 replicas are unavailable, for example, no
strongly consistent operations on that object can succeed.

While Riak uses an N of 3 by default, bear in mind that **higher values
of N will allow for more fault tolerance**. The table below shows the
number of allowable missing replicas for assorted values of N:

Replicas | Allowable missing replicas
:--------|:--------------------------
3 | 1
5 | 2
7 | 3
9 | 4
15 | 7

### `n_val` Recommendations

Due to the quorum requirements explained above, we recommend that you
use at least N=5 for strongly consistent data. You can achieve change
the value of N for buckets [[using bucket types]]. For example, you can
create and activate a bucket type with N set to 5 and strong consistency
enabled---we'll call it `consistent_and_fault_tolerant`---using the
following series of [[commands|riak-admin Command Line]]:

```bash
riak-admin bucket-type create consistent_and_fault_tolerant \
  '{"props": {"consistent":true,"n_val":5}}'
riak-admin bucket-type activate consistent_and_fault_tolerant
```

More on creating and activating bucket types can be found in our
[[bucket types|Using Bucket
Types#Managing-Bucket-Types-Through-the-Command-Line]] documentation.

<div class="note">
<div class="title">Note on bucket types and the <code>consistency</code>
property</div>
The <code>consistent</code> bucket property is one of two bucket
properties, alongside <code>datatype</code>, that cannot be changed once
a bucket type has been created and activated.
</div>

### Offline Node Recommendations

In general, strongly consistent Riak is more sensitive to the number of
nodes in the cluster than eventually consistent Riak, due to the quorum
requirements mentioned above. While Riak is designed to withstand many
a variety of failure scenarios that remove nodes from the cluster, e.g.
hardware or network failure, we nonetheless recommend that you limit the
number of nodes that you intentionally down or reboot, because having
multiple nodes leave the cluster at once can threaten quorum and thus
affect the viability of some or all strongly consistent operations.

## Performance

Strongly consistent operations in Riak are typically slower than their
eventually consistent counterparts, to varying degrees, because
consistent operations require more communication between Riak nodes.

If you are running into performance issues, bear in mind that the key
space in a Riak cluster is spread across multiple [[consensus
groups|Strong Consistency#Implementation-Details]], each of which
manages a portion of that key space. Increasing the [[ring
size|Clusters#The-Ring]] means more independent consensus groups, which
can provide for more concurrency and higher throughput, and thus better
performance. Instructions on increasing the ring size in an
already-running cluster can be found in [[Ring Resizing]].

Adding nodes to your cluster is another means of enhancing the
performance of strongly consistent operations. More information can be
found in [[Adding and Removing Nodes]].

## riak-admin ensemble-status

The `[[riak-admin|riak-admin Command Line#ensemble-status]]` interface
has an `ensemble-status` command that provides insight into the current
status of the consensus subsystem undergirding strong consistency.

Running the command by itself will provide general insight:

```bash
riak-admin ensemble-status
```

If the strong consistency subsystem is not currently enabled, you will
see `Note: The consensus subsystem is not enabled.` in the output of the
command.

If the consensus subsystem is enabled, you will see output like this:

```
============================== Consensus System ===============================
Enabled:     true
Active:      true
Ring Ready:  true
Validation:  strong (trusted majority required)
Metadata:    best-effort replication (asynchronous)

================================== Ensembles ==================================
 Ensemble     Quorum        Nodes      Leader
-------------------------------------------------------------------------------
   root       4 / 4         4 / 4      riak@riak1
    2         3 / 3         3 / 3      riak@riak2
    3         3 / 3         3 / 3      riak@riak4
    4         3 / 3         3 / 3      riak@riak1
    5         3 / 3         3 / 3      riak@riak2
    6         3 / 3         3 / 3      riak@riak2
    7         3 / 3         3 / 3      riak@riak4
    8         3 / 3         3 / 3      riak@riak4
```

### Interpreting ensemble-status Output

The following table provides a guide to `ensemble-status` output:

Item | Meaning
:----|:-------
`Enabled` | Whether the consensus subsystem is enabled on the current node, i.e. whether the `strong_consistency` parameter in <code><a href="/ops/advanced/configs/configuration-files#Strong-Consistency">riak.conf</a></code> has been set to `on`. If this reads `off` and you wish to enable strong consistency, see our documentation on <a href="/dev/advanced/strong-consistency#Enabling-Strong-Consistency">enabling strong consistency</a>.
`Active` | Whether the consensus subsystem is active, i.e. whether there are enough nodes in the cluster to use strong consistency, which requires at least three nodes.
`Ring Ready` | If `true`, then all of the vnodes in the cluster have seen the current <a href="/theory/concepts/clusters#The-Ring">ring</a>, which means that the strong consistency subsystem can be used; if `false`, then the system is not yet ready. If you have recently added or removed a node to/from the cluster, it may take some time for `Ring Ready` to change.
`Validation` | This will display `strong` if the `tree_validation` setting in <code><a href="/ops/advanced/configs/configuration-files#Strong-Consistency">riak.conf</a></code> has been set to `on` and `weak` if set to `off`.
`Metadata` | This depends on the value of the `synchronous_tree_updates` setting in <code><a href="/ops/advanced/configs/configuration-files#Strong-Consistency">riak.conf</a></code>, which determines whether strong consistency-related Merkle trees are updated synchronously or asynchronously. If `best-effort replication (asynchronous)`, then `synchronous_tree_updates` is set to `false`; if `guaranteed replication (synchronous)` then `synchronous_tree_updates` is set to `true`.
`Ensembles` | This displays a list of all of the currently existing ensembles active in the cluster.<br /><ul><li><code>Ensemble</code> --- The ID of the ensemble</li><li><code>Quorum</code> --- The number of ensemble peers that are either leading or following</li><li><code>Nodes</code> --- The number of nodes currently online</li><li><code>Leader</code> --- The current leader node for the ensemble</li></ul>

### Inspecting Specific Ensembles

The `ensemble-status` command enables you to inspect any currently
ensembles, i.e. the ensembles listed under `Ensembles` in the sample
`ensemble-status` output displayed above.

To inspect a specific ensemble, specify the ID:

```bash
riak-admin ensemble-status <id>
```

The following would inspect ensemble `2`:

```bash
riak-admin ensemble-status 2
```

Below is sample output for a single ensemble:

```
================================= Ensemble #2 =================================
Id:           {kv,0,3}
Leader:       riak@riak2 (2)
Leader ready: true

==================================== Peers ====================================
 Peer  Status     Trusted          Epoch         Node
-------------------------------------------------------------------------------
  1    following    yes             1            riak@riak1
  2     leading     yes             1            riak@riak2
  3    following    yes             1            riak@riak2
```

The table below provides a guide to the output:

Item | Meaning
:----|:-------
`Id` | The ID for the ensemble used internally by Riak, expressed as a 3-tuple. All ensembles are `kv`; the second element names the ring partition for which the ensemble is responsible; and the third element is the `n_val` for the keys for which the ensemble is responsible.
`Leader` | Identifies the ensemble's leader. In this case, the leader is on node `riak@riak2` and is identified as leader `2` on that node.
`Leader ready` | States whether the ensemble's leader is ready to respond to requests. If not, requests to the ensemble will fail.
`Peers` | A list of peer vnodes associated with the ensemble.<br /><ul><li><code>Peer</code> --- The ID of the peer</li><li><code>Status</code> --- Whether the peer is a leader or a follower</li><li><code>Trusted</code> --- Whether the peer's Merkle tree is currently considered trusted or not</li><li><code>Epoch</code> --- The current consensus epoch for the peer. The epoch is incremented each time the leader changes.</li><li><code>Node</code> --- The node on which the peer resides.</li></ul>

## Configuring Strong Consistency

All of the parameters listed below must be set in each node's
`advanced.config` file, _not_ in `riak.conf`. Information on the syntax
and usage of `advanced.config` can be found in our documentation on
[[advanced configuration|Configuration Files#Advanced-Configuration]],
as well as a full listing of [[strong-consistency-related
parameters|Configuration Files#Strong-Consistency]].

We also recommend reading about some of the [[implementation
details|Strong Consistency#]] behind strong consistency before changing
any of these parameters.

### Leaders, Followers, and Workers

Any ensemble responsible for a portion of the cluster's key space at any
point in time consists of **leaders** and **followers** that coordinate
on most requests. If you wish, leaders can be granted **leases** that
empower them to respond to read requests on their own, without
contacting any followers.

In addition to leaders and followers, any ensemble managing a portion of
the [[key space|Clusters#The-Ring]] in your cluster relies upon
concurrent **workers** to service requests.

#### Leader Behavior

The `trust_lease` setting determines whether leader leases are used to
optimize reads. When set to `true`, a leader with a valid lease can
handle reads directly without needing to contact any followers. When
`false`, the leader will always contact followers. The default is
`true`.

All leaders have periodic duties that they perform, including refreshing
the leader lease. You can determine how frequently this occurs, in
milliseconds, using the `ensemble_tick` setting. The default is 500
milliseconds. This setting must be lower than both `lease_duration` and
`follower_timeout` (both explained below).

If you set `trust_lease` to `true`, you can also specify how long a
leader lease remains valid without being refreshed, using the
`lease_duration` setting, which is specified in milliseconds. This
setting should be higher than `ensemble_tick` to ensure that leaders
have to time to refresh their leases before they time out, and it _must_
be lower than `follower_timeout`. The default is `ensemble_tick` * 2/3,
i.e. if `ensemble_tick` is 600, `lease_duration` will default to 400.

#### Worker Settings

You can choose how many workers are assigned to ensembles using the
`peer_workers` setting. The default is 1. Increasing the number of
workers will make the strong consistency system more computationally
expensive but it can improve performance in some cases, depending on the
workload. The default is `1`.

### Timeouts

You can establish timeouts for both gets and puts, using the
`peer_get_timeout` and `peer_put_timeout` settings, respectively. Both
are expressed in milliseconds and default to 60000 (i.e. 1 minute).

Longer timeouts will decrease the likelihood that get or put operations
will fail; shorter timeouts entail shorter wait times for connecting
clients, but at a greater risk of failed operations.

### Merkle Tree Settings

All peers in Riak's strong consistency system maintain persistent
[Merkle trees](http://en.wikipedia.org/wiki/Merkle_tree) for all data
stored by that peer. These trees are particularly useful when nodes
enter and leave the cluster.

#### Tree Validation

The `tree_validation` parameter determines whether Riak considers Merkle
trees to be trusted after a node restart. When enabled, i.e. when
`tree_validation` is set to `true` (the default), Riak does not trust
peer trees after a restart, instead requiring the peer to sync with a
trusted majority. While this is the safest mode because it protects Riak
against silent corruption in Merkle trees, it carries the drawback that
it can reduce Riak availability by requiring more than a simple majority
of nodes to be online and reachable.

#### Synchronous vs. Asynchronous Tree Updates

Merkle tree updates can happen synchronously or asynchronously. This is
determined by the `synchronous_tree_updates` parameter. When set to
`false`, which is the default, Riak responds to the client after the
first roundtrip, allowing the Merkle tree update to happen
asynchronously in the background; when set to `true`, Riak requires two
quorum roundtrips to occur before replying back to the client.

Please note that this setting applies only to the metadata writes sent
to followers. Leaders _always_ update their local Merkle trees before
responding to the client. Asynchronous updates can be unsafe in certain
scenarios. For example, if a leader crashes before sending metadata
updates to followers _and_ all followers that had acknowledged the write
somehow revert the object value immediately prior to the write request,
a future read could hypothetically return the immediately preceding
value without realizing that the value was incorrect. Setting
`synchronous_tree_updates` to `false` does bear this possibility, but it
is exceedingly unlikely.

## Monitoring Strong Consistency

Riak provides a wide variety of data related to the current operating
status of a node. This data is available by running the `[[riak-admin
status|Inspecting a Node#riak-admin-status]]` command.

A full listing of these stats is available in [[Inspecting a Node]].
All strong consistency-related stats are prefixed with `consistent_`,
e.g. `consistent_gets`, `consistent_puts`, etc. Most of these stats
are so-called "one-minute stats," meaning that they reflect node
activity in the last minute.

Strong consistency stats fall into two categories: GET-related and
PUT-related stats.

### GET-related stats

Stat | Description
:----|:-----------
`consistent_gets` | Number of strongly consistent GETs coordinated by this node in the last minute
`consistent_gets_total` | Total number of strongly consistent GETs coordinated by this node
`consistent_get_objsize_mean` | Mean object size for strongly consistent GETs on this node in the last minute
`consistent_get_objsize_median` | Median object size for strongly consistent GETs on this node in the last minute
`consistent_get_objsize_95` | 95th-percentile object size for strongly consistent GETs on this node in the last minute
`consistent_get_objsize_99` | 99th-percentile object size for strongly consistent GETs on this node in the last minute
`consistent_get_objsize_100` | 100th-percentile object size for strongly consistent GETs on this node in the last minute
`consistent_get_time_mean` | Mean time between reception of client GETs to strongly consistent keys and subsequent response
`consistent_get_time_median` | Median time between reception of client GETs to strongly consistent keys and subsequent response
`consistent_get_time_95` | 95th-percentile time between reception of client GETs to strongly consistent keys and subsequent response
`consistent_get_time_99` | 99th-percentile time between reception of client GETs to strongly consistent keys and subsequent response
`consistent_get_time_100` | 100th-percentile time between reception of client GETs to strongly consistent keys and subsequent response

### PUT-related stats

Stat | Description
:----|:-----------
`consistent_puts` | Number of strongly consistent PUTs coordinated by this node in the last minute
`consistent_puts_total` | Total number of strongly consistent PUTs coordinated by this node
`consistent_put_objsize_mean` | Mean object size for strongly consistent PUTs on this node in the last minute
`consistent_put_objsize_median` | Median object size for strongly consistent PUTs on this node in the last minute
`consistent_put_objsize_95` | 95th-percentile object size for strongly consistent PUTs on this node in the last minute
`consistent_put_objsize_99` | 99th-percentile object size for strongly consistent PUTs on this node in the last minute
`consistent_put_objsize_100` | 100th-percentile object size for strongly consistent PUTs on this node in the last minute
`consistent_put_time_mean` | Mean time between reception of client PUTs to strongly consistent keys and subsequent response
`consistent_put_time_median` | Median time between reception of client PUTs to strongly consistent keys and subsequent response
`consistent_put_time_95` | 95th-percentile time between reception of client PUTs to strongly consistent keys and subsequent response
`consistent_put_time_99` | 99th-percentile time between reception of client PUTs to strongly consistent keys and subsequent response
`consistent_put_time_100` | 100th-percentile time between reception of client PUTs to strongly consistent keys and subsequent response

## Strong Consistency and Active Anti-Entropy

Riak's [[active anti-entropy]] \(AAE) feature _can_ repair strongly
consistent data. Although it is not necessary to use active anti-entropy
if you are using strong consistency, we nonetheless recommend doing so.

Without AAE, all object conflicts are repaired via [[read
repair|Active Anti-Entropy#Read-Repair-vs.-Active-Anti-Entropy]].
Read repair, however, cannot repair conflicts in so-called "cold data,"
i.e. data that may not be read for long periods of time. While using AAE
does entail small performance losses, not using AAE can lead to problems
with silent on-disk corruption. 

To avoid these problems, you should [[enable active anti-entropy|Managing
Active Anti-Entropy##Enabling-Active-Anti-Entropy]].

## Known Issues 

There are a few known issues that you should be aware of when using the
latest version of strong consistency.

* **Consistent reads of never-written keys create tombstones** --- A
  tombstone will be written if you perform a read against a key that a
  majority of peers claims to not exist. This is necessary for certain
  corner cases in which offline or unreachable replicas containing
  partially written data need to be rolled back in the future.
* **Consistent keys and key listing** --- In Riak, key listing
  operations, such as listing all the keys in a bucket, do not filter
  out tombstones. While this is rarely a problem for non-strongly-
  consistent keys, it does present an issue for strong consistency due
  to the tombstone issues mentioned above.
* **Secondary indexes not supported** --- Strongly consistent
  operations do not support [[secondary indexes|Using Strong
  Consistency]] \(2i) at this time. Furthermore, any other metadata
  attached to objects will be silently ignored by Riak.
* **Multi-Datacenter Replication not supported** --- At this time,
  consistent keys are *not* replicated across clusters using [[Multi-
  Datacenter Replication]] \(MDC). This is because MDC replication
  currently supports only eventually consistent replication across
  clusters. Mixing strongly consistent data within a cluster with
  eventually consistent data between clusters is difficult to reason
  about from the perspective of applications. In a future version of
  Riak, we will add support for strongly consistent replication across
  multiple datacenters/clusters.
* **Client library exceptions** --- Basho's official [[client
  libraries]] convert errors return by Riak as generic exceptions with
  a message derived from the returned server-side error message. More
  information on this problem can be found in [[Using Strong
  Consistency|Using Strong Consistency#Error-Messages]].

