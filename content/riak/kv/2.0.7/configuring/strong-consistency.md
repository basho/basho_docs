---
title: "Implementing Strong Consistency"
description: ""
project: "riak_kv"
project_version: "2.0.7"
menu:
  riak_kv-2.0.7:
    name: "Implementing Strong Consistency"
    identifier: "configuring_strong_consistency"
    weight: 105
    parent: "configuring"
toc: true
---

[apps strong consistency]: {{<baseurl>}}riak/kv/2.0.7/developing/app-guide/strong-consistency
[concept strong consistency]: {{<baseurl>}}riak/kv/2.0.7/using/reference/strong-consistency
[cluster ops add remove node]: {{<baseurl>}}riak/kv/2.0.7/using/cluster-operations/adding-removing-nodes
[config reference#strong-cons]: {{<baseurl>}}riak/kv/2.0.7/configuring/reference/#strong-consistency
[use admin riak cli]: {{<baseurl>}}riak/kv/2.0.7/using/admin/riak-cli
[concept eventual consistency]: {{<baseurl>}}riak/kv/2.0.7/learn/concepts/eventual-consistency
[plan backend bitcask]: {{<baseurl>}}riak/kv/2.0.7/setup/planning/backend/bitcask
[glossary vnode]: {{<baseurl>}}riak/kv/2.0.7/learn/glossary/#vnode
[concept buckets]: {{<baseurl>}}riak/kv/2.0.7/learn/concepts/buckets
[cluster ops bucket types]: {{<baseurl>}}riak/kv/2.0.7/using/cluster-operations/bucket-types
[use admin riak-admin#ensemble]: {{<baseurl>}}riak/kv/2.0.7/using/admin/riak-admin/#ensemble-status
[use admin riak-admin]: {{<baseurl>}}riak/kv/2.0.7/using/admin/riak-admin
[config reference#advanced]: {{<baseurl>}}riak/kv/2.0.7/configuring/reference/#advanced-configuration
[plan cluster capacity]: {{<baseurl>}}riak/kv/2.0.7/setup/planning/cluster-capacity
[cluster ops strong consistency]: {{<baseurl>}}riak/kv/2.0.7/using/cluster-operations/strong-consistency
[apps replication properties]: {{<baseurl>}}riak/kv/2.0.7/developing/app-guide/replication-properties
[concept causal context]: {{<baseurl>}}riak/kv/2.0.7/learn/concepts/causal-context
[dev data types]: {{<baseurl>}}riak/kv/2.0.7/developing/data-types
[glossary aae]: {{<baseurl>}}riak/kv/2.0.7/learn/glossary/#active-anti-entropy-aae
[cluster ops 2i]: {{<baseurl>}}riak/kv/2.0.7/using/reference/secondary-indexes
[usage commit hooks]: {{<baseurl>}}riak/kv/2.0.7/developing/usage/commit-hooks
[cluster ops obj del]: {{<baseurl>}}riak/kv/2.0.7/using/reference/object-deletion
[dev client libraries]: {{<baseurl>}}riak/kv/2.0.7/developing/client-libraries

> **Please Note:**
>
> Riak KV's strong consistency is an experimental feature and may be removed from the product in the future. Strong consistency is not commercially supported or production-ready. Strong consistency is incompatible with Multi-Datacenter Replication, Riak Search, Bitcask Expiration, LevelDB Secondary Indexes, Riak Data Types and Commit Hooks. We do not recommend its usage in any production environment.

This document provides information on configuring and monitoring a Riak
cluster's optional strong consistency subsystem. Documentation for
developers building applications using Riak's strong consistency feature
can be found in [Using Strong Consistency][apps strong consistency], while a more theoretical
treatment can be found in [Strong Consistency][concept strong consistency].

## Minimum Cluster Size

In order to use strong consistency in Riak, **your cluster must consist
of at least three nodes**. If it does not, all strongly consistent
operations will fail. If your cluster is smaller than three nodes, you
will need to [add more nodes][cluster ops add remove node] and make sure
that strong consistency is [enabled](#enabling-strong-consistency) on all of them.

Strongly consistent operations on a given key may also fail if a
majority of object replicas in a given ensemble are unavailable, whether
due to slowness, crashes, or network partitions. This means that you may
see strongly consistent operations fail even if the minimum cluster size
requirement has been met. More information on ensembles can be found in
[Implementation Details](#implementation-details).

While strong consistency requires at least three nodes, we have a
variety of recommendations regarding cluster size, which can be found in
[Fault Tolerance](#fault-tolerance).

## Enabling Strong Consistency

Strong consistency in Riak is disabled by default. You can enable it in
each node's [configuration files][config reference#strong-cons].

```riakconf
strong_consistency = on
```

```appconfig
%% In the older, app.config-based system, the strong consistency
%% parameter is enable_consensus:

{riak_core, [
    % ...
    {enable_consensus, true},
    % ...
    ]}
```

Remember that you must [restart your node][use admin riak cli] for
configuration changes to take effect.

For strong consistency requirements to be applied to specific keys,
those keys must be in [buckets][concept buckets] bearing a bucket type with the
`consistent` property set to `true`. More information can be found in
[Using Bucket Types][cluster ops bucket types].

If you enable strong consistency on all nodes in a cluster with fewer
than three nodes, strong consistency will be **enabled** but not yet
**active**. Strongly consistent operations are not possible in this
state. Once at least three nodes with strong consistency enabled are
detected in the cluster, the system will be activated and ready for use.
You can check on the status of the strong consistency subsystem using
the [`riak-admin ensemble-status`][use admin riak-admin#ensemble] command.

## Fault Tolerance

Strongly consistent operations in Riak are necessarily less highly
available than [eventually consistent][concept eventual consistency] operations
because strongly consistent operations can only succeed if a **quorum**
of object replicas are currently reachable. A quorum can be expressed as
N / 2 + 1 (or `n_val` / 2 + 1), meaning that 3 replicas constitutes a
quorum if N=5, 4 replicas if N=7, etc. If N=7 and 4 replicas are
unavailable, for example, no strongly consistent operations on that
object can succeed.

While Riak uses N=3 by default, bear in mind that **higher values of N
will allow for more fault tolerance**. The table below shows the number
of allowable missing replicas for assorted values of N:

Replicas | Allowable missing replicas
:--------|:--------------------------
3 | 1
5 | 2
7 | 3
9 | 4
15 | 7

Thus, we recommend setting `n_val` higher than the default of 3 for
strongly consistent operations. More on `n_val` in the section below.

### n_val Recommendations

Due to the quorum requirements explained above, we recommend that you
use _at least_ N=5 for strongly consistent data. You can set the value
of N, i.e. `n_val`, for buckets
[using bucket types][cluster ops bucket types]. For example, you
can create and activate a bucket type with N set to 5 and strong
consistency enabled---we'll call the bucket type
`consistent_and_fault_tolerant`---using the following series of
[commands][use admin riak-admin]:

```bash
riak-admin bucket-type create consistent_and_fault_tolerant \
  '{"props": {"consistent":true,"n_val":5}}'
riak-admin bucket-type activate consistent_and_fault_tolerant
```

If the `activate` command outputs `consistent_and_fault_tolerant has
been activated`, the bucket type is now ready to provide strong
consistency guarantees.

#### Setting the target_n_val parameter

The `target_n_val` parameter sets the highest `n_val` that you intend to
use in an entire cluster. The purpose of this parameter is to ensure
that so-called "hot spots" don't occur, i.e. that data is never stored
more than once on the same physical node. This can happen when:

* `target_n_val` is greater than the number of physical nodes, or
* the `n_val` for a bucket is greater than `target_n_val`.

A problem to be aware of if you're using strong consistency is that the
default for `target_n_val` is 4, while our suggested minimum `n_val` for
strongly consistent bucket types is 5. This means that you will need to
raise `target_n_val` if you intend to use an `n_val` over 4 for _any_
bucket type in your cluster. If you anticipate using an `n_val` of 7 as
the largest `n_val` within your cluster, for example, you will need to
set `target_n_val` to 7.

This setting is not contained in `riak.conf`, and must instead be set in
the `advanced.config` file. For more information, see our documentation
on [advanced configuration][config reference#advanced].

If you are using strong consistency in a cluster that has already been
created with a `target_n_val` that is too low (remember that the default
is too low), you will need to raise it to the desired higher value and
restart each node.

#### Note on Bucket Properties

The `consistent` bucket property is one of two bucket properties,
alongside [`datatype`][cluster ops bucket types], that cannot be changed once a
bucket type has been created.

Furthermore, if `consistent` is set to `true` for a bucket type, you
cannot change the `n_val` for the bucket type once it's been created. If
you attempt to do so, you'll see the following error:

```
Error updating bucket <bucket_type_name>:
n_val cannot be modified for existing consistent type
```

If you've created a bucket type with a specific `n_val` and wish to
change it, you will need to create a new bucket type with the
appropriate `n_val` and use the new bucket type instead.

### Fault Tolerance and Cluster Size

From the standpoint of strongly consistent operations, larger clusters
tend to be more fault tolerant. Spreading ensembles across more nodes will decrease the number of ensembles active on each node and thus decrease the number of quorums affected when a node goes down.

Imagine a 3-node cluster in which all ensembles are N=3 ensembles. If
two nodes go down, _all_ ensembles will lose quorum and will be unable
to function. Strongly consistent operations on the entire keyspace will
fail until at least one node is brought back online. And even when that
one node is brought back online, a significant portion of the keyspace
will continue to be unavailable for strongly consistent operations.

For the sake of contrast, imagine a 50-node cluster in which all
ensembles are N=5 (i.e. all objects are replicated to five nodes).  In
this cluster, each node is involved in only 10% of the total ensembles;
if a single node fails, that failure will thus impact only 10% of
ensembles. In addition, because N is set to 5, that will not impact
quorum for _any_ ensemble in the cluster; two additional node failures
would need to occur for quorum to be lost for _any_ ensemble.  And even
in the case of three nodes failing, it is highly unlikely that that
failure would impact the same ensembles; if it did, only those ensembles
would become unavailable, affecting only 10% of the key space, as
opposed to 100% in the example of a 3-node cluster consisting of N=3
ensembles.

These examples illustrate why we recommend higher values for N---again,
at least N=5---as well as clusters with many nodes. The 50-node cluster
example above is used only to illustrate why larger clusters are more
fault tolerant. The definition of "many" nodes will vary according to your needs.
For recommendations regarding cluster size, see [Cluster Capacity Planning][plan cluster capacity].

### Offline Node Recommendations

In general, strongly consistent Riak is more sensitive to the number of
nodes in the cluster than eventually consistent Riak, due to the quorum
requirements described above. While Riak is designed to withstand a
variety of failure scenarios that make nodes in the cluster unreachable,
such as hardware or network failure, **we nonetheless recommend that you
limit the number of nodes that you intentionally down or reboot**.
Having multiple nodes leave the cluster at once can threaten quorum and
thus affect the viability of some or all strongly consistent operations,
depending on the size of the cluster.

If you're using strong consistency and you do need to reboot multiple
nodes, we recommend rebooting them very carefully. Rebooting nodes too
quickly in succession can force the cluster to lose quorum and thus be
unable to service strongly consistent operations. The best strategy is
to reboot nodes one at a time and wait for each node to rejoin existing
[ensembles][cluster ops strong consistency] before
continuing to the next node. At any point in time, the state of
currently existing ensembles can be checked using [`riak-admin ensemble-status`][admin riak-admin#ensemble].

## Performance

If you run into performance issues, bear in mind that the key space in a
Riak cluster is spread across multiple [consensus groups][cluster ops strong consistency], each of which manages a portion of
that key space. Larger [ring sizes][concept clusters] allow more
independent consensus groups to exist in a cluster, which can provide
for more concurrency and higher throughput, and thus better performance.
The ideal ring size, however, will also depend on the number of nodes in
the cluster. General recommendations can be found in [Cluster Capacity Planning][plan cluster capacity].

Adding nodes to your cluster is another means of enhancing the
performance of strongly consistent operations. Instructions on doing so
can be found in [Adding and Removing Nodes][cluster ops add remove node].

Your cluster's configuration can also affect strong consistency
performance. See the section on [configuration][config reference#strong-cons] below.

## riak-admin ensemble-status

The [`riak-admin`][use admin riak-admin] interface
used for general node/cluster management has an `ensemble-status`
command that provides insight into the current status of the consensus
subsystem undergirding strong consistency.

Running the command by itself will provide the current state of the
subsystem:

```bash
riak-admin ensemble-status
```

If strong consistency is not currently enabled, you will see `Note: The
consensus subsystem is not enabled.` in the output of the command; if
strong consistency is enabled, you will see output like this:

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
`Enabled` | Whether the consensus subsystem is enabled on the current node, i.e. whether the `strong_consistency` parameter in [`riak.conf`][config reference#strong-cons] has been set to `on`. If this reads `off` and you wish to enable strong consistency, see our documentation on <a href="{{< baseurl >}}riak/kv/2.0.7/configuring/reference/#strong-consistency">enabling strong consistency</a>.
`Active` | Whether the consensus subsystem is active, i.e. whether there are enough nodes in the cluster to use strong consistency, which requires at least three nodes.
`Ring Ready` | If `true`, then all of the [vnodes][glossary vnode] in the cluster have seen the current <a href="{{< baseurl >}}riak/kv/2.0.7/learn/concepts/clusters/#the-ring">ring</a>, which means that the strong consistency subsystem can be used; if `false`, then the system is not yet ready. If you have recently added or removed one or more nodes to/from the cluster, it may take some time for `Ring Ready` to change.
`Validation` | This will display `strong` if the `tree_validation` setting in <code><a href="{{< baseurl >}}riak/kv/2.0.7/configuring/reference/#strong-consistency">riak.conf</a></code> has been set to `on` and `weak` if set to `off`.
`Metadata` | This depends on the value of the `synchronous_tree_updates` setting in <code><a href="{{< baseurl >}}riak/kv/2.0.7/configuring/reference/#strong-consistency">riak.conf</a></code>, which determines whether strong consistency-related Merkle trees are updated synchronously or asynchronously. If `best-effort replication (asynchronous)`, then `synchronous_tree_updates` is set to `false`; if `guaranteed replication (synchronous)` then `synchronous_tree_updates` is set to `true`.
`Ensembles` | This displays a list of all of the currently existing ensembles active in the cluster.<br /><ul><li><code>Ensemble</code> --- The ID of the ensemble</li><li><code>Quorum</code> --- The number of ensemble peers that are either leading or following</li><li><code>Nodes</code> --- The number of nodes currently online</li><li><code>Leader</code> --- The current leader node for the ensemble</li></ul>

**Note**: The **root ensemble**, designated by `root` in the sample
output above, is a special ensemble that stores a list of nodes and
ensembles in the cluster.

More in-depth information on ensembles can be found in our [internal
documentation](https://github.com/basho/riak_ensemble/blob/develop/doc/Readme.md).

### Inspecting Specific Ensembles

The `ensemble-status` command also enables you to directly inspect the
status of specific ensembles in a cluster. The IDs for all current
ensembles are displayed in the `Ensembles` section of the
`ensemble-status` output described above.

To inspect a specific ensemble, specify the ID:

```bash
riak-admin ensemble-status <id>
```

The following would inspect ensemble 2:

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
  3    following    yes             1            riak@riak3
```

The table below provides a guide to the output:

Item | Meaning
:----|:-------
`Id` | The ID for the ensemble used internally by Riak, expressed as a 3-tuple. All ensembles are `kv`; the second element names the ring partition for which the ensemble is responsible; and the third element is the `n_val` for the keys for which the ensemble is responsible.
`Leader` | Identifies the ensemble's leader. In this case, the leader is on node `riak@riak2` and is identified as peer `2` in the ensemble.
`Leader ready` | States whether the ensemble's leader is ready to respond to requests. If not, requests to the ensemble will fail.
`Peers` | A list of peer [vnodes][glossary vnode] associated with the ensemble.<br /><ul><li><code>Peer</code> --- The ID of the peer</li><li><code>Status</code> --- Whether the peer is a leader or a follower</li><li><code>Trusted</code> --- Whether the peer's Merkle tree is currently considered trusted or not</li><li><code>Epoch</code> --- The current consensus epoch for the peer. The epoch is incremented each time the leader changes.</li><li><code>Node</code> --- The node on which the peer resides.</li></ul>

More information on leaders, peers, Merkle trees, and other details can
be found in [Implementation Details](#implementation-details) below.

## Implementation Details

Strong consistency in Riak is handled by a subsystem called
[`riak_ensemble`](https://github.com/basho/riak_ensemble/blob/develop/doc/Readme.md)
This system functions differently from other systems in Riak in a number
of ways, and many of these differences are important to bear in mind for
operators configuring their cluster's usage of strong consistency.

### Basic Operations

The first major difference is that strongly consistent Riak involves a
different set of operations from [eventually consistent][concept eventual consistency] Riak KV. In strongly consistent buckets, there are four types
of atomic operations on objects:

* **Get** operations work just as they do against
  non-strongly-consistent keys, but with two crucial differences:
  1. Connecting clients are guaranteed to return the most recently
     written value (which makes those operations CP, i.e. consistent and
     partition tolerant)
  2. Reads on strongly consistent keys *never* return siblings, hence
     there is no need to develop any sort of [conflict resolution][usage conflict resolution]
     strategy for those keys
* **Conditional put** operations write an object only if no object
  currently exists in that key. The operation will fail if the key
  already exists; if the key was never written or has been deleted, the
  operation succeeds.
* **Conditional modify** operations are compare-and-swap (CAS)
  operations that succeed only if the value of a key has not changed
  since it was previously read.
* **Delete** operations work mostly like they do against
  non-strongly-consistent keys, with the exception that
  [tombstones][cluster ops obj deletion] are not harvested, which is
  the equivalent of having `delete_mode` set to `keep`.

**From the standpoint of clients connecting to Riak, there is little
difference between strongly and non-strongly consistent data**. The
operations performed on objects---reads, writes, deletes, etc.---are the
same, which means that the client API for strong consistency is
essentially the same as it is for eventually consistent operations, with
the important exception of error handling.

### Ensembles

The main actors in Riak's implementation of strong consistency are
**ensembles**, which are independent groups that watch over a portion of
a Riak cluster's key space and coordinate strongly consistent operations
across nodes. When watching over a given key space, ensembles must act
upon multiple replicas of a given object, the number of which is
specified by `n_val` (more on this in [Replication Properties][apps replication properties]).

Eventually consistent Riak can service requests even when only a single
object replica is available, using mechanisms like [vector clocks][concept causal context] and [dotted version vectors][concept causal context]---or, in a different way, [Riak Data Types][dev data types])---to ensure eventual consistency between replicas.  Strongly consistent Riak is different because it
requires that a **quorum** of object replicas be online and reachable,
where a quorum is defined as `n_val` / 2 + 1. **If a quorum is not
available for a key, all strongly consistent operations against that key
will fail**.

More information can be found in the section on Fault Tolerance above.

### Peers, Leaders, Followers, and Workers

All ensembles in strongly consistent Riak consist of agents called
**peers**. The number of peers in an ensemble is defined by the `n_val`
of that ensemble, i.e. the number of object replicas that the
ensemble watches over. Amongst the peers in the ensemble, there are two
basic actors: **leaders** and **followers**.

Leaders and followers coordinate with one another on most requests.
While leaders and followers coordinate on all writes, i.e. all puts and
deletes, you can enable leaders to respond to gets without the need to
coordinate with followers. This is known as granting a **leader lease**.
Leader leases are enabled by default, and are disabled (or re-enabled)
at the cluster level. A more in-depth account of ensemble behavior can
be found in our [internal
documentation](https://github.com/basho/riak_ensemble/tree/develop/doc).

In addition to leaders and followers, ensemble peers use lightweight
Erlang processes called **workers** to perform long-running K/V
operations, allowing peers to remain responsive to requests. The number
of workers assigned to each peer depends on your configuration.

These terms should be borne in mind in the sections on configuration
below.

### Integrity Checking

An essential part of implementing a strong consistency subsystem in a
distributed system is **integrity checking**, which is a process that
guards against data corruption and inconsistency even in the face of
network partitions and other adverse events that Riak was built to
handle gracefully.

Like Riak's [active anti-entropy][glossary aae] subsystem, strong consistency
integrity checking utilizes [Merkle
trees](http://en.wikipedia.org/wiki/Merkle_tree) that are persisted on
disk. All peers in an ensemble, i.e. all leaders and followers, maintain
their own Merkle trees and update those trees in the event of most
strongly consistent operations. Those updates can occur synchronously or
asynchronously from the standpoint of client operations, depending on
the configuration that you specify.

While integrity checking takes place automatically in Riak, there are
important aspects of its behavior that you can configure. See the <a
href="#merkle">Merkle Tree settings</a> section below for more
information on configurable parameters.

## Configuring Strong Consistency

The `riak_ensemble` subsystem provides a wide variety of tunable
parameters that you can adjust to fit the needs of your Riak cluster.
All `riak_ensemble`-specific parameters, with the exception of the
`strong_consistency` parameter used to [enable strong consistency](#enabling-strong-consistency),
must be set in each node's `advanced.config` file, _not_ in `riak.conf`
or `app.config`.

Information on the syntax and usage of `advanced.config` can be found in
our documentation on [advanced configuration][config reference#advanced]. That same document also contains a full
listing of [strong-consistency-related configuration parameters][config reference#strong-cons].

Please note that the sections below require a basic understanding of the
following terms:

* ensemble
* peer
* leader
* follower
* worker
* integrity checking
* Merkle tree

For an explanation of these terms, see the [Implementation Details](#implementation-details) section
above.

#### Leader Behavior

The `trust_lease` setting determines whether leader leases are used to
optimize reads. When set to `true`, a leader with a valid lease can
handle reads directly without needing to contact any followers. When
`false`, the leader will always contact followers, which can lead to
degraded read performance. The default is `true`. We recommend leaving
leader leases enabled for performance reasons.

All leaders have periodic duties that they perform, including refreshing
the leader lease. You can determine how frequently this occurs, in
milliseconds, using the `ensemble_tick` setting. The default is 500
milliseconds. Please note that this setting must be lower than both
the `lease_duration` and `follower_timeout` settings (both explained
below).

If you set `trust_lease` to `true`, you can also specify how long a
leader lease remains valid without being refreshed using the
`lease_duration` setting, which is specified in milliseconds. This
setting should be higher than `ensemble_tick` to ensure that leaders
have to time to refresh their leases before they time out, and it _must_
be lower than `follower_timeout`, explained in the section below. The
default is `ensemble_tick` * 3/2, i.e. if `ensemble_tick` is 400,
`lease_duration` will default to 600.

#### Worker Settings

You can choose how many workers are assigned to each peer using the
`peer_workers` setting. Workers are lightweight processes spawned by
leaders and followers. While increasing the number of workers will make
the strong consistency subsystem slightly more computationally
expensive, more workers can mean improved performance in some cases,
depending on the workload. The default is 1.

### Timeouts

You can establish timeouts for both reads and writes (puts and deletes)
using the `peer_get_timeout` and `peer_put_timeout` settings,
respectively. Both are expressed in milliseconds and default to 60000
(1 minute).

Longer timeouts will decrease the likelihood that read or write
operations will fail due to long computation times; shorter timeouts
entail shorter wait times for connecting clients, but at a higher risk
of failed operations under heavy load.

### Merkle Tree Settings
<a name="merkle"></a>

Leaders and followers in Riak's strong consistency system maintain
persistent [Merkle trees](http://en.wikipedia.org/wiki/Merkle_tree) for
all data stored by that peer. More information can be found in the
**Integrity Checking** section above. The two sections directly below
describe Merkle-tree-related parameters.

#### Tree Validation

The `tree_validation` parameter determines whether Riak considers Merkle
trees to be trusted after peers are restarted (for whatever reason).
When enabled, i.e. when `tree_validation` is set to `true` (the
default), Riak does not trust peer trees after a restart, instead
requiring the peer to sync with a trusted quorum. While this is the
safest mode because it protects Riak against silent corruption in Merkle
trees, it carries the drawback that it can reduce Riak availability by
requiring more than a simple majority of nodes to be online and
reachable when peers restart.

If you are using ensembles with N=3, we strongly recommend setting
`tree_validation` to `false`.

#### Synchronous vs. Asynchronous Tree Updates

Merkle tree updates can happen synchronously or asynchronously. This is
determined by the `synchronous_tree_updates` parameter. When set to
`false`, which is the default, Riak responds to the client after the
first roundtrip that updates the followers' data but before the second
roundtrip required to update the followers' Merkle trees, allowing the
Merkle tree update to happen asynchronously in the background; when set
to `true`, Riak requires two quorum roundtrips to occur before replying
back to the client, which can increase per-request latency.

Please note that this setting applies only to Merkle tree updates sent
to followers. Leaders _always_ update their local Merkle trees before
responding to the client. Asynchronous updates can be unsafe in certain
scenarios. For example, if a leader crashes before sending metadata
updates to followers _and_ all followers that had acknowledged the write
somehow revert the object value immediately prior to the write request,
a future read could hypothetically return the immediately preceding
value without realizing that the value was incorrect. Setting
`synchronous_tree_updates` to `false` does bear this possibility, but it
is highly unlikely.

## Strong Consistency and Active Anti-Entropy

Riak's [active anti-entropy][glossary aae] \(AAE) feature _can_ repair strongly
consistent data. Although it is not necessary to use active anti-entropy
if you are using strong consistency, we nonetheless recommend doing so.

Without AAE, all object conflicts are repaired via read repair.
Read repair, however, cannot repair conflicts in so-called "cold data,"
i.e. data that may not be read for long periods of time. While using AAE
does entail small performance losses, not using AAE can lead to problems
with silent on-disk corruption.

## Strong Consistency and Bitcask

One feature that is offered by Riak's optional [Bitcask][plan backend bitcask] backend is object expiry. If you are using strong consistency and Bitcask together, you should be aware that object metadata is often updated by the strong consistency subsystem during leader changes, which typically take place when nodes go down or during network partitions. When these metadata updates take place, the time to live (TTL) of the object is refreshed, which can lead to general unpredictably in objects' TTL. Although leader changes will be rare in many clusters, we nonetheless recommend that you use object expiry in
strongly consistent buckets only in situations when these occasional
irregularities are acceptable.

## Important Caveats

The following Riak features are not currently available in strongly
consistent buckets:

* [Secondary indexes][cluster ops 2i] --- If you do attach
  secondary index metadata to objects in strongly consistent buckets,
  strongly consistent operations can still proceed, but that metadata
  will be silently ignored.
* [Riak Data Types][dev data types] --- Data Types can currently be
  used only in an eventually consistent fashion
* [Using commit hooks][usage commit hooks] --- Neither pre- nor post-commit hooks are supported in strongly consistent buckets. If you do associate a
  strongly consistent bucket with one or more commit hooks, strongly
  consistent operations can proceed as normal in that bucket, but all
  commit hooks will be silently ignored.

Furthermore, you should also be aware that strong consistency guarantees
are applied only at the level of single keys. There is currently no
support within Riak for strongly consistent operations against multiple
keys, although it is always possible to incorporate client-side write
and read locks in applications that use strong consistency.

## Known Issues

There are a few known issues that you should be aware of when using the
latest version of strong consistency.

* **Consistent reads of never-written keys create tombstones** --- A
  [tombstone][cluster ops obj del] will be written if you perform a read
  against a key that a majority of peers claims to not exist. This is
  necessary for certain corner cases in which offline or unreachable
  replicas containing partially written data need to be rolled back in
  the future.
* **Consistent keys and key listing** --- In Riak, key listing
  operations, such as listing all the keys in a bucket, do not filter
  out tombstones. While this is rarely a problem for
  non-strongly-consistent keys, it does present an issue for strong
  consistency due to the tombstone issues mentioned above.
* **Secondary indexes not supported** --- Strongly consistent
  operations do not support [secondary indexes][cluster ops 2i] \(2i) at this time. Furthermore, any other metadata
  attached to objects, even if not related to 2i, will be silently
  ignored by Riak in strongly consistent buckets.
* **Multi-Datacenter Replication not supported** --- At this time,
  consistent keys are *not* replicated across clusters using Multi-
  Datacenter Replication \(MDC). This is because MDC Replication currently supports only eventually consistent replication across clusters. Mixing strongly
  consistent data within a cluster with eventually consistent data
  between clusters is difficult to reason about from the perspective of
  applications. In a future version of Riak, we will add support for
  strongly consistent replication across multiple datacenters/clusters.
* **Client library exceptions** --- Basho's official [client
  libraries][dev client libraries] convert errors returned by Riak into generic exceptions,
  with a message derived from the returned server-side error message.
