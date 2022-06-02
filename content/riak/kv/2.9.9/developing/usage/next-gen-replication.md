---
title: "Next-Gen Replication"
description: ""
project: "riak_kv"
project_version: "2.9.9"
menu:
  riak_kv-2.9.9:
    name: "Next Gen Replication"
    identifier: "learn_concepts_next_gen_replication"
    weight: 108
    parent: "learn_concepts"
version_history:
  in: "2.9.1+"
toc: true
aliases:
  - /riak-docs/riak/2.9.9/dev/using/nextgenreplication
---
[concept TicTac aae]: {{<baseurl>}}riak/kv/2.9.9/learn/concepts/tictac-active-anti-entropy
[concept causal context vc]: {{<baseurl>}}riak/kv/2.9.9/learn/concepts/causal-context/#vector-clocks
[concept clusters]: {{<baseurl>}}riak/kv/2.9.9/learn/concepts/clusters
[concept vnodes]: {{<baseurl>}}riak/kv/2.9.9/learn/concepts/vnodes
[glossary node]: {{<baseurl>}}riak/kv/2.9.9/learn/glossary/#node
[glossary ring]: {{<baseurl>}}riak/kv/2.9.9/learn/glossary/#ring
[usage replication]: {{<baseurl>}}riak/kv/2.9.9/developing/usage/replication

## Next Generation Replication - How it Works

### Replication Actors

Each node in `riak_kv` starts three processes that manage the inter-cluster replication.  A tictac AAE full-sync manager, a replication queue source manager, and a replication queue sink manager.  All processes are started by default (whether or not replication is enabled), but will only play an active role should replication be configured.  Further details on the processes involved:

* __Tictac AAE Full-Sync Manager__ - `riak_kv_ttaaefs_manager`

  * There is a single actor on each node that manages the full-sync reconciliation workload configured for that node.  

  * Each node is configured with the details of a peer node at a remote cluster.  Each manager is responsible for controlling cluster-wide hashtree exchanges between the local node and the peer node, and to prompt any repairs required across the cluster (not just on this node).  The information is exchanged between the peers, but that information represents the data across the whole cluster.  Necessary repairs are prompted through the replication queue source-side manager `riak_kv_replrtq_src`.

  * Each node is configured with a schedule to determine how frequently this manager will run its reconcile and repair operations.

  * It is is an administrator responsibility to ensure the cluster AAE workload is distributed across nodes with sufficient diversity to ensure correct operation under failure.  Work is not re-distributed between nodes in response to failure on either the local or remote cluster, so there must be other nodes already configured to share that workload to continue operation under failure conditions.

  * Each node can only full-sync with one other cluster (via the one peer node).  If the cluster needs to full-sync with more than one cluster, then the administrator should ensure different nodes have the different configurations necessary to achieve this.

  * Scheduling of work to minimise concurrency of reconciliation operations is managed by this actor using a simple, coordination-free mechanism.

  * The administrator may at run-time suspend or resume the regular running of full-sync operations on any given node via the `riak_kv_ttaaefs_manager`.

* __Replication Queue Source-Side Manager__

  * There is a single actor on each node that manages the queueing of replication object references to be consumed from other clusters. This actor runs a configurable number of queues, which contain pointers to data which is required to be consumed by different remote clusters.

  * The general pattern is that each delta within a cluster will be published once via the `riak_kv_replrtq_src` on a node local to the discovery of the change.  Each queue which is a source of updates will have multiple consumers spread across multiple sink nodes on the receiving cluster - where each sink-side node's consumers are being managed by a `riak_kv_replrtq_snk` process on that node.

  * Queues may have data filtering rules to restrict what changes are distributed via that queue.  The filters can restrict replication to a specific bucket, or bucket type, a bucket name prefix or allow for any change to be published to that queue.

  * __Real-time replication__ changes (i.e. PUTs that have just been co-ordinated on this node within the cluster), are sent to the `riak_kv_replrtq_src` in one of the following formats:
    * {Bucket, Key, Clock, {tombstone, Object}};
    * {Bucket, Key, Clock, {object, Object}};
    * {Bucket, Key, Clock, to_fetch}.

  * Real-time replicated objects are the highest priority items to be queued, and are placed on __every queue whose data filtering rules are matched__ by the object.  If the priority queue has grown beyond a limited number of items (the number being defined in `riak_kv.replrtq_srcobjectlimt`), then any {object, Object} references is stripped and replaced with `to_fetch`.  This is to help limit the memory consumed by the queue during failure conditions i.e. when a sink has stopped consuming from the source queue.

  * Changes identified by __AAE full-sync replication__ processes run by the `riak_kv_ttaaefs` manager on the local node are sent to the `riak_kv_replrtq_src` as references, and queued as the second highest priority.  These changes are queued only on __a single queue defined within the configuration__ of `riak_kv_ttaaefs_manager`.  The changes queued are only references to the object (Bucket, Key and Clock) not the actual object.

  * Changes identified by __AAE fold operations__ for administrator initiated transition or repair operations (e.g. fold over a bucket or key-range, or for a given range of modified dates), are sent to the `riak_kv_replrtq_src` to be queued as the lowest priority onto __a single queue defined by the administrator when initiating the AAE fold operation__.  The changes queued are only references to the object (Bucket, Key and Clock) not the actual object - and are only the changes discovered through the fold running on vnodes local to this node.

  * Should the local node fail, all undelivered object references will be dropped.

  * Queues are bounded, with limits set separately for each priority.  Items are consumed from the queue in strict priority order.  So a backlog of non-real-time replication events cannot cause a backlog or failure in real-time events.

  * The queues are provided using the existing `riak_core_priority_queue` module in Riak.

  * The administrator may at run-time suspend or resume the publishing of data to specific queues via the `riak_kv_replrtq_src` process.

* __Replication Queue Sink-Side Manager__ `riak_kv_replrtq_snk`

  * There is a single actor on each node that manages the process of consuming from queues on the `riak_kv_replrtq_src` on remote clusters.

  * The `riak_kv_replrtq_snk` can be configured to consume from multiple queues, across an open-ended number of peers.  For instance if each node on Cluster A maintains a queue named `cluster_c_full`, and each node on Cluster B maintains a queue named `cluster_c_partial` - then `riak_kv_replrtq_snk` can be configured to consume from the `cluster_c_full` from every node in Cluster A and from `cluster_c_partial` from every node in Cluster B.

  * The `riak_kv_replrtq_snk` manages a finite number of workers for consuming from remote peers.  The `riak_kv_replrtq_snk` tracks the results of work in order to back-off slightly from peers regularly not returning results to consume requests (in favour of those peers indicating a backlog by regularly returning results).  The `riak_kv_replrtq_snk` also tracks the results of work in order to back-off severely from those peers returning errors (so as not to lock too many workers consuming from unreachable nodes).

  * The administrator may at run-time suspend or resume the consuming of data from specific queues or peers via the `riak_kv_replrtq_snk`.
  
### Real-time Replication - Step by Step

Previous replication implementations initiate replication through a post-commit hook.  Post-commit hooks are fired from the `riak_kv_put_fsm` after "enough" responses have been received from other vnodes (based on n, w, dw and pw values for the PUT).  Without enough responses, the replication hook is not fired, although the client should receive an error and retry. This process of retrying may eventually fire the hook - although it is possible for a PUT to fail, the hook not to be fired, but a GET be locally successful (due to read-repair and anti-entropy) and there be no clue that the object has not been replicated.

In implementing the new replication solution, the point of firing off replication has been changed to the point that the co-ordinated PUT is completed.  So the replication of the PUT to the clusters may occur in parallel to the replication of the PUT to other nodes in the source cluster.  This is the first opportunity where sufficient information is known (e.g. the updated vector clock), and reduces the size of the time-window of inconsistency between the clusters, and also reduce the window of opportunity for a PUT to succeed but not have replication triggered.

Replication is fired within the `riak_kv_vnode` `actual_put/8`.  On condition of the vnode being a co-ordinator of the put, and of `riak_kv.replrtq_enablesrc` being set to enabled (true), the following work is done:

- The object reference to be replicated is determined, this is the type of reference to be placed on the replication queue.  

  - If the object is now a tombstone, the whole object is used as the replication reference.  The whole object is used due to the small size of the object, and the need to avoid race conditions with reaping activity if `delete_mode` is not `keep` - the cluster may not be able to fetch the tombstone to replicate in the future.  The whole object must be kept on the queue and not be filtered by the `riak_kv_replrtq_src` to be replaced with a `to_fetch` reference.

  - If the object is below the `riak_kv.replrtq_srcobjectsize` (default 200KB) then whole object will be sent to the `riak_kv_replrtq_src`, and it will be queued as a whole object as long as the current size of the priority real-time queue does not exceed the `riak_kv.replrtq_srcobjectlimit` (default 1000).  If an object is over the size limit a `to_fetch` references will be sent instead of the object, and if the queue is too large the `riak_kv_replrtq_src` will substitute a `to_fetch` reference before queueing.

- The `{Bucket, Key, Clock, ObjectReference}` is cast to the `riak_kv_replrtq_src` and placed by the `riak_kv_replrtq_src` on the priority queue.

- The queue has a configurable absolute limit, that is applied individually for each priority.  The limit is configured via `riak_kv.replrtq_srcqueuelimit` and defaults to 300,000 references (5 minutes of traffic at 1,000 PUTs per second).  When this limit is reached, new replication references are discarded on receipt rather than queued - these discarded references will need to eventually be re-replicated via full-sync.

The reference now needs to be handled by the `riak_kv_replrtq_src`.  The task list for this process is:

- Assign a priority to the replication event depending on what prompted the replication (e.g. highest priority to real-time events received from co-ordinator vnodes).

- Add the reference to the tail of the __every__ matching queue based on priority.  Each queue is configured to either match `any` replication event, no real-time events (using the configuration `block_rtq`), or a subset of events (using either a bucket `type` filter or a `bucket` filter).

In order to replicate the object, it must now be fetched from the queue by a sink.  A sink-side cluster should have multiple consumers, on multiple nodes, consuming from each node in the source-side cluster.  These workers are handed work items by the `riak_kv_replrtq_snk`, with a Riak client configured to communicate to the remote node, and the worker will initiate a `fetch` from that node.

On receipt of the `fetch` request the source node should:

- Initiate a `riak_kv_get_fsm`, passing `{queuename, QueueName}` in place of `{Bucket, Key}`.

- The GET FSM should go directly into the `queue_fetch` state, and try and fetch the next replication reference from the given queue name via the `riak_kv_replrtq_src`.

  - If the fetch from the queue returns `queue_empty` this is relayed back to the sink-side worker, and ultimately the `riak_kv_replrtq_snk` which may then slow down the pace at which fetch requests are sent to this node/queue combination.  To reduce the volume of individual requests when queues are mainly empty, the queue is only considered empty if it has reported empty 8 times from requests 4ms apart.

  - If the fetch returns an actual object, this is relayed back to the sink worker.

  - If the fetch returns a replication reference with the flag `to_fetch`, the `riak_kv_get_fsm` will continue down the standard path os states starting with `prepare`, and fetch the object which the will be returned to the sink worker.

- If a successful fetch is relayed back to the sink worker it will replicate the PUT using a local `riak_client:push/4`.  The push will complete a PUT of the object on the sink cluster - using a `riak_kv_put_fsm` with appropriate options (e.g. `asis`, `disable-hooks`).

  - The code within the `riak_client:push/4` follows the behaviour of the existing `riak_repl` on receipt of a replicated object.

- If the fetch and push request fails, the sink worker will report this back to the `riak_kv_replrtq_snk` which should delay further requests to that node/queue so as to avoid rapidly locking sink workers up communicating to a failing node.


### Full-Sync Reconciliation and Repair - Step by Step

The `riak_kv_ttaaefs_manager` controls the full-sync replication activity of a node.  Each node is configured with a single peer with which it is to run full-sync checks and repairs, assuming that across the cluster sufficient peers to sufficient clusters have been configured to complete the overall work necessary for that cluster.  Ensuring there are sufficient peer relations is an administrator responsibility, there are no re-balancing or re-scaling scenarios during failure scenarios.

The `riak_kv_ttaaefs_manager` is a source side process.   It will not attempt to repair any discovered discrepancies where the remote cluster is ahead of the local cluster - the job of the process is to ensure that a remote cluster is up-to-date with the changes which have occurred in the local cluster.  For mutual full-sync replication, there will be a need for an equivalent configuration on the peer cluster.

The `riak_kv_ttaaefs_manager` has a schedule of work obtained from the configuration.  The schedule has wants, the number of times per day that it is desired that this manager will:

- Reconcile changes across all the whole cluster over all time;

- Skip work for a schedule slot and do nothing;

- Reconcile changes that have occurred in the past hour;

- Reconcile changes that have occurred in the past day.

On startup, the manager looks at these wants and provides a random distribution of work across slots.  The day is divided into slots evenly distributed so there is a slot for each want in the schedule.  It will run work for the slot at an offset from the start of the slot, based on the place this node has in the sorted list of currently active nodes.  So if each node is configured with the same total number of wants, work will be synchronised to have limited overlapping work within the cluster.

When, on a node, a scheduled piece of work comes due, the `riak_kv_ttaaefs_manager` will start an `aae_exchange` to run the work between the two clusters (using the peer configuration to reach the remote cluster).  Once the work is finished, it will schedule the next piece of work - unless the start time for the next piece of work has already passed, in which case the next work is skipped.  When all the work in the schedule is complete, a new schedule is calculated from the wants.

When starting an `aae_exchange` the `riak_kv_ttaaefs_manager` must pass in a repair function.  This function will compare clocks from identified discrepancies, and where the source cluster is ahead of the sink, send the `{Bucket, Key, Clock, to_fetch}` tuple to a configured queue name on `riak_kv_replrtq_src`.  These queued entries will then be replicated through being fetched by the `riak_kv_replrtq_snk` workers, although this will only occur when there is no higher priority work to replicate i.e. real-time replication events prompted by locally co-ordinated PUTs.



