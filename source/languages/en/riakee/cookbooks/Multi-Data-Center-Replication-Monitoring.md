---
title: "Multi Data Center Replication: Monitoring"
project: riakee
version: 1.3.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, operator, bnw, monitoring]
---

Monitoring Riak's realtime replication allows you to identify, trend, and alert during times when replication is halted or delayed. Issues or delays in replication can be caused by:

- Sudden increases or spikes in PUT traffic
- Network connectivity issues or outages
- Errors experienced in Riak

Identification and trending of issues or delays in realtime replication is important for identifying a root cause, while alerting is important for addressing any SLA impacting issues or delays. We recommend combining the below when monitoring Riak's realtime replication:

- Monitoring Riak's replication status output, from either `riak-repl status` or the HTTP `/riak-repl/stats` endpoint
- Using canary (test) objects to test replication and establish trip times from `source` to `sink` clusters

### Statistics

Through the monitoring and graphing of realtime replication statistics, the following questions can be answered:

- Is the realtime replication queue backed up?
- Have any errors occurred on either the `source` or `sink` cluster, that may have impacted realtime replication?
- Have any objects been dropped from the realtime queue, meaning their replication to the `sink` cluster is unconfirmed?

---

#### Is the realtime replication queue backed up?

Identifying times when the realtime replication queue experiences increases in the number of `pending` objects can help identify problems with realtime replication, or identify times where replication becomes overloaded due to increases in traffic. The `pending` statistic, found under the `realtime_queue_stats` section of `riak-repl status` should be monitored and graphed. Graphing this statistic allows for increases in the number of `pending` objects to be trended. Any trend, indicating predictable increases in the number of `pending` objects, can help determine if any tuning or cluster expansion is required. Unpredictable increases can help identify unexpected load, network or system errors, or Riak errors. 

#### Have any errors occurred on either the `source` or `sink` cluster?

Errors experienced on either the `source` or `sink` cluster can result in failure to replicate object(s) via realtime replication. The top level `rt_dirty` statistic in `riak-repl status` indicates if such an error has occurred, and how many times. This statistic only tracks errors, and does not definitively indicate that an object was not successfully replicated. It is suggested that any time `rt_dirty` is non-zero, a fullsync should be performed. Once a fullsync successfully completes, `rt_dirty` is reset to 0. While the size of `rt_dirty` can quantify the number of errors have have occurred and should be graphed, any non-zero value should also alert so that a fullsync can be performed (if not regularly scheduled). Like realtime queue back ups, trends in `rt_dirty` can reveal problems with the network, system, or Riak.

#### Have any objects been dropped from the realtime queue?

The realtime replication queue will drop objects when the queue is full, with the dropped object(s) being the last (oldest) in the queue. Each time an object is dropped the `drops` statistic, found under the `realtime_queue_stats` section of `riak-repl status`, is incremented. It is safe to assume that an object dropped from the queue has not been replicated and a fullsync is required. A dropped object can indicate a halt or delay in replication, or indicate that the realtime queue is overloaded. In cases of high load, tunings to the maximum size of the queue (displayed in the `realtime_queue_stats` section of `riak-repl status` as `max_bytes`) can be made to accomodate a particular usage pattern of expected high load.

---

Although the above statistics were highlighted for answering specific questions, other statistics can also be helpful in diagnosing issues with realtime replication.  We recommend graphing any statistic that is reported as a number.  While their values and trends may not answer common questions or those we've highlighted here, they may be important when investigating issues in the future. Other questions, that cannot be answered from statistics, can be answered through the use of canary objects.

### Canary Objects

A canary or test object is an object that exists in your environment that you can guarantee has no actors other than your test infrastructure. This allows for the object to have predictable states where an unexpected change or lack of change in state can indicate an issue.

The general process for using canary objects to test realtime replication is:

- Perform a GET for your canary object on both your `source` and `sink` clusters, noting their states. The state of the object in each cluster can be referred to as state `S0`, or the object's initial state.
- PUT an update for your canary object to the `source` cluster, updating the state of the object to the next state, `S1`.
- Perform a GET for your canary on the sink cluster, comparing the state of the object on the `source` cluster to the state of the object on the `sink` cluster.

By expanding upon the general process above, the following questions can be answered:

- Is a backed up realtime replication queue still replicating objects within a defined SLA?
- How long is it taking for objects to be replicated from the `source` cluster to the `sink` cluster?

---

#### Is a backed up realtime replication queue still replicating objects within a defined SLA?

Buidling on the final step of the general process, we can determine if our objects are being replicated from the `source` cluster to the `sink` cluster within a certain SLA time period by adding the following steps:

- If the state of the object on the `source` cluster is not equal to the state of the object on the `sink` cluster repeat step 3 until an SLA time threshold is exceeded.
- If the SLA time threshold is exceeded, alert that replication is not meeting the necessary SLA.

#### How long is it taking for objects to be replicated from the `source` cluster to the `sink` cluster?

Getting a rough estimate of how long it takes an object PUT to a `source` cluster to be replicated to a `sink` cluster get be done by either:

- Comparing the time the object was PUT to the `source` with the time the states of the object in the `source` and `sink` were equivalent
- Comparing the timestamps of the object on the `source` and `sink` when the states are equivalent

These are rough estimates, as neither method is 100% accurate. The first method relies on a timestamp for a GET and subsequent successful comparison, which means the object was replicated prior to that timestamp.  The second method relies on the system clocks of two different machines, which may not be in sync.

---

It's important to note that each node in a cluster has its own realtime replication queue. The general process needs to be applied to every node in the `source` cluster, with a variety of canary objects and states, to get a complete picture of realtime replication between two clusters.
