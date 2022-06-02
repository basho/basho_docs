---
title: "Multi-Datacenter Replication Reference: Statistics"
description: ""
project: "riak_kv"
project_version: 2.9.1
menu:
  riak_kv-2.9.1:
    name: "Statistics"
    identifier: "managing_ref_mdc_stats"
    weight: 100
    parent: "managing_ref_mdc"
toc: true
aliases:
  - /riak/2.9.1/ops/mdc/statistics
  - /riak/kv/2.9.1/ops/mdc/statistics
---

The following definitions describe the output of `riak-repl status`.
Both Version 2 and Version 3 Replication statistics can be obtained
using the `riak-repl status` command.

There are two things that you should note:

1. Many of these statistics will appear only on the current
   leader node
2. The counts for all statistics will be reset to 0 upon restarting Riak
   Riak unless otherwise noted

Field | Description
:-----|:----------
`cluster_leader` | Which node is the current leader of the cluster
`connected_clusters` | A list of all sink clusters to which this source is connected

## Performance

The `riak-repl status` command should not be executed more than once a
minute, as statistics are recalculated every time the command is
executed, and some statistics require network communication between
nodes. This performance note also applies to the HTTP `/riak-repl/stats`
endpoint.

## Realtime Replication Statistics

Statistics for both the source or sink sides of realtime replication.
These values can be found under either `sources.source_stats` or
`sinks.sink_stats`.

Field | Description
------|------------
`realtime_enabled` | A list of all realtime sinks that are enabled
`realtime_started` | A list of all realtime sinks that are started
`rt_dirty` | The number of errors detected that can prevent objects from being replicated via realtime. These include errors on the source or sink connection, or realtime queue overload resulting in objects being dropped from the queue. *This value will persist across restarts until a fullsync is complete.*
`rt_sink_errors` | A sink error has been detected on the source node. This value will be reset to 0 after a node restarts.
`rt_sink_connected_to.source_drops` |  The number of dropped put transfers from the perspective of the sink cluster
`rt_source_errors` | A source error has been detected on the source node. This value will be reset to 0 after a node restarts.

Field | Description
------|------------
`rt_source_connected_to` | The name of the sink cluster to which the source cluster is connected
`rt_sink_connected_to` | The name of the source cluster to which the sink cluster is connected
`connected` | If `true`, then the source is connected to a sink (or vice versa)
`objects` | The number of realtime replication objects that have been successfully transmitted to the sink cluster
`sent_seq` | The last realtime queue sequence number that has been transmitted
`acked_seq` | The last realtime queue sequence number that has been acknowledged
`expect_seq` | The next realtime queue sequence number that is expected
`hb_rtt` | Realtime replication heartbeat round-trip time in milliseconds, recorded on the replication source
`hb_last` | `{MegaSeconds, Seconds, MicroSeconds}` since a heartbeat message was received on the realtime sink


These values are under `realtime_queue_stats`.

Field | Description
------|------------
`bytes` | The size in bytes of all objects currently in the realtime queue
`consumers` | A list of source consumers of the realtime queue
`consumers.<clustername>.drops` | The number of dropped realtime sync put transfers per sink cluster, from the perspective of the source cluster ("dropped" in this context meaning either that the outgoing data queue was full or that there was a connection error)
`drops` | The number of objects dropped from the realtime queue as the result of the queue being full or other errors
`errs` | The number of errors while pushing/popping from the realtime queue
`overload_drops` | The number of put transfers that have been dropped due to an overload of the message queue of the Erlang process responsible for processing outgoing transfers
`pending` | The number of objects waiting to be sent to the sink cluster
`sinkclustername` | A consumer of the realtime queue
`unacked` | The number of objects waiting to be acknowledged by a queue consumer


## Fullsync Replication Statistics

Field | Description
------|------------
`fullsync_enabled` | A list of all sinks that are enabled
`fullsync_running` | A list of all sinks that are running
`server_fullsyncs` | The number of fullsync operations that have occurred since the server was started
`fullsyncs_completed` | The number of fullsyncs that have been completed to the specified sink cluster.
`fullsync_start_time` | The time the current fullsink to the specified cluster began.
`last_fullsync_duration `| The duration (in seconds) of the last completed fullsync.

If this cluster is acting as a **source**, the `fullsync_coordinator` field returns a list of `{<sink_clustername>:<fullsync_stats>}`. If this cluster is acting as a **sink**, the `fullsync_coordinator_srv` field returns a list of `{<LocalIP:Port>:<fullsync_coordinator_srv_stats>}`.

Those fields are described in the following tables.

Field | Description
------|------------
`cluster` | The name of the sink cluster
`queued` | The number of partitions that are waiting for an available process
`in_progress` | The number of partitions that are being synced
`starting` | The number of partitions connecting to remote cluster
`successful_exits` | The number of partitions successfully synced. When completed, this will be the same number as total number of partitions in the ring.
`error_exits` | If a sync failed or was aborted, the partition will be queued again and try again later
`running_stats` | `[{<PID>, <stats>},…]` Any running sync processes are listed here, and described in the table below
`socket` | See [Socket Statistics](#socket-statistics)
`fullsync_suggested` | Realtime replication errors occurred on these nodes, a fullsync is suggested
`fullsync_suggested_during_fs` | Realtime replication errors occurred on these nodes while a fullsync is already in progress. A fullsync is suggested after the current fullsync completes. These value will be moved to the `fullsync_suggested` value when the current fullsync completes.
`socket` | `{peername: <RemoteIP:Port>`, `sockname: <LocalIP:Port>}`

The `running_stats` field contains the following fields.

Field | Description
------|------------
`node` | The local cluster source node currently participating in fullsync replication
`site` | The name of the sink cluster. *Warning: This will be renamed in future versions of Riak*.
`strategy` | The strategy that fulfills fullsync replication. In previous versions of replication, different values could be configured. This value could be changed depending on your replication needs.
`fullsync_worker` | The Erlang process id of the fullsync worker.
`socket` | See [Socket Statistics](#socket-statistics)
`state` | The current state of fullsync replication. This can be used by Basho support to identify replication issues.<ul><li>**`wait_for_partition`**</li><li>**`build_keylist`**</li><li>**`wait_keylist`**</li><li>**`diff_bloom`**</li><li>**`diff_keylist`**</li></ul>
`fullsync` | The partition that is currently being synchronized with the sink cluster
`partition_start` | Elapsed time in seconds since the *fullsync* partition started replication to a sink
`stage_start` | Elapsed time in seconds since the `state` started running on the source
`get_pool_size` | The number of workers that are used to read data from Riak during a fullsync

## Socket Statistics

Many sections of the status output include a `socket` section. A reading is taken once every 10 seconds, and the last 7 readings are stored.

Field | Description
------|------------
`peername` | `<ip:port>` The address and port for the other end of a connection
`recv_avg` | The average size of packets in bytes received to the socket
`recv_cnt` | The number of packets received by the socket
`recv_dvi` | The average packet size deviation in bytes received by the socket
`recv_kbps` | Socket kilobits/second received
`recv_max` | Size of the largest packet in bytes received to the socket
`send_cnt` | Number of packets sent from the socket
`send_kbps` | Socket kilobits/second sent
`send_pend` | The number of bytes in the Erlang VM to be sent over the socket
`sockname` | `<host:port>` The address and port for "this end" of the connection

## Version 2 Replication Statistics

The following definitions describe the output of `riak-repl status`.
Please note that many of these statistics will only appear on the
current leader node.

**Note**: All counts will be reset to 0 upon restarting Riak.

Field | Description
------|------------
`listener_[nodeid]` | Defines a replication listener (primary) that is running on node `[nodeid]`
`[sitename]_ips` | Defines a replication skin
`client_bytes_recv` | The total number of bytes the client has received since the server has been started
`client_bytes_sent` | The total number of bytes sent to all connected secondaries
`client_connect_errors` | The number of TCP/IP connection errors
`client_connects` | A count of the number of sink connections made to this node.
`client_redirect` | If a client connects to a non-leader node, it will be redirected to a leader node
`client_rx_kbps` | A snapshot of the sink received kilobits/second taken once a minute. The past 8 snapshots are stored in this list. Newest snapshots appear on the left side of the list.
`client_tx_kbps` | A snapshot of the sink sent kilobits/second taken once a minute. The past 8 snapshots are stored in this list. Newest snapshots appear on the left side of the list.
`elections_elected` | If the replication leader node becomes unresponsive or unavailable, a new leader node in the cluster will be elected
`elections_leader_changed` | The number of times a Riak node has surrendered leadership
`objects_dropped_no_clients` | If the realtime replication work queue is full and there are no clients to receive objects, then objects will be dropped from the queue. These objects will be synchronized during a fullsync operation.
`objects_dropped_no_leader` | If a sink cannot connect to a leader, objects will be dropped during realtime replication
`objects_forwarded` | The number of Riak objects forwarded to the leader the participate in replication. *Please note that this value will only be accurate on a non-leader node*.
`objects_sent` | The number of objects sent via realtime replication
`server_bytes_recv` | The total number of bytes the primary has received
`server_bytes_sent` | The total number of bytes the primary has sent
`server_connect_errors` | The number of primary to sink connection errors
`server_connects` | The number of times the primary connects to the client sink
`server_rx_kbps` | A snapshot of the primary received kilobits/second taken once a minute. The past 8 snapshots are stored in this list. Newest snapshots appear on the left side of the list
`server_tx_kbps` | A snapshot of the primary sent kilobits/second taken once a minute. The past 8 snapshots are stored in this list. Newest snapshots appear on the left side of the list.
`leader` | Which node is the current leader of the cluster for Version 2 Replication
`local_leader_message_queue_len` | The length of the object queue on the leader
`local_leader_heap_size` | The amount of memory the leader is using
`client_stats` | See [Client Statistics](#client-statistics)
`server_stats` | See [Server Statistics](#server-statistics)

## Client Statistics

Field | Description
------|------------
`node` | A unique ID for the Riak node that the sink in running on
`site` | The connected site (sink) name. **Warning**: This will be renamed in a future version of Riak.
`strategy` | A replication strategy defines an implementation of the Riak Replication protocol. Valid values: `keylist`, `syncv1`.
`fullsync_worker` | The Erlang process ID of the fullsync worker
`waiting_to_retry` | The primaries currently waiting to retry replication after a failure
`connected` | A list of connected clients<ul><li>**`connected`** The IP address and port of a connected sink</li><li>**`cluster_name`** The name of the connected sink</li><li>**`connecting`** The PID, IP address, and port of a client currently establishing a connection</li></ul>
`state` | State shows what the current replication strategy is currently processing. The following definitions appear in the status output if keylist strategy is being used. They can be used by Basho support to identify replication issues.<ul><li>**`request_partition`**</li><li>**`wait_for_fullsync`**</li><li>**`send_keylist`**</li><li>**`wait_ack`**</li></ul>


## Server Statistics

Field | Description
------|------------
`node` | A unique ID for the Riak node that the source is running on
`site` | The connected site (sink) name configured with. *Warning: This will be renamed in a future version of Riak*.
`strategy` | A replication strategy defines an implementation of the Riak Replication protocol. Valid values: `keylist`, `syncv1`.
`fullsync_worker` | The Erlang process ID of the fullsync worker
`bounded_queue` | See [Bounded Queue](#bounded-queue)
`state` | State shows what the current replication strategy is currently processing. The following definitions appear in the status output if keylist strategy is being used. They can be used by Basho support to identify replication issues.<ul><li>**`wait_for_partition`**</li><li>**`build_keylist`**</li><li>**`wait_keylist`**</li><li>**`diff_bloom`**</li><li>**`diff_keylist`**</li></ul>
`message_queue_len` | The number of Erlang messages that are waiting to be processed by the server


## Bounded Queue

The bounded queue is responsible for holding objects that are waiting to
participate in realtime replication. Please see the [Riak V2 MDC Replication Configuration][config v2 mdc] or [Riak V3 MDC Replication Configuration][config v3 mdc] guides for
more information.

Field | Description
------|------------
`queue_pid` | The Erlang process ID of the bounded queue
`dropped_count` | The number of objects that failed to be enqueued in the bounded queue due to the queue being full. *These objects will be replicated during the next fullsync operation*.
`queue_length` | The number of Riak objects currently in the bounded queue
`queue_byte_size` | The size of all objects currently in the queue
`queue_max_size `| The number of bytes the queue can hold before objects are dropped. *These objects will be replicated during the next fullsync operation*.
`queue_percentage` | The percentage of the queue that is full
`queue_pending` | The current count of "in-flight" objects we've sent that the client has not acknowledged
`queue_max_pending` | The maximum number of objects that can be "in flight" before we refuse to send any more.


## Accessing Replication Web-Based Statistics

These stats can be accessed via the command line with the following
command:

```curl
curl -q http://127.0.0.1:8098/riak-repl/stats
```

A simple way to view formatted statistics is to use a command such as:

```curl
curl -q http://127.0.0.1:8098/riak-repl/stats | json_pp
```
