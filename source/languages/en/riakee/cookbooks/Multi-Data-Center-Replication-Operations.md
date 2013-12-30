---
title: "Multi Data Center Replication: Operations"
project: riakee
version: 1.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, operator]
---

## The `riak-repl` Command
Replication is controlled by the `riak-repl` command. Usage:

#### `add-listener`

Adds a listener (primary) to the given node, IP address, and port.

  * *Syntax:* `riak-repl add-listener <nodename> <listen_ip> <port>`
  * *Example:* `riak-repl add-listener riak@10.0.1.156 10.0.1.156 9010`

{{#1.2.1+}}
#### `add-nat-listener`

Adds a NAT-aware listener (primary) to the given node, IP address, port, NAT IP, and NAT port. If a non-NAT listener already exists with the same internal IP and port, it is “upgraded” to a NAT Listener.

  * *Syntax:* `riak-repl add-nat-listener <nodename> <internal_ip> <internal_port> <nat_ip> <nat_port>`
  * *Example:* `riak-repl add-nat-listener riak@10.0.1.156 10.0.1.156 9010 50.16.238.123 9010`
{{/1.2.1+}}

#### `del-listener`
Removes and shuts down a listener (primary) on the given node, IP address, and port.

  * *Syntax:* `riak-repl del-listener <nodename> <listen_ip> <port>`
  * *Example:* `riak-repl del-listener riak@10.0.1.156 10.0.1.156 9010`

#### `add-site`
Adds a site (secondary) to the local node, connecting to the specified listener.

  * *Syntax:* `riak-repl add-site <ipaddr> <portnum> <sitename>`
  * *Example:* `riak-repl add-site 10.0.1.156 9010 newyork`

#### `del-site`
Removes a site (secondary) from the local node by name.

  * *Syntax:* `riak-repl del-site <sitename>`
  * *Example:* `riak-repl del-site newyork`

#### `status`
Gets status information about replication. Reports counts on how much data has been transmitted, transfer rates, message queue lengths of clients and servers, number of fullsync operations, and connection status. This command only displays useful information on the leader node.

  * *Syntax:* `riak-repl status`

#### `start-fullsync`
Manually initiates a fullsync operation with connected sites.

  * *Syntax:* `riak-repl start-fullsync`

#### `cancel-fullsync`
Cancels any fullsync operations in progress. If a partition is in progress, synchronization will stop after that partition completes. During cancellation, `riak-repl status` will show `cancelled` in the status.

  * *Syntax:* `riak-repl cancel-fullsync`

#### `pause-fullsync`
Pauses any fullsync operations in progress. If a partition is in progress, synchronization will pause after that partition completes. While paused, `riak-repl status` will show `paused` in the status information. Fullsync may be cancelled while paused.

  * *Syntax:* `riak-repl pause-fullsync`

#### `resume-fullsync`
Resumes any fullsync operations that were paused. If a fullsync operation was running at the time of the pause, the next partition will be synchronized. If not, it will wait until the next `start-fullsync` command or `fullsync_interval`.

  * *Syntax:* `riak-repl resume-fullsync`


## `riak-repl` Status Output

The following definitions describe the output of `riak-repl status`. Please note that many of these statistics will only appear on the current leader node.

**All counts will be reset to 0 upon restarting Riak Enterprise.**

Field | Description
------|------------
`listener_[nodeid]` | Defines a replication listener that is running on node `[nodeid]`
`[sitename]_ips` | Defines a replication site
`client_bytes_recv` | The total number of bytes the client has received since the server has been started
`client_bytes_sent` | The total number of bytes sent to all connected sites
`client_connect_errors` | The number of TCP/IP connection errors
`client_connects` | A count of the number of site connections made to this node
`client_redirect` | If a client connects to a non-leader node, it will be redirected to a leader node
`client_rx_kbps` | A snapshot of the client (site)-received kilobits/second taken once a minute. The past 8 snapshots are stored in this list. Newest snapshots appear on the left side of the list.
`client_tx_kbps` | A snapshot of the client (site)-sent kilobits/second taken once a minute. The past 8 snapshots are stored in this list. Newest snapshots appear on the left side of the list.
`elections_elected` | If the replication leader node becomes unresponsive or unavailable, a new leader node in the cluster will be elected
`elections_leader_changed` | The number of times a Riak node has surrendered leadership
`objects_dropped_no_clients` | If the realtime replication work queue is full and there aren't any clients to receive objects, then objects will be dropped from the queue. These objects will be synchronized during a fullsync operation.
`objects_dropped_no_leader` | If a client (site) cannot connect to a leader, objects will be dropped during realtime replication
`objects_forwarded` | The number of Riak objects forwarded to the leader the participate in replication. *Please note that this value will only be accurate on a non-leader node.*
`objects_sent` | The number of objects sent via realtime replication
`server_bytes_recv` | The total number of bytes the server (listener) has received
`server_bytes_sent` | The total number of bytes the server (listener) has sent
`server_connect_errors` | The number of listener to site connection errors
`server_connects` | The number of times the listener connects to the client site
`server_fullsyncs` | The number of fullsync operations that have occurred since the server was started
`server_rx_kbps` | A snapshot of the server (listener) received kilobits/second taken once a minute. The past 8 snapshots are stored in this list. Newest snapshots appear on the left side of the list.
`server_tx_kbps` | A snapshot of the server (listener) sent kilobits/second taken once a minute. The past 8 snapshots are stored in this list. Newest snapshots appear on the left side of the list.
`leader` | Which node is the current leader of the cluster
`local_leader_message_queue_len` | The length of the object queue on the leader
`local_leader_heap_size `| The amount of memory the leader is using
`client_stats` | See <a href="/cookbooks/Multi-Data-Center-Replication-Operations/#Client-Statistics" class="riakee">Client Statistics</a>
`server_stats` | See <a href="/cookbooks/Multi-Data-Center-Replication-Operations/#Server-Statistics" class="riakee">Server Statistics</a>


## Client Statistics

Field | Description
------|------------
`node` | A unique ID for the Riak node on which the client (site) is running
`site` | The connected site name configured with `riak-repl add-site`
`strategy` | A replication strategy defines an implementation of the Riak Replication protocol. Valid values: `keylist`, `syncv1`
`fullsync_worker` | The Erlang process ID of the fullsync worker
`waiting_to_retry` | The listeners currently waiting to retry replication after a failure
`connected` | A list of connected clients<ul><li>**`connected`** The IP address and port of a connected client (site)</li><li>**`cluster_name`** The name of the connected client (site)</li><li>**`connecting`** The PID, IP address, and port of a client currently establishing a connection</li></ul>
`state` | State shows what the current replication strategy is currently processing. The following definitions appear in the status output if keylist strategy is being used. They can be used by Basho support to identify replication issues.<ul><li>**`request_partition`**</li><li>**`wait_for_fullsync`**</li><li>**`send_keylist`**</li><li>**`wait_ack`**</li></ul>

## Server Statistics

Field | Description
------|------------
`node`  | A unique ID for the Riak node on which the server (listener) is running
`site` | The connected site name configured with `riak-repl add-site`
`strategy` | A replication strategy defines an implementation of the Riak Replication protocol. Valid values: `keylist`, `syncv1`
`fullsync_worker` | The Erlang process ID of the fullsync worker
`bounded_queue` | See <a href="/cookbooks/Multi-Data-Center-Replication-Operations/#Bounded-Queue" class="riakee">Bounded Queue</a>
`state` | State shows what the current replication strategy is currently processing. The following definitions appear in the status output if keylist strategy is being used. They can be used by Basho support to identify replication issues.<ul><li>**`wait_for_partition`**</li><li>**`build_keylist`**</li><li>**`wait_keylist`**</li><li>**`diff_bloom`**</li><li>**`diff_keylist`**</li></ul>
`message_queue_len` | The number of Erlang messages that are waiting to be process by the server

## Keylist Strategy

These similar fields are under both `keylist_server` and `keylist_client` fields. Any differences are described in the table.

Field | Description
------|------------
`fullsync` | On the client, the number of partitions that remain to be processed. On the server, the partition currently being processed by fullsync replication.
`partition_start` | The number of elapsed seconds since replication has started on a given partition
`stage_start` | The number of elapsed seconds since replication has started on a given stage
`get_pool_size` | The number of Riak get finite state workers available to process requests

{{#1.2.1+}}

## Bounded Queue

The bounded queue is responsible for holding objects that are waiting to participate in realtime replication. Please see the [[Riak EE MDC Replication Configuration|Multi-Data-Center Replication: Configuration]] guide for more information.

Field | Description
------|------------
`queue_pid` | The Erlang process ID of the bounded queue
`dropped_count` | The number of objects that failed to be enqueued in the bounded queue due to the queue being full. *These objects will be replicated during the next fullsync operation*.
`queue_length` | The number of Riak objects currently in the bounded queue
`queue_byte_size` | The size of all objects currently in the queue
`queue_max_size` | The number of bytes the queue can hold before objects are dropped. *These objects will be replicated during the next fullsync operation*.
`queue_percentage` | The percentage of the queue that is full
`queue_pending` | The current count of "in-flight" objects we've sent that the client has not acknowledged
`queue_max_pending` | The maximum number of objects that can be "in flight" before we refuse to send any more.

{{/1.2.1+}}
