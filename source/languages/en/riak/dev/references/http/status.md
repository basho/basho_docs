---
title: HTTP Status
project: riak
version: 0.10.0+
document: api
toc: true
audience: advanced
keywords: [api, http]
group_by: "Server Operations"
moved: {
  '1.4.0-': '/references/apis/http/HTTP-Status'
}
---

Reports about the performance and configuration of the Riak node to which it was requested. You must have the `{riak_kv_stat,true}` configuration setting in app.config for this endpoint to be active. This is equivalent to the [[riak-admin status|Command-Line Tools#status]] command.

## Request

```bash
GET /stats
```

Important headers:

* `Accept` - determines whether the response will be formatted in `application/json` or `text/plain`.

## Response

Normal status codes:
* `200 OK`

Typical error codes:
* `404 Not Found` - if `riak_kv_stat` is not enabled

Important headers:
* `Content-Type` - `application/json` or `text/plain` (JSON with added line-breaks)

## Example

```curl
$ curl -v http://127.0.0.1:8098/stats -H "Accept: text/plain"
* About to connect() to 127.0.0.1 port 8098 (#0)
*   Trying 127.0.0.1... connected
* Connected to 127.0.0.1 (127.0.0.1) port 8098 (#0)
> GET /stats HTTP/1.1
> User-Agent: curl/7.19.7 (universal-apple-darwin10.0) libcurl/7.19.7 OpenSSL/0.9.8l zlib/1.2.3
> Host: 127.0.0.1:8098
> Accept: text/plain
>
< HTTP/1.1 200 OK
< Vary: Accept, Accept-Encoding
< Server: MochiWeb/1.1 WebMachine/1.9.0 (participate in the frantic)
< Date: Fri, 30 Sep 2011 15:24:35 GMT
< Content-Type: text/plain
< Content-Length: 2102
<
{
    "vnode_gets": 0,
    "vnode_puts": 0,
    "read_repairs": 0,
    "vnode_gets_total": 0,
    "vnode_puts_total": 0,
    "node_gets": 0,
    "node_gets_total": 0,
    "node_get_fsm_time_mean": "undefined",
    "node_get_fsm_time_median": "undefined",
    "node_get_fsm_time_95": "undefined",
    "node_get_fsm_time_99": "undefined",
    "node_get_fsm_time_100": "undefined",
    "node_puts": 0,
    "node_puts_total": 0,
    "node_put_fsm_time_mean": "undefined",
    "node_put_fsm_time_median": "undefined",
    "node_put_fsm_time_95": "undefined",
    "node_put_fsm_time_99": "undefined",
    "node_put_fsm_time_100": "undefined",
    "read_repairs_total": 0,
    "cpu_nprocs": 84,
    "cpu_avg1": 251,
    "cpu_avg5": 174,
    "cpu_avg15": 110,
    "mem_total": 7946684000.0,
    "mem_allocated": 4340880000.0,
    "nodename": "riak@127.0.0.1",
    "connected_nodes": [

    ],
    "sys_driver_version": "1.5",
    "sys_global_heaps_size": 0,
    "sys_heap_type": "private",
    "sys_logical_processors": 2,
    "sys_otp_release": "R13B04",
    "sys_process_count": 189,
    "sys_smp_support": true,
    "sys_system_version": "Erlang R13B04 (erts-5.7.5) [[source]] [[64-bit]] [[smp:2:2]] [[rq:2]] [[async-threads:5]] [[hipe]] [[kernel-poll:true]]",
    "sys_system_architecture": "i386-apple-darwin10.3.0",
    "sys_threads_enabled": true,
    "sys_thread_pool_size": 5,
    "sys_wordsize": 8,
    "ring_members": [
        "riak@127.0.0.1"
    ],
    "ring_num_partitions": 64,
    "ring_ownership": "[{'riak@127.0.0.1',64}]",
    "ring_creation_size": 64,
    "storage_backend": "riak_kv_bitcask_backend",
    "pbc_connects_total": 0,
    "pbc_connects": 0,
    "pbc_active": 0,
    "riak_kv_version": "0.11.0",
    "riak_core_version": "0.11.0",
    "bitcask_version": "1.0.1",
    "luke_version": "0.1",
    "webmachine_version": "1.7.1",
    "mochiweb_version": "1.7.1",
    "erlang_js_version": "0.4",
    "runtime_tools_version": "1.8.3",
    "crypto_version": "1.6.4",
    "os_mon_version": "2.2.5",
    "sasl_version": "2.1.9",
    "stdlib_version": "1.16.5",
    "kernel_version": "2.13.5"
}
* Connection #0 to host 127.0.0.1 left intact
* Closing connection #0
```

## Output Explanation

The output of `/stats` contains a number of configuration and performance details. An explanation of these details follows.


## CPU and Memory

CPU statistics are taken directly from Erlang's cpu\_sup module.  Documentation for which can be found at [[ErlDocs: cpu_sup|http://erldocs.com/R14B04/os_mon/cpu_sup.html]].

* `cpu_nprocs`: Number of operating system processes
* `cpu_avg1`: The average number of active processes for the last 1 minute (equivalent to top(1) command's load average when divided by 256()
* `cpu_avg5`: The average number of active processes for the last 5 minutes (equivalent to top(1) command's load average when divided by 256()
* `cpu_avg15`: The average number of active processes for the last 15 minutes (equivalent to top(1) command's load average when divided by 256()


Memory statistics are taken directly from the Erlang virtual machine. Documentation for which can be found at [[ErlDocs: Memory|http://erldocs.com/R14B04/erts/erlang.html?i=0&search=erlang:memory#memory/0]].

* `memory_total`: Total allocated memory (sum of processes and system)
* `memory_processes`: Total amount of memory allocated for Erlang processes
* `memory_processes_used`: Total amount of memory used by Erlang processes
* `memory_system`: Total allocated memory that is not directly related to an Erlang process
* `memory_atom`: Total amount of memory currently allocated for atom storage
* `memory_atom_used`: Total amount of memory currently used for atom storage
* `memory_binary`: Total amount of memory used for binaries
* `memory_code`: Total amount of memory allocated for Erlang code
* `memory_ets`: Total memory allocated for Erlang Term Storage
* `mem_total`: Total available system memory
* `mem_allocated`: Total memory allocated for this node


## Node, Cluster & System

* `nodename`: The name of the node that produced the stats output
* `connected_nodes`: List of nodes connected to this node
* `read_repairs`: Number of read repair operations this this node has coordinated in the last minute
* `read_repairs_total`: Number of read repair operations this this node has coordinated since node was started
* `coord_redirs_total`: Number of requests this node has redirected to other nodes for coordination since node was started
* `ring_members`: List of nodes which are members of the ring
* `ring_num_partitions`: Number of partitions in the ring
* `ring_ownership`: List of all nodes in the ring and their associated partition ownership
* `ring_creation_size`: Number of partitions this node is configured to own
* `ignored_gossip_total`: Total number of ignored gossip messages since node was started
* `handoff_timeouts`: Number of handoff timeouts encountered by this node
* `precommit_fail`: Number of pre commit hook failures
* `postcommit_fail`: Number of post commit hook failures
* `sys_driver_version`: String representing the Erlang driver version in use by the runtime system
* `sys_global_heaps_size`: Current size of the shared global heap
* `sys_heap_type`: String representing the heap type in use (one of private, shared, hybrid)
* `sys_logical_processors`: Number of logical processors available on the system
* `sys_otp_release`: Erlang OTP release version in use on the node
* `sys_process_count`: Number of processes existing on this node
* `sys_smp_support`: Boolean value representing whether symmetric multi-processing (SMP) is available
* `sys_system_version`: Detailed Erlang version information
* `sys_system_architecture`: The node operating system and hardware architecture
* `sys_threads_enabled`: Boolean value representing whether threads are enabled
* `sys_thread_pool_size`: Number of threads in the asynchronous thread pool
* `sys_wordsize`: Size of Erlang term words in bytes as an integer, for examples, on 32-bit architectures 4 is returned and on 64-bit architectures 8 is returned
* `storage_backend`: Name of the active storage backend
* `pbc_connects_total`: Number of protocol buffers connections since node was started
* `pbc_connects`: Number of protocol buffers connections in the last minute
* `pbc_active`: Number of active protocol buffers connections
* `ssl_version`: Version of secure sockets layer (SSL) application in use
* `public_key_version`: Version of public key application in use
* `runtime_tools_version`: Version of runtime tools application in use
* `basho_stats_version`: Version of Basho stats application in use
* `riak_search_version`: Version of Riak Search application in use
* `riak_kv_version`: Version of Riak KV application in use
* `bitcask_version`: Version of Bitcask backend application in use
* `luke_version`: Version of Luke application in use
* `erlang_js_version`: Version of Erlang JS application in use
* `mochiweb_version`: Version of MochiWeb application in use
* `inets_version`: Version of Inets application in use
* `riak_pipe_version`: Version of Riak Pipe application in use
* `merge_index_version`: Version of Merge Index application in use
* `cluster_info_version`: Version of Cluster Information application in use
* `basho_metrics_version`: Version of Basho Metrics application in use
* `riak_control_version`: Version of Riak Control application in use
* `riak_core_version`: Version of Riak Core application in use
* `lager_version`: Version of Lager application in use
* `riak_sysmon_version`: Version of Riak System Monitor application in use
* `webmachine_version`: Version of Webmachine application in use
* `crypto_version`: Version of Cryptography application in use
* `os_mon_version`: Version of Operating System Monitor application in use
* `sasl_version`: Version of SASL application in use
* `stdlib_version`: Version of Standard Library application in use
* `kernel_version`: Version of Kernel application in use

### Node & VNode Counters

* `vnode_gets`: Number of GET operations coordinated by vnodes on this node within the last minute
* `vnode_puts`: Number of PUT operations coordinated by vnodes on this node within the last minute
* `vnode_gets_total`: Number of GET operations coordinated by vnodes on this node since node was started
* `vnode_puts_total`: Number of PUT operations coordinated by vnodes on this node since node was started
* `node_gets`: Combined number of local and non-local GET operations coordinated by this node in the last minute
* `node_puts`: Combined number of local and non-local PUT operations coordinated by this node in the last minute
* `node_gets_total`: Combined number of local and non-local GET operations coordinated by this node since node was started
* `node_puts_total`: Combined number of local and non-local PUT operations coordinated by this node since node was started

### Microsecond Timers

* `node_get_fsm_time_mean`: Mean time between reception of client GET request and subsequent response to client
* `node_get_fsm_time_median`: Median time between reception of client GET request and subsequent response to client
* `node_get_fsm_time_95`: 95th percentile time between reception of client GET request and subsequent response to client
* `node_get_fsm_time_99` 99th percentile time between reception of client GET request and subsequent response to client
* `node_get_fsm_time_100` 100th percentile time between reception of client GET request and subsequent response to client
* `node_put_fsm_time_mean`: Mean time between reception of client PUT request and subsequent response to client
* `node_put_fsm_time_median`: Median time between reception of client PUT request and subsequent response to client
* `node_put_fsm_time_95`: 95th percentile time between reception of client PUT request and subsequent response to client
* `node_put_fsm_time_99`: 99th percentile time between reception of client PUT request and subsequent response to client
* `node_put_fsm_time_100`: 100th percentile time between reception of client PUT request and subsequent response to client

### Object, Index & Sibling Metrics

* `node_get_fsm_objsize_mean`: Mean object size encountered by this node within the last minute
* `node_get_fsm_objsize_median`: Median object size encountered by this node within the last minute
* `node_get_fsm_objsize_95`: 95th percentile object size encountered by this node within the last minute
* `node_get_fsm_objsize_99`: 99th percentile object size encountered by this node within the last minute
* `node_get_fsm_objsize_100` 100th percentile object size encountered by this node within the last minute
* `vnode_index_reads`: Number of vnode index read operations performed in the last minute
* `vnode_index_writes`: Number of vnode index write operations performed in the last minute
* `vnode_index_deletes`: Number of vnode index delete operations performed in the last minute
* `vnode_index_reads_total`: Number of vnode index read operations performed since the node was started
* `vnode_index_writes_total`: Number of vnode index write operations performed since the node was started
* `vnode_index_deletes_total`: Number of vnode index delete operations performed since the node was started
* `node_get_fsm_siblings_mean`: Mean number of siblings encountered during all GET operations by this node within the last minute
* `node_get_fsm_siblings_median`: Median number of siblings encountered during all GET operations by this node within the last minute
* `node_get_fsm_siblings_95`: 95th percentile of siblings encountered during all GET operations by this node within the last minute
* `node_get_fsm_siblings_99`: 99th percentile of siblings encountered during all GET operations by this node within the last minute
* `node_get_fsm_siblings_100`: 100th percentile of siblings encountered during all GET operations by this node within the last minute

{{#1.2.0+}}

### Pipeline Metrics

The following metrics from from riak_pipe are generated during MapReduce operations.

* `pipeline_active` The number of pipelines active in the last 60 seconds
* `pipeline_create_count` The total number of pipelines created since the node was started
* `pipeline_create_error_count` The total number of pipeline creation errors since the node was started
* `pipeline_create_one` The number of pipelines created in the last 60 seconds
* `pipeline_create_error_one` The number of pipeline creation errors in the last 60 seconds

{{/1.2.0+}}
