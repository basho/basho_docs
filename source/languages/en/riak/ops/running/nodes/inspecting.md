---
title: Inspecting a Node
project: riak
version: 0.10.0+
document: appendix
toc: true
audience: intermediate
keywords: [operator, status, riaknostic]
moved: {
    '1.4.0-': '/references/appendices/Inspecting-a-Node'
}
---

When inspection of a Riak node to gather metrics on performance or
potential issues is desired, a number of tools are available to help,
and are either included with Riak itself or made available through the
Riak community.

This guide provides starting points and details on some of the available
tools for inspecting a Riak node.

riak-admin status
-----------------

`riak-admin status` is a subcommand of the `riak-admin` command that is
included with every installation of Riak. The `status` subcommand
provides data related to the current operating status for a node. The output
of `riak-admin status` is categorized and detailed below.

Please note, for some counters such as node_get_fsm_objsize a minimum of
5 transactions is required for statistics to be generated.

### One-minute

One-minute Counters are data points delineating the number of times a
particular activity has occurred within the last minute on this node.

List of one-minute counters (13):

-   **node_gets**: Number of GETs coordinated by this node, including
    GETs to non-local vnodes
-   **node_puts**: Number of PUTs coordinated by this node, including
    PUTs to non-local vnodes
-   **vnode_gets**: Number of GET operations coordinated by vnodes
-   **vnode_puts**: Number of PUT operations coordinated by vnodes
-   **read_repairs**: Number of read repair operations this this node
    has coordinated
-   **vnode_index_refreshes**:
-   **vnode_index_reads**:
-   **vnode_index_writes**:
-   **vnode_index_writes_postings**:
-   **vnode_index_deletes**:
-   **vnode_index_deletes_postings**:
-   **pbc_active**:
-   **pbc_connects**:

### Totals

Total Counters are data points that represent the total number of times
a particular activity has occurred since this node was started.

List of total counters (15):

-   **node_gets_total**: Number of GETs coordinated by this node, including GETs to non-local vnodes
-   **node_puts_total**: Number of PUTs coordinated by this node, including PUTs to non-local vnodes
-   **vnode_gets_total**: Number of GETs coordinated by local vnodes
-   **vnode_puts_total**: Number of PUTS coordinated by local vnodes
-   **read_repairs_total**: Number of Read Repairs this node has
    coordinated
-   **coord_redirs_total**: Number of requests this node has redirected
    to other nodes for coordination
-   **vnode_index_refreshes_total**:
-   **vnode_index_reads_total**:
-   **vnode_index_writes_total**:
-   **vnode_index_writes_postings_total**:
-   **vnode_index_deletes_total**:
-   **vnode_index_deletes_postings_total**:
-   **pbc_connects_total**:
-   **precommit_fail**: Number of pre commit hook failures
-   **postcommit_fail**: Number of post commit hook failures

### FSM_Time

FSM_Time Counters represent the amount of time in microseconds required
to traverse the GET or PUT Finite State Machine code, offering a picture
of general node health. From your application's perspective, FSM_Time
effectively represents experienced latency. Mean, Median, and 95th-,
99th-, and 100th-percentile (Max) counters are displayed. These are
one-minute stats.

List of Counters (10):

-   **node_get_fsm_time_mean**: Mean time between reception of
    client GET request and subsequent response to client
-   **node_get_fsm_time_median**: Median time between reception of
    client GET request and subsequent response to client
-   **node_get_fsm_time_95**: 95th percentile time between reception
    of client GET request and subsequent response to client
-   **node_get_fsm_time_99** 99th percentile time between reception
    of client GET request and subsequent response to client
-   **node_get_fsm_time_100** 100th percentile time between
    reception of client GET request and subsequent response to client
-   **node_put_fsm_time_mean**: Mean time between reception of
    client PUT request and subsequent response to client
-   **node_put_fsm_time_median**: Median time between reception of
    client PUT request and subsequent response to client
-   **node_put_fsm_time_95**: 95th percentile time between reception
    of client PUT request and subsequent response to client
-   **node_put_fsm_time_99**: 99th percentile time between reception
    of client PUT request and subsequent response to client
-   **node_put_fsm_time_100**: 100th percentile time between
    reception of client PUT request and subsequent response to client

### GET_FSM_Siblings

GET_FSM_Sibling Stats offer a count of the number of siblings
encountered by this node on the occasion of a GET request. These are
one-minute stats.

Sample finite state machine sibling counters (5):

-   **node_get_fsm_siblings_mean**: Mean number of siblings
    encountered during all GET operations by this node within the last
    minute
-   **node_get_fsm_siblings_median**: Median number of siblings
    encountered during all GET operations by this node within the last
    minute
-   **node_get_fsm_siblings_95**: 95th percentile of siblings
    encountered during all GET operations by this node within the last
    minute
-   **node_get_fsm_siblings_99**: 99th percentile of siblings
    encountered during all GET operations by this node within the last
    minute
-   **node_get_fsm_siblings_100**: 100th percentile of siblings
    encountered during all GET operations by this node within the last
    minute

### GET_FSM_Objsize

GET_FSM_Objsize is a window on the sizes of objects flowing through
this node's GET_FSM. The size of an object is obtained by summing the
length of the bucket name, key, the serialized vector clock, the value,
and the serialized metadata of each sibling. GET_FSM_Objsize and
GET_FSM_Siblings are inextricably linked. These are one-minute stats.

Sample finite state machine object size counters (5):

-   **node_get_fsm_objsize_mean**: Mean object size encountered by
    this node within the last minute
-   **node_get_fsm_objsize_median**: Median object size encountered
    by this node within the last minute
-   **node_get_fsm_objsize_95**: 95th percentile object size
    encountered by this node within the last minute
-   **node_get_fsm_objsize_99**: 99th percentile object size
    encountered by this node within the last minute
-   **node_get_fsm_objsize_100** 100th percentile object size
    encountered by this node within the last minute

### General FSM

List of general FSM stats (20):

-   **index_fsm_create**:
-   **index_fsm_create_error**:
-   **index_fsm_active**:
-   **list_fsm_create**:
-   **list_fsm_create_error**:
-   **list_fsm_active**:
-   **node_get_fsm_active**:
-   **node_get_fsm_active_60s**:
-   **node_get_fsm_in_rate**:
-   **node_get_fsm_out_rate**:
-   **node_get_fsm_rejected**:
-   **node_get_fsm_rejected_60s**:
-   **node_get_fsm_rejected_total**:
-   **node_put_fsm_active**:
-   **node_put_fsm_active_60s**:
-   **node_put_fsm_in_rate**:
-   **node_put_fsm_out_rate**:
-   **node_put_fsm_rejected**:
-   **node_put_fsm_rejected_60s**:
-   **node_put_fsm_rejected_total**:

### Timestamps

The various Erlang applications that Riak is comprised of contribute their own statistics to `riak-admin status`.  The below timestamps record, in Epoch time, the last time statistics for that application were generated.

(2)

-   **riak_kv_stat_ts**: The last time Riak KV stats were generated.
-   **riak_pipe_stat_ts**: The last time Riak Pipe stats were generated.

### Ring

General ring information is reported in `riak-admin status`.

(4)

-   **ring_members**: List of nodes which are members of the ring
-   **ring_num_partitions** The configured number of partitions in the
    ring
-   **ring_ownership**: List of all nodes in the ring and their
    associated partition ownership
-   **ring_creation_size**:

### CPU and Memory

CPU statistics are taken directly from Erlang’s cpu_sup module.
Documentation for which can be found at [ErlDocs:
cpu_sup](http://erldocs.com/R14B04/os_mon/cpu_sup.html).

(4)

-   **cpu_nprocs**: Number of operating system processes
-   **cpu_avg1**: The average number of active processes for the last 1
    minute (equivalent to top(1) command’s load average when divided by
    256()
-   **cpu_avg5**: The average number of active processes for the last 5
    minutes (equivalent to top(1) command’s load average when divided by
    256()
-   **cpu_avg15**: The average number of active processes for the last
    15 minutes (equivalent to top(1) command’s load average when divided
    by 256()

Memory statistics are taken directly from the Erlang virtual machine.
Documentation for which can be found at [ErlDocs:
Memory](http://erldocs.com/R14B04/erts/erlang.html?i=0&search=erlang:memory#memory/0).

(11)

-   **memory_total**: Total allocated memory (sum of processes and
    system)
-   **memory_processes**: Total amount of memory allocated for Erlang
    processes
-   **memory_processes_used**: Total amount of memory used by Erlang
    processes
-   **memory_system**: Total allocated memory that is not directly
    related to an Erlang process
-   **memory_atom**: Total amount of memory currently allocated for
    atom storage
-   **memory_atom_used**: Total amount of memory currently used for
    atom storage
-   **memory_binary**: Total amount of memory used for binaries
-   **memory_code**: Total amount of memory allocated for Erlang code
-   **memory_ets**: Total memory allocated for Erlang Term Storage
-   **mem_total**: Total available system memory
-   **mem_allocated**: Total memory allocated for this node

### Erlang VM

The below statistics describe the Erlang VM.

(14)

-   **nodename**: The name this node uses to identify itself
-   **connected_nodes**: A list of the nodes that this node is aware of
    at this time
-   **sys_driver_version**: String representing the Erlang driver version in use by the runtime system
-   **sys_global_heaps_size**: Current size of the shared global heap
-   **sys_heap_type**: String representing the heap type in use (one
    of private, shared, hybrid)
-   **sys_logical_processors**: Number of logical processors available
    on the system
-   **sys_otp_release**: Erlang OTP release version in use on the node
-   **sys_process_count**:
-   **sys_smp_support**:
-   **sys_system_version**: Detailed Erlang version information
-   **sys_system_architecture**:
-   **sys_threads_enabled**:
-   **sys_thread_pool_size**:
-   **sys_wordsize**:

### Miscellaneous Information

Miscellaneous Information stats are data points that provide details
particular to this node.

List of miscellaneous information statistics:

(3)

-   **leveldb_read_block_error**: The number of LevelDB read block errors.  Will read as undefined if LevelDB is not being used.
-   **disk**: Information about the disk, taken from Erlang's disksup module.  Reported as [{"ID",KBytes_Used,Percent_Util}].
-   **storage_backend**:  The storage backend currently in use.

{{#1.2.0+}}
### Pipeline Metrics

The following metrics from from riak_pipe are generated during MapReduce operations.

(5)

- **pipeline_active**: The number of pipelines active in the last 60 seconds
- **pipeline_create_count**: The total number of pipelines created since the node was started
- **pipeline_create_error_count**: The total number of pipeline creation errors since the node was started
- **pipeline_create_error_one**: The number of pipelines created in the last 60 seconds
- **pipeline_create_one**: The number of pipeline creation errors in the last 60 seconds
{{/1.2.0+}}

### Application and Subsystem Versions

The specific version of each Erlang application and subsystem which
makes up a Riak node is present in `riak-admin status` output.

(30)

-   **erlydtl_version**:
-   **riak_control_version**: Version of Riak Control application in use
-   **cluster_info_version**: Version of Cluster Information application in use
-   **riak_search_version**: Version of Riak Search application in use
-   **merge_index_version**: Version of Merge Index application in use
-   **riak_kv_version**: Version of Riak KV application in use
-   **sidejob_version**:
-   **riak_api_version**:
-   **riak_pipe_version**: Version of Riak Pipe application in use
-   **riak_core_version**: Version of Riak Core application in use
-   **bitcask_version**: Version of Bitcask backend application in use
-   **basho_stats_version**: Version of Basho stats application in use
-   **luke_version**: Version of Luke application in use {{<1.3.0}}
-   **webmachine_version**: Version of Webmachine application in use
-   **mochiweb_version**: Version of MochiWeb application in use
-   **inets_version**: Version of Inets application in use
-   **erlang_js_version**: Version of Erlang JS application in use
-   **runtime_tools_version**: Version of runtime tools application in use
-   **os_mon_version**: Version of Operating System Monitor application in use
-   **riak_sysmon_version**: Version of Riak System Monitor application in use
-   **ssl_version**: Version of secure sockets layer (SSL) application in use
-   **public_key_version**: Version of public key application in use
-   **crypto_version**: Version of Cryptography application in use
-   **sasl_version**: Version of SASL application in use
-   **lager_version**: Version of Lager application in use
-   **goldrush_version**:
-   **compiler_version**:
-   **syntax_tools_version**:
-   **stdlib_version**: Version of Standard Library application in use
-   **kernel_version**: Version of Kernel application in use

{{#1.2.0+}}
### Riak Search Statistics

The following statistics related to Riak Search message queues are available.

- **riak_search_vnodeq_max**: Maximum number of unprocessed messages all
  virtual node (vnode) message queues in the Riak Search subsystem have
  received on this node in the last minute
- **riak_search_vnodeq_mean**: Mean number of unprocessed messages all
  vnode message queues in the Riak Search subsystem have received on this
  node in the last minute
- **riak_search_vnodeq_median**: Median number of unprocessed messages all
  vnode message queues in the Riak Search subsystem have received on this
  node in the last minute
- **riak_search_vnodeq_min**: Minimum number of unprocessed messages all
  vnode message queues in the Riak Search subsystem have received on this
  node in the last minute
- **riak_search_vnodeq_total**: Total number of unprocessed messages all
  vnode message queues in the Riak Search subsystem have received on this
  node since it was started
- **riak_search_vnodes_running**: Total number of vnodes currently running
  in the Riak Search subsystem

Note that under ideal operation and with the exception of
`riak_search_vnodes_running` these statistics should contain low values
(e.g., 0-10). Presence of higher values could be indicative of an issue.
{{/1.2.0+}}

Riaknostic
----------

[Riaknostic](http://riaknostic.basho.com/) is a small suite of
diagnostic checks that can be run against a Riak node to discover common
problems, and recommend how to resolve them. These checks are derived
from the experience of the Basho Client Services Team as well as
numerous public discussions on the mailing list, `#riak` IRC channel,
and other online media.

{{#1.3.0-}}
Riaknostic is an open source project developed by Basho Technologies and
Riak community members. The code is available in the [Riaknostic
Github repository](https://github.com/basho/riaknostic).

Getting started with Riaknostic is easy, and instructions for
installation and use are provided on the Riaknostic website. Once
downloaded and installed, Riaknostic adds a `diag` subcommand to the
`riak-admin` command.

Executing `riak-admin diag` will provide
information on any node problems as detected by Riaknostic, and also
recommendations for resolution of the problems. Riaknostic can be
extremely handy for diagnosing a range of configuration issues and is strongly
recommended as a first step when inspecting a problematic node or cluster
issue.
{{/1.3.0-}}

{{#1.3.0+}}
As of Riak version 1.3, Riaknostic is installed by default.

Riaknostic is included with Riak and exposed through the `riak-admin diag` command. It is an open source project developed by Basho Technologies and
Riak community members. The code is available in the
[Riaknostic Github repository](https://github.com/basho/riaknostic).
{{/1.3.0+}}


Related Resources
-----------------

-   [Configuration and Management: Command Line Tools:
    riak-admin](http://docs.basho.com/riak/1.2.0/references/riak-admin Command Line/)
-   [Riaknostic](http://riaknostic.basho.com/)
-   [[HTTP API status|HTTP Status]]
