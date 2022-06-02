---
title: "Inspecting a Node"
description: ""
project: "riak_kv"
project_version: "2.2.6"
menu:
  riak_kv-2.2.6:
    name: "Inspecting a Node"
    identifier: "cluster_operations_inspecting_node"
    weight: 103
    parent: "managing_cluster_operations"
toc: true
aliases:
  - /riak/2.2.6/ops/running/nodes/inspecting
  - /riak/kv/2.2.6/ops/running/nodes/inspecting
---

When inspection of a Riak node to gather metrics on performance or
potential issues is desired, a number of tools are available to help,
and are either included with Riak itself or made available through the
Riak community.

This guide provides starting points and details on some of the available
tools for inspecting a Riak node.

## riak-admin status

`riak-admin status` is a subcommand of the `riak-admin` command that is
included with every installation of Riak. The `status` subcommand
provides data related to the current operating status for a node. The
output of `riak-admin status` is categorized and detailed below.

Please note, for some counters, such as `node_get_fsm_objsize`, a
minimum of 5 transactions is required for statistics to be generated.

#### Performance

We recommended checking stats every 90-120 seconds for best performance.

Repeated runs of the `riak-admin status` command should not have a
negative performance impact as the statistics are cached internally in
Riak.

### Active Stats

Active Stats represent current activity on the node.

Stat                    | Description
------------------------|---------------------------------------------------
`pbc_active`            | Number of active Protocol Buffers connections
`node_get_fsm_active`   | Number of active GET FSMs
`node_put_fsm_active`   | Number of active PUT FSMs
`index_fsm_active`      | Number of active Secondary Index FSMs
`list_fsm_active`       | Number of active Keylisting FSMs
`node_get_fsm_rejected` | Number of GET FSMs actively being rejected by Sidejob's overload protection
`node_put_fsm_rejected` | Number of PUT FSMs actively being rejected by Sidejob's overload protection

### Average Stats

Average Stats represent an average calculated as (total occurrences /
number of samples) since this node was started.  In the below stats the
sample time is 1s, giving us a per-second average.  Currently, the only
Average Stats are reported by Sidejob - an Erlang library that
implements a parallel, capacity-limited request pool.

Stat                    | Description
------------------------|---------------------------------------------------
`node_get_fsm_in_rate`  | Average number of GET FSMs enqueued by Sidejob
`node_get_fsm_out_rate` | Average number of GET FSMs dequeued by Sidejob
`node_put_fsm_in_rate`  | Average number of PUT FSMs enqueued by Sidejob
`node_put_fsm_out_rate` | Average number of PUT FSMs dequeued by Sidejob

### One-Minute Stats

One-Minute Stats represent the number of times a particular activity has
occurred within the last minute on this node.

#### General One-Minute Stats

Stat                                  | Description
--------------------------------------|---------------------------------------------------
`node_gets`                           | Number of GETs coordinated by this node, including GETs to non-local vnodes in the last minute
`node_puts`                           | Number of PUTs coordinated by this node, where a PUT is sent to a local vnode in the last minute
`vnode_gets`                          | Number of GET operations coordinated by local vnodes on this node in the last minute
`vnode_puts`                          | Number of PUT operations coordinated by local vnodes on this node in the last minute
`vnode_index_refreshes`               | Number of secondary indexes refreshed on this node during secondary index anti-entropy in the last minute
`vnode_index_reads`                   | Number of local replicas participating in secondary index reads in the last minute
`vnode_index_writes`                  | Number of local replicas participating in secondary index writes in the last minute
`vnode_index_writes_postings`         | Number of individual secondary index values written in the last minute
`vnode_index_deletes`                 | Number of local replicas participating in secondary index deletes in the last minute
`vnode_index_deletes_postings`        | Number of individual secondary index values deleted in the last minute
`pbc_connects`                        | Number of Protocol Buffers connections made in the last minute
`node_get_fsm_active_60s`             | Number of GET FSMs active in the last minute
`node_put_fsm_active_60s`             | Number of PUT FSMs active in the last minute
`node_get_fsm_rejected_60s`           | Number of GET FSMs rejected by Sidejob's overload protection in the last minute
`node_put_fsm_rejected_60s`           | Number of PUT FSMs rejected by Sidejob's overload protection in the last minute
`index_fsm_create`                    | Number of Secondary Index query FSMs created in the last minute
`index_fsm_create_error`              | Number of Secondary Index query FSM creation errors in the last minute
`list_fsm_create`                     | Number of Keylisting FSMs created in the last minute
`list_fsm_create_error`               | Number of Keylisting FSM creation errors in the last minute
`read_repairs`                        | Number of read repair operations this node has coordinated in the last minute
`read_repairs_primary_outofdate_one`  | Number of read repair operations performed on primary vnodes in the last minute due to stale replicas
`read_repairs_primary_notfound_one`   | Number of read repair operations performed on primary vnodes in the last minute due to missing replicas
`read_repairs_fallback_outofdate_one` | Number of read repair operations performed on fallback vnodes in the last minute due to stale replicas
`read_repairs_fallback_notfound_one`  | Number of read repair operations performed on fallback vnodes in the last minute due to missing replicas

#### FSM Time

FSM Time Stats represent the amount of time in microseconds required to
traverse the GET or PUT Finite State Machine code, offering a picture of
general node health. From your application's perspective, FSM Time
effectively represents experienced latency. Mean, Median, and 95th-,
99th-, and 100th-percentile (Max) counters are displayed. These are
one-minute stats.

Stat                       | Description
---------------------------|---------------------------------------------------
`node_get_fsm_time_mean`   | Mean time between reception of client GET request and subsequent response to client
`node_get_fsm_time_median` | Median time between reception of client GET request and subsequent response to client
`node_get_fsm_time_95`     | 95th percentile time between reception of client GET request and subsequent response to client
`node_get_fsm_time_99`     | 99th percentile time between reception of client GET request and subsequent response to client
`node_get_fsm_time_100`    | 100th percentile time between reception of client GET request and subsequent response to client
`node_put_fsm_time_mean`   | Mean time between reception of client PUT request and subsequent response to client
`node_put_fsm_time_median` | Median time between reception of client PUT request and subsequent response to client
`node_put_fsm_time_95`     | 95th percentile time between reception of client PUT request and subsequent response to client
`node_put_fsm_time_99`     | 99th percentile time between reception of client PUT request and subsequent response to client
`node_put_fsm_time_100`    | 100th percentile time between reception of client PUT request and subsequent response to client

#### GET FSM Siblings

GET FSM Sibling Stats offer a count of the number of siblings
encountered by this node on the occasion of a GET request. These are
one-minute stats.

Stat                           | Description
-------------------------------|---------------------------------------------------
`node_get_fsm_siblings_mean`   | Mean number of siblings encountered during all GET operations by this node within the last minute
`node_get_fsm_siblings_median` | Median number of siblings encountered during all GET operations by this node within the last minute
`node_get_fsm_siblings_95`     | 95th percentile of siblings encountered during all GET operations by this node within the last minute
`node_get_fsm_siblings_99`     | 99th percentile of siblings encountered during all GET operations by this node within the last minute
`node_get_fsm_siblings_100`    | 100th percentile of siblings encountered during all GET operations by this node within the last minute

#### GET FSM Objsize

GET FSM Objsize Stats represent a view of the sizes of objects flowing
through this node's GET FSMs. The size of an object is obtained by
summing the length of the bucket name, key, serialized vector clock,
value, and serialized metadata of each sibling. GET FSM Objsize and GET
FSM Siblings are inextricably linked. These are one-minute stats.

Stat                          | Description
------------------------------|---------------------------------------------------
`node_get_fsm_objsize_mean`   | Mean object size (bytes) encountered by this node within the last minute
`node_get_fsm_objsize_median` | Median object size (bytes) encountered by this node within the last minute
`node_get_fsm_objsize_95`     | 95th percentile object size (bytes) encountered by this node within the last minute
`node_get_fsm_objsize_99`     | 99th percentile object size (bytes) encountered by this node within the last minute
`node_get_fsm_objsize_100`    | 100th percentile object size (bytes) encountered by this node within the last minute

### Total Stats

Total Stats represent the total number of times a particular activity
has occurred since this node was started.

Stat                                   | Description
---------------------------------------|---------------------------------------------------
`node_gets_total`                      | Total number of GETs coordinated by this node, including GETs to non-local vnodes
`node_puts_total`                      | Total number of PUTs coordinated by this node, including PUTs to non-local vnodes
`vnode_gets_total`                     | Total number of GETs coordinated by local vnodes
`vnode_puts_total`                     | Total number of PUTS coordinated by local vnodes
`read_repairs_total`                   | Total number of Read Repairs this node has coordinated
`coord_redirs_total`                   | Total number of requests this node has redirected to other nodes for coordination
`vnode_index_refreshes_total`          | Total number of indexes refreshed during secondary index anti-entropy
`vnode_index_reads_total`              | Total number of local replicas participating in secondary index reads
`vnode_index_writes_total`             | Total number of local replicas participating in secondary index writes
`vnode_index_writes_postings_total`    | Total number of individual secondary index values written
`vnode_index_deletes_total`            | Total number of local replicas participating in secondary index deletes
`vnode_index_deletes_postings_total`   | Total number of individual secondary index values deleted
`pbc_connects_total`                   | Total number of Protocol Buffers connections made
`precommit_fail`                       | Total number of pre-commit hook failures
`postcommit_fail`                      | Total number of post-commit hook failures
`node_get_fsm_rejected_total`          | Total number of GET FSMs rejected by Sidejob's overload protection
`node_put_fsm_rejected_total`          | Total number of PUT FSMs rejected by Sidejob's overload protection
`read_repairs_primary_outofdate_count` | Total number of read repair operations performed on primary vnodes due to stale replicas
`read_repairs_primary_notfound_count`  | Total number of read repair operations performed on primary vnodes due to missing replicas
`read_repairs_fallback_outofdate_count`| Total number of read repair operations performed on fallback vnodes due to stale replicas
`read_repairs_fallback_notfound_count` | Total number of read repair operations performed on fallback vnodes due to missing replicas

### Timestamps

Some of the Erlang applications that Riak is comprised of contribute
statistics to `riak-admin status`.  The below timestamps record, in
Epoch time, the last time statistics for that application were
generated.

Stat                | Description
--------------------|---------------------------------------------------
`riak_kv_stat_ts`   | The last time Riak KV stats were generated.
`riak_pipe_stat_ts` | The last time Riak Pipe stats were generated.

### Ring

General ring information is reported in `riak-admin status`.

Stat                 | Description
---------------------|---------------------------------------------------
`ring_members`       | List of nodes that are members of the ring
`ring_num_partitions`| The number of partitions in the ring
`ring_ownership`     | List of all nodes in the ring and their associated partition ownership
`ring_creation_size` | Ring size this cluster was created with

### CPU and Memory

CPU statistics are taken directly from Erlang’s cpu_sup module.
Documentation for which can be found at [ErlDocs:
cpu_sup](http://erlang.org/doc/man/cpu_sup.html).

Stat         | Description
-------------|---------------------------------------------------
`cpu_nprocs` | Number of operating system processes
`cpu_avg1`   | The average number of active processes for the last 1 minute (equivalent to top(1) command’s load average when divided by 256())
`cpu_avg5`   | The average number of active processes for the last 5 minutes (equivalent to top(1) command’s load average when divided by 256())
`cpu_avg15`  | The average number of active processes for the last 15 minutes (equivalent to top(1) command’s load average when divided by 256())

Memory statistics are taken directly from the Erlang virtual machine.
Documentation for which can be found at [ErlDocs:
Memory](http://erlang.org/doc/man/erlang.html#memory-0#memory/0).

Stat                    | Description
------------------------|---------------------------------------------------
`memory_total`          | Total allocated memory (sum of processes and system)
`memory_processes`      | Total amount of memory allocated for Erlang processes
`memory_processes_used` | Total amount of memory used by Erlang processes
`memory_system`         | Total allocated memory that is not directly related to an Erlang process
`memory_atom`           | Total amount of memory currently allocated for atom storage
`memory_atom_used`      | Total amount of memory currently used for atom storage
`memory_binary`         | Total amount of memory used for binaries
`memory_code`           | Total amount of memory allocated for Erlang code
`memory_ets`            | Total memory allocated for Erlang Term Storage
`mem_total`             | Total available system memory
`mem_allocated`         | Total memory allocated for this node

### Erlang VM

The below statistics describe properties of the Erlang VM.

Stat                      | Description
--------------------------|---------------------------------------------------
`nodename`                | The name this node uses to identify itself
`connected_nodes`         | A list of the nodes that this node is aware of at this time
`sys_driver_version`      | String representing the Erlang driver version in use by the runtime system
`sys_global_heaps_size`   | Current size of the shared global heap
`sys_heap_type`           | String representing the heap type in use (one of private, shared, hybrid)
`sys_logical_processors`  | Number of logical processors available on the system
`sys_otp_release`         | Erlang OTP release version in use on the node
`sys_process_count`       | Number of processes currently running in the Erlang VM
`sys_smp_support`         | Boolean value representing whether symmetric multi-processing (SMP) is available
`sys_system_version`      | Detailed Erlang version information
`sys_system_architecture` | The node operating system and hardware architecture
`sys_threads_enabled`     | Boolean value representing whether threads are enabled
`sys_thread_pool_size`    | Number of threads in the asynchronous thread pool
`sys_wordsize`            | Size of Erlang term words in bytes as an integer, for examples, on 32-bit architectures 4 is returned and on 64-bit architectures 8 is returned

### Miscellaneous Information

Miscellaneous Information provide additional details particular to this
node.

Stat                       | Description
---------------------------|---------------------------------------------------
`leveldb_read_block_error` | The number of LevelDB read block errors.  Will read as undefined if LevelDB is not being used.
`disk`                     | Information about the disk, taken from Erlang's disksup module.  Reported as [{"ID",KBytes_Used,Percent_Util}].
`storage_backend`          | The storage backend currently in use.

### Pipeline Metrics

The following metrics from from riak_pipe are generated during MapReduce
operations.

Stat                            | Description
--------------------------------|---------------------------------------------------
`pipeline_active`               | The number of pipelines active in the last 60 seconds
`pipeline_create_count`         | The total number of pipelines created since the node was started
`pipeline_create_error_count`   | The total number of pipeline creation errors since the node was started
`pipeline_create_error_one`     | The number of pipeline creation errors in the last 60 seconds
`pipeline_create_one`           | The number of pipelines created in the last 60 seconds

### Application and Subsystem Versions

The specific version of each Erlang application and subsystem which
makes up a Riak node is present in the `riak-admin status` output.  Each
application is linked below next to it's version identifier.

Stat                    | Description
------------------------|---------------------------------------------------
`erlydtl_version`       | [ErlyDTL](http://github.com/erlydtl/erlydtl)
`riak_control_version`  | [Riak Control](http://github.com/basho/riak_control)
`cluster_info_version`  | [Cluster Information](http://github.com/basho/cluster_info)
`riak_search_version`   | [Riak Search](http://github.com/basho/riak_search)
`merge_index_version`   | [Merge Index](http://github.com/basho/merge_index)
`riak_kv_version`       | [Riak KV](http://github.com/basho/riak_kv)
`sidejob_version`       | [Sidejob](http://github.com/basho/sidejob)
`riak_api_version`      | [Riak API](http://github.com/basho/riak_api)
`riak_pipe_version`     | [Riak Pipe](http://github.com/basho/riak_pipe)
`riak_core_version`     | [Riak Core](http://github.com/basho/riak_core)
`bitcask_version`       | [Bitcask](http://github.com/basho/bitcask)
`basho_stats_version`   | [Basho Stats](http://github.com/basho/basho_stats)
 `webmachine_version`   | [Webmachine](http://github.com/basho/webmachine)
`mochiweb_version`      | [MochiWeb](http://github.com/basho/mochiweb)
`inets_version`         | [inets](http://erlang.org/doc/apps/inets/)
`erlang_js_version`     | [Erlang JS](http://github.com/basho/erlang_js)
`runtime_tools_version` | [Erlang Runtime Tools](http://erlang.org/doc/apps/runtime_tools/)
`os_mon_version`        | [Erlang Operating System Monitor](http://erlang.org/doc/apps/os_mon/)
`riak_sysmon_version`   | [Riak System Monitor](http://github.com/basho/riak_sysmon)
`ssl_version`           | [Erlang Secure Sockets Layer (SSL)](http://erlang.org/doc/apps/ssl/)
`public_key_version`    | [Erlang Public Key](http://erlang.org/doc/apps/public_key/)
`crypto_version`        | [Erlang crypto](http://erlang.org/doc/apps/crypto/)
`sasl_version`          | [SASL](http://erlang.org/doc/apps/sasl/)
`lager_version`         | [Lager](http://github.com/DeadZen/lager)
`goldrush_version`      | [Goldrush](http://github.com/DeadZen/goldrush)
`compiler_version`      | [Erlang Compiler](http://erlang.org/doc/apps/compiler/)
`syntax_tools_version`  | [Erlang Syntax Tools](http://www.erlang.org/doc/apps/syntax_tools/)
`stdlib_version`        | [Standard Library](http://erlang.org/doc/apps/stdlib/)
`kernel_version`        | [Kernel](http://erlang.org/doc/apps/kernel/)

### Riak Search Statistics

The following statistics related to Riak Search message queues are
available.

Stat                         | Description
-----------------------------|---------------------------------------------------
`riak_search_vnodeq_max`     | Maximum number of unprocessed messages all virtual node (vnode) message queues in the Riak Search subsystem have received on this node in the last minute
`riak_search_vnodeq_mean`    | Mean number of unprocessed messages all vnode message queues in the Riak Search subsystem have received on this node in the last minute
`riak_search_vnodeq_median`  | Median number of unprocessed messages all vnode message queues in the Riak Search subsystem have received on this node in the last minute
`riak_search_vnodeq_min`     | Minimum number of unprocessed messages all vnode message queues in the Riak Search subsystem have received on this node in the last minute
`riak_search_vnodeq_total`   | Total number of unprocessed messages all vnode message queues in the Riak Search subsystem have received on this node since it was started
`riak_search_vnodes_running` | Total number of vnodes currently running in the Riak Search subsystem

Note that under ideal operation and with the exception of
`riak_search_vnodes_running` these statistics should contain low values
(e.g., 0-10). Presence of higher values could be indicative of an issue.

## `riak-debug`

The `riak-debug` command is used to identify and diagnose common problems with your Riak KV nodes.

`riak-debug` also runs `riak-admin diag`, which runs a small suite of diagnostic checks against a Riak KV node to discover common problems. It often offers recommendations about how to resolve those problems as well. 

{{% note title="Warning about `riak-debug` and `riak-admin diag` usage" %}}
The `riak-debug` and `riak-admin diag` commands should only be used after a new installation or configuration change. It should not be used as part of regular monitoring. Overuse of `riak-debug` or `riak-admin diag` can eventually cause the node to crash from atom table exhaustion.
{{% /note %}}

## Strong Consistency Stats

Riak tabulates a variety of stats related to Riak's optional [strong consistency](../../reference/strong-consistency) feature. The table below lists those stats.

### GET-related stats

Stat | Description
:----|:-----------
`consistent_gets` | Number of strongly consistent GETs coordinated by this node in the last minute
`consistent_gets_total` | Total number of strongly consistent GETs coordinated by this node
`consistent_get_objsize_mean` | Mean object size (bytes) for strongly consistent GETs on this node in the last minute
`consistent_get_objsize_median` | Median object size (bytes) for strongly consistent GETs on this node in the last minute
`consistent_get_objsize_95` | 95th-percentile object size (bytes) for strongly consistent GETs on this node in the last minute
`consistent_get_objsize_99` | 99th-percentile object size (bytes) for strongly consistent GETs on this node in the last minute
`consistent_get_objsize_100` | 100th-percentile object size (bytes) for strongly consistent GETs on this node in the last minute
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
`consistent_put_objsize_mean` | Mean object size (bytes) for strongly consistent PUTs on this node in the last minute
`consistent_put_objsize_median` | Median object size (bytes) for strongly consistent PUTs on this node in the last minute
`consistent_put_objsize_95` | 95th-percentile object size (bytes) for strongly consistent PUTs on this node in the last minute
`consistent_put_objsize_99` | 99th-percentile object size (bytes) for strongly consistent PUTs on this node in the last minute
`consistent_put_objsize_100` | 100th-percentile object size (bytes) for strongly consistent PUTs on this node in the last minute
`consistent_put_time_mean` | Mean time between reception of client PUTs to strongly consistent keys and subsequent response
`consistent_put_time_median` | Median time between reception of client PUTs to strongly consistent keys and subsequent response
`consistent_put_time_95` | 95th-percentile time between reception of client PUTs to strongly consistent keys and subsequent response
`consistent_put_time_99` | 99th-percentile time between reception of client PUTs to strongly consistent keys and subsequent response
`consistent_put_time_100` | 100th-percentile time between reception of client PUTs to strongly consistent keys and subsequent response
## riak-admin diag

Running `riak-admin diag` by itself will perform a check of all of the
data partitions in your cluster. It will return a listing of partitions
that have been checked, each of which looks something like this:

```
{1392993748081016843912887106182707253109560705024, % the partition checked
 'dev-rel@127.0.0.1'},                              % that partition's nodename
```

At the end of that (potentially very long) listing of checked
partitions, it will print notices, warnings, and other pieces of
information about issues that it has found, including date/time, message
type, and a detailed description. Here's an example:

```
15:34:52.736 [warning] Riak crashed at Wed, 07 Dec 2011 21:47:50 GMT, leaving crash dump in /srv/riak/log/erl_crash.dump. Please inspect or remove the file.
15:34:52.736 [notice] Data directory /srv/riak/data/bitcask is not mounted with 'noatime'. Please remount its disk with the 'noatime' flag to improve performance.
```

Messages bear the following types (derived from
[syslog](http://en.wikipedia.org/wiki/Syslog) security levels):

* `debug`
* `info`
* `notice`
* `warning`
* `error`
* `critical`
* `alert`
* `emergency`

#### Command flags

Attaching the `--help` flag will return a list of flags and commands
that can be used with Riaknostic:

```
Usage: riak-admin diag [-d <level>] [-l] [-h] [--export] [check_name ...]

-h, --help            Display help/usage dialogue
-d, --level           Minimum message severity level (default: notice)
-l, --list            Describe available diagnostic tasks
--export              Package system info in '/export.zip'
check_name            A specific check to run
```

Running `riak-admin diag`  with the `--list` flag will return a list of
available diagnostic checks. The following checks are available:

Check | Description
:-----|:-----------
`disk` | Data directory permissions and atime
`dumps` | Find crash dumps
`memory_use` | Measure memory usage
`nodes_connected` | Cluster node liveness
`ring_membership` | Cluster membership validity
`ring_preflists` | Check if the ring satisfies `n_val`
`ring_size` | Check if the ring size valid
`search` | Check whether Riak Search is enabled on all nodes

The `--level` flag enables you to specify the log level and thus to
filter messages based on type. You can pass in any of the message types
listed above (`debug`, `info`, etc.).

The `--level` flag can be used when running `riak-admin diag` with or
without specifying a diagnostic check.

#### Contributing

Do you have an idea that would help us improve Riaknostic? If so, fork
the [GitHub repository](https://github.com/basho/riaknostic) and send us
a pull request with your changes. The code is documented with
[edoc](http://riaknostic.basho.com/edoc/index.html), so give the API
Docs a read before you contribute.

If you want to run the Riaknostic script while developing and you don't
have it hooked up to your local Riak installation, you can invoke it
directly like so:

```bash
./riaknostic --etc ~/code/riak/rel/riak/etc --base ~/code/riak/rel/riak --user `whoami` [other options]
```

Those extra options are usually assigned by the `riak-admin` script for
you, but here's how to set them:

* `--etc` --- The location of your Riak configuration directory (usually
    `/etc`). In the example above, configuration is in the generated
    directory of a source checkout of Riak.
* `--base` --- The "base" directory of Riak, usually the root of the
    generated directory or `/usr/lib/riak` on Linux. Scan the
    `riak-admin` script for how the `RUNNER_BASE_DIR` variable is
    assigned on your platform.
* `--user` --- The user/UID as which the Riak node runs. In a source
    checkout, it's the current user; on most systems, it's `riak`.

## Related Resources

* [The riak-admin configuration management tool](../../admin/riak-admin/)
* [Riaknostic](http://riaknostic.basho.com/)
* [HTTP API Status](../../../developing/api/http/status/)
