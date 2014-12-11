---
title: Common Failure Scenarios
project: riak
version: 2.0.0+
document: cookbook
audience: intermediate
keywords: [operator, failure, troubleshooting]
---

This document walks you through a number of failure scenarios that you
may encounter, pointing to relevant documentation and suggesting ways of
addressing the problem.

If you don't a specific problem addressed in this document, it may also
be worthwhile to check out our [[troubleshooting checklist]]. This guide
provides general information about how to begin to address a wide
variety of issues in Riak.

## High Latency

If your cluster is experiencing latency issues, we recommend consulting
our [[Latency Reduction Checklist]].

## Node Has Crashed Unexpectedly

For reasons that are not immediately clear, a node in your cluster has
crashed, i.e. is not responsive or has gone offline. There are a few
potential causes of this:

* The node is out of memory
* Data corruption
* An expensive and/or long-running query is in progress

## Node Won't Start

If you're attempting to start a node (using the `[[riak start|riak
Command Line#start]]` command) and the operation keeps timing out or
throwing errors, there's a good chance that there's either an error in
your [[configuration files]] or a pre-condition for running Riak in your
environment that is not being met.

* Check for configuration errors by running the `[[riak chkconfig|riak
  Command Line#chkconfig]]` command. If the output is `config is OK`,
  then your configuration files are syntactically correct. Please note,
  though, that there may be other configuration issues involved that are
  not syntactic. More information can be found in [[Checking Your
  Configuration|Configuration Files#Checking-Your-Configuration]].
* Riak tends to require a lot of open files. Some operating systems, in
  particular Mac OS X, have an open files limit that is below what Riak
  requires. If this is a problem in your OS, Riak will provide a warning
  when you attempt to start the node. See our [[Open Files Limit]]
  documentation for more information.

## Node Won't Stop

If you're attempting to stop a node (using the `[[riak stop|riak Command
Line#stop]]` command) and you get `ok` as a response, then the operation
was successful. If not, then the stop operation has probably failed.
This problem usually arises when there is some set of work that the node
is scheduled to perform that has not yet been completed. This might
involve one of the following:

* **Replication queue**
* **Unfinished compactions**

If all else fails and it's imperative that the node be stopped, you can
always resort to the
`[kill](http://www.cyberciti.biz/faq/unix-kill-command-examples/)`
command, though we do not advise this.

## Disk is Full

If a Riak node's disk is full or getting perilously close to it, this
can cause a wide variety of problems and should be avoided at all costs.
See [[Cluster Capacity Planning]] for information on how to avoid this
possibility.

If, however, you need to reclaim disk space immediately, we recommend
beginning in these two places:

* [[Bitcask]] hintfiles --- These files are used by Bitcask to build an
    in-memory representation of the current key space. If you are using
    Bitcask, they can be deleted if necessary; if you are not using
    Bitcask, disregard this suggestion. You can find them in the node's
    `/data` directory, in the `/bitcask` subdirectory. Within the
    `/bitcask` directory you'll find a subdirectory for each
    [[vnode|Riak Glossary#vnodes]] data partition within the node. Each
    partition contains a `*.bitcask.hint` file, where the `*` is an
    assigned number. Those are the files that you must remove. One
    drawback to eliminating Bitcask hintfiles is that Bitcask will
    likely take longer to start up when the node is restarted. If disk
    space has become a serious issue, however, then deleting hintfiles
    is probably worth the risk.
* Active anti-entropy \(AAE) files --- These files are used by
    Riak's [[active anti-entropy]] subsystem. By default, they reside in
    each node's `/data` directory, in the `/anti_entropy` subdirectory

First turn off AAE using `rpc:multicall`, then delete all the
`/anti_entropy` directories

```erlang
k_core_util:rpc_every_member_ann(riak_kv_entropy_manager,disable,[],infinity).

%% To verify that it's off
riak_core_util:rpc_every_member_ann(riak_kv_entropy_manager,enabled,[],infinity).
```

## Transfers Won't Complete

Riak relies heavily on inter-node transfers of data for its normal
functioning. If those transfers won't complete for some reason, we
recommend the following:

* Turn all transfers off and then back on again using the `[[riak-admin
    transfer-limit|riak-admin Command Line]]` interface. You can disable
    transfers by setting the limit to zero:

    ```bash
    riak-admin transfer-limit <nodename> 0
    ```

    You can then re-enable transfers by setting the limit to a non-zero
    integer. The following would set the limit to 2:

    ```bash
    riak-admin transfer-limit <nodename> 2
    ```

    You should repeat this process for all nodes in your cluster that
    are experiencing this issue.
* Check your firewall and/or
    [iptables](http://en.wikipedia.org/wiki/Iptables) to see if there is
    a networking issue external to Riak that is causing transfers to
    hang.
* Check your [[logs|Logging]] to see if there are corrupted or repeated
    handoff attempts. From the logs you should be able to discern which
    partitions are experiencing problems.

## Replication is Slow or Stalled

If you're using [Riak Enterprise](http://basho.com/riak-enterprise/)'s
[[Multi-Datacenter Replication|Multi Data Center Replication v3
Architecture]] capabilities, you might experience cases where
replication between source and sink clusters is laggy or outright
stalled. If this happens to you, we recommend the following:

* Your replication queue may be full. You can check for this using the
    `[[riak-repl status|Multi Data Center Replication v3
    Operations##-code-riak-repl-code-Status-Output]]` command. In the
    [[output|Multi Data Center Replication: Statistics]] of that
    command, there is a `drops` stat that displays the number of objects
    dropped from the realtime queue as a result of the queue being full.
    If this number of greater than zero for several minutes or longer,
    then this is a good indicator that the queue is full.

## Queries are Timing Out

While Riak offers a variety of complex query tools, such as [[secondary
indexes (2i)|Using Secondary Indexes]], [[Riak Search|Using Search]],
and [[MapReduce|Using MapReduce]], Riak always performs best and most
predictably when you restrict your application to key/value operations,
i.e. reads and writes. If, however, your use case does require a complex
querying tool, there are some things to be aware of and some problems
that you may run into.

### Issues with MapReduce and Listing Operations

In general, MapReduce should be used only for batch processing purposes
and _never_ for realtime querying. Overusing MapReduce in production
clusters will invariably lead to performance issues, many of them
difficult to trace and address. In many cases the key to properly
addressing MapReduce-related issues is to curtail its usage and restrict
MapReduce operations to times of relatively light load.

If you suspect that MapReduce operations are producing problems, you can
verify this by checking your error logs for `riak_pipe`-related errors.
If you see such errors popping up frequently, that may be an indicator
that you should re-examine your MapReduce usage.

The same goes for [[list buckets|PBC List Buckets]] and [[list keys|PBC
List Keys]] operations. These operations can be useful for certain
testing purposes but should _never_ be used in production and were never
intended as a normal querying tool.

## Query Results are Inconsistent

MapReduce and secondary index queries are `r=1` queries, which means
that a result will be returned as soon as one node responds. If objects
have different values on different nodes, which is a scenario that Riak
is designed to handle, then queries can return differing results. If
your use case demands completely consistent results across queries over
the same buckets and/or keys, then [[Riak Search|Using Search]] might be
a better fit for your use case.

## Backend Corruption

If data or other auxiliary files have become corrupted in one or more of
your backends, this can cause nodes to fail to start and for other Riak
operations to fail behind the scenes. If you suspect that backend
corruption may be an issue in your cluster, we recommend checking the
`console.log` files location in the `/log` directory of each node. In
particular, be on the lookout for log messages that begin with one of
the following atoms:

* `bad_arg`
* `crc_error`
* `io`
* `db_open`

If you're using [[Bitcask]], one way to address corruption is to delete
Bitcask's hintfiles. Each Bitcask partition directory in which data is
stored will have a `.hint` file that can be deleted.

If you're using [[LevelDB]], you should check the logs for
LevelDB-specific errors, whether stemming from [[active anti-entropy]]
problems or from normal LevelDB backend operations.

## Riak Search Issues

Failures specific to [[Riak Search|Using Search]] include index
corruption. Riak Search stores data associated with each index in your
cluster in `/data/yz/`. Each index will have its own subdirectory.

* `merge_index` can suffer from corruption
* Manual compaction

## Configuration Issues

While configuration problems can arise for a variety of reasons, it's
important to first ensure that your configuration files are

* `riak chkconfig` for syntactic and other issues
* ETOOMANYBACKENDS
* Configs can be changed on the fly using `riak attach`. But make sure
    to also register those changes in the config files so that they are
    activated on node restart. Example:

    ```erlang
    application:set_env(bitcask, merge_window, never).
    ```

## Open Files Limit Issues

Riak tends to use a lot of open files as part of its normal operations.
If your system's open files limits are set too low, this can lead to a
wide variety of problems in your cluster, from undue latency to failed
operations. We recommend consulting our documentation on [[open files
limits|Open Files Limit]] and [[system performance tuning]] _prior_ to
building and starting a new cluster. Those documents should be useful
even if you're working with an already built and running cluster.

At any time, you can check the open files limit under which a Riak
node's Erlang VM is operating by running the `[[riak attach|riak Command
Line#riak-attach]]` and accessing the node's Erlang shell. In the shell,
run this command:

```
> os:cmd('ulimit -n').
```

The output should be something along the lines of `"65536'n"`. Use that
figure when making judgments about what to do next.

## Common Erlang VM Problems

Because Riak runs in an [Erlang
VM](http://www.erlang.org/faq/implementations.html), a variety of
problems can arise from an improperly tuned VM. We recommend reading
through our [[Erlang VM Tuning]] document _prior_ to building and
running a cluster.

Commonly encountered Erlang VM problems include the following:

* **Insufficient ETS tables** --- We recommend setting the
  `erlang.max_ets_tables` setting to at least 8192 (or the `+e` setting
  in `vm.args` if you're using the older configuration system).
* Process counts
* `+zdbbl`/`erlang.distribution_buffer_size`
