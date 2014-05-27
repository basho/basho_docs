---
title: Ring Resizing
project: riak
version: 2.0.0+
document: cookbook
toc: true
audience: advanced
keywords: [ops, ring, ring-resizing]
---

The ring resizing feature in Riak 2.0 and greater enables Riak operators to change the number of partitions in a Riak cluster's ring during normal operations, under load.

Previously, a cluster was limited to having `ring_creation_size` partitions throughout its entire lifespan. In order to change the number of partitions, a separate cluster would need to be spun up alongside the original and the data migrated between the two by external means.

The intended purpose of the ring resizing feature is to support users who create a cluster with either too few or two many partitions and need to change this without disrupting operations more than necessary.

<div class="note">
<div class="title">Note on ring resizing and scalability</div>
Ring resizing is <em>not</em> intended as a scaling feature for clusters to add or remove concurrent processing ability. Since the number of partitions can limit the number of nodes in the cluster, ring resizing can be used to increase capacity in that regard. In short, the feature is intended for infrequent use in highly specific scenarios.
</div>

There are a number of important considerations to bear in mind while running a ring resizing process:

* For a resize to succeed, all nodes should be up. The only cluster operation permitted during a ring resize is `force-remove`. Other operations will be delayed while the resize completes.
* If you perform a [[listkeys|HTTP List Keys]] or [[secondary index|Using Secondary Indexes]] query during a ring resize, you may get duplicates or miscounts in coverage queries. In an upcoming release of Riak, this will be self-healing (see [this pull request](https://github.com/basho/riak_kv/pull/685) for more information).
* Resizing partitions can take up a lot of disk space. Make sure that you have sufficient storage to complete the resize operation.
* Basho strongly recommends that you do _not_ use the `force-replace` command that is part of the `[[riak-admin|riak-admin Command Line#cluster-force-replace]]` interface during ring resizing.

## Starting the Resize

To resize your Riak cluster, use the `riak-admin cluster` command interface to `submit`, `plan`, and `commit` the change to your cluster. The command to submit a resize request has the following form:

```bash
riak-admin cluster resize-ring <new_size>
```

The following command would schedule changing the size of the ring to 64:

```bash
riak-admin cluster resize-ring 64
```

If successful, the following would appear in the console:

```
Success: staged resize ring request with new size: 64
```

**Note**: The size of a Riak ring should _always_ be a 2<sup>n</sup> integer, e.g. 16, 32, 64, etc.

Prior to committing any ring size-related changes, you will need to view the planned changes using the `plan` command (as you would for any cluster-wide changes):

```bash
riak-admin cluster plan
```

This will result in output along the following lines:

```
=============================== Staged Changes ================================
Action         Details(s)
-------------------------------------------------------------------------------
resize-ring    32 to 64 partitions
-------------------------------------------------------------------------------


NOTE: Applying these changes will result in 1 cluster transition

###############################################################################
                         After cluster transition 1/1
###############################################################################

================================= Membership ==================================
Status     Ring    Pending    Node
-------------------------------------------------------------------------------
valid      21.9%     20.3%    'dev1@127.0.0.1'
valid      21.9%     20.3%    'dev2@127.0.0.1'
valid      18.8%     20.3%    'dev3@127.0.0.1'
valid      18.8%     20.3%    'dev4@127.0.0.1'
valid      18.8%     18.8%    'dev5@127.0.0.1'
-------------------------------------------------------------------------------
Valid:5 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

Ring is resizing. see riak-admin ring-status for transfer details.
```

If you are satisfied with the changes, begin the resize operation by committing the changes using the `commit` command:

```bash
riak-admin cluster commit
```

If successful, you should see the following in the console:

```
Cluster changes committed
```

If you change your mind prior to committing, you can abort the pending changes using the `clear` command:

```bash
riak-admin cluster clear
```

## Monitoring Resize Progress

With the new plan committed, the progress of the resizing operation can be monitored using the same means used to monitor other handoff operations. You can use the `ring-status` command to view changes to the cluster that are either in progress or queued:

```bash
riak-admin ring-status
```

Response:

```
================================== Claimant ===================================
Claimant:  'dev1@127.0.0.1'
Status:     up
Ring Ready: true

============================== Ownership Handoff ==============================
Owner:      dev1@127.0.0.1
Next Owner: $resize

Index: 0
  Waiting on: [riak_kv_vnode]
  Complete:   [riak_pipe_vnode]

Index: 228359630832953580969325755111919221821239459840
  Waiting on: [riak_kv_vnode]
  Complete:   [riak_pipe_vnode]

Index: 456719261665907161938651510223838443642478919680
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 685078892498860742907977265335757665463718379520
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 913438523331814323877303020447676887284957839360
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 1141798154164767904846628775559596109106197299200
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 1370157784997721485815954530671515330927436759040
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

# The output directly above will be repeated for all nodes in the cluster, e.g. dev2, dev3, etc.

-------------------------------------------------------------------------------

============================== Unreachable Nodes ==============================
All nodes are up and reachable
```

You can also throttle the ring resize activity using `riak-admin transfer-limit`, which will change the `handoff_concurrency` limit.

For the whole cluster:

```bash
riak-admin transfer-limit <limit>
```

For a specific node:

```bash
riak-admin transfer-limit <node> <limit>
```

Using `riak-admin transfers` will provide you more information about the partitions that are currently in progress. 

```bash
riak-admin transfers
```

Response:

```
'dev5@127.0.0.1' waiting to handoff 3 partitions
'dev4@127.0.0.1' waiting to handoff 1 partitions
'dev3@127.0.0.1' waiting to handoff 1 partitions
'dev2@127.0.0.1' waiting to handoff 2 partitions

Active Transfers:

transfer type: resize_transfer
vnode type: riak_kv_vnode
partition: 1438665674247607560106752257205091097473808596992
started: 2014-01-20 21:03:53 [1.14 min ago]
last update: 2014-01-20 21:05:01 [1.21 s ago]
total size: 111676327 bytes
objects transferred: 122598

                         1818 Objs/s                          
     dev1@127.0.0.1        =======>       dev4@127.0.0.1      
        |=========================                  |  58%    
                         950.38 KB/s                          

transfer type: resize_transfer
vnode type: riak_kv_vnode
partition: 205523667749658222872393179600727299639115513856
started: 2014-01-20 21:03:53 [1.14 min ago]
last update: 2014-01-20 21:05:01 [1.29 s ago]
total size: 100143148 bytes
objects transferred: 130510

                         1939 Objs/s                          
     dev1@127.0.0.1        =======>       dev5@127.0.0.1      
        |==============================             |  69%    
                         1013.71 KB/s                         

transfer type: resize_transfer
vnode type: riak_kv_vnode
partition: 1233142006497949337234359077604363797834693083136
started: 2014-01-20 21:04:44 [17.81 s ago]
last update: 2014-01-20 21:05:01 [1.19 s ago]
total size: 82010614 bytes
objects transferred: 37571

                         2259 Objs/s                          
     dev3@127.0.0.1        =======>       dev5@127.0.0.1      
        |==========                                 |  24%    
                          1.15 MB/s                           

transfer type: resize_transfer
vnode type: riak_kv_vnode
partition: 251195593916248939066258330623111144003363405824
started: 2014-01-20 21:04:55 [7.24 s ago]
last update: 2014-01-20 21:05:01 [898.81 ms ago]
total size: 82012730 bytes
objects transferred: 11864

                         1870 Objs/s                          
     dev3@127.0.0.1        =======>       dev2@127.0.0.1      
        |===                                        |   7%    
                         977.72 KB/s                          
```

You can confirm that the resize operation is no longer running using the `transfers` command and verifying that there are no active or pending transfers:

```
riak-admin transfers
```

Response:

```
No transfers active
```

You can also verify that there are the expected number of partitions in the ring by opening the Erlang shell via the [[`riak attach`|riak Command Line#attach]] command and running this snippet:

```erlang
length(riak_core_ring:all_owners(2, riak_core_ring_manager:get_my_ring())).
```

This command will return the number of partitions as an integer.

## Aborting a Ring Resize Already in Progress

The process to abort a currently running resize is very similar to the process used to set one up. You must submit an `abort` request, plan it, and commit it, all using the `riak-admin cluster` interface.

Submit an `abort` request:

```bash
riak-admin cluster resize-ring abort
```

One of the following messages will appear, depending on the outcome of the `abort` request. If successful:

```
Success: staged abort resize ring request
```

If unsuccessful:

```
Failure: ring is not resizing or resize has completed
```

View planned changes:

```bash
riak-admin cluster resize-ring plan
```

In the output, you should find something like the following:

```
Action         Details(s)
--------------------------------------
resize-ring    abort. current size: 128
```

Commit planned changes:

```bash
riak-admin cluster resize-ring commit
```

If successful, you should see the following:

```bash
Cluster changes committed
```

If console output confirms that the changes have been committed, then your resize operation has been successfully aborted.
