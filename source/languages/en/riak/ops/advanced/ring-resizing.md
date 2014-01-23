---
title: Ring Resizing
project: riak
version: 2.0.0+
document: cookbook
toc: true
audience: advanced
keywords: [ops, ring, ring-resizing]
---

Ring resizing enables Riak operators to change the number of partitions during normal operations, under load.

Previously, a cluster was limited to always having `ring_creation_size` partitions. In order to change the number of partitions, a separate cluster would need to be stood up along side the original and the data would be migrated between the two by external means.

The intended purpose of the ring resizing feature is to support users who create a cluster with either too few or two many partitions and need to change this without the hassle mentioned above.

<div class="note">
<div class="title">Note</div>
Ring resizing is *not* intended as a scaling feature for clusters to add or remove concurrent processing ability. Since the number of partitions ultimately limits the number of nodes in the cluster, ring resizing can be used to increase capacity in that regard. In short, the feature is intended for infrequent use in specific scenarios.
</div>

There are a number of important considerations to bear in mind while running a ring resizing process:

* All nodes must eventually be up for a resize to succeed. The only cluster operation that will be permitted during ring resize is a `force-remove`. Any other operations will be delayed while the resize completes.
* If you perform a [[listkeys|HTTP List Keys]] or [[secondary index|Using Secondary Indexes]] query, you can get duplicates or miscounts in coverage queries. In an upcoming release of Riak, this will be self-healing (see [this pull request](https://github.com/basho/riak_kv/pull/685) for more information).
* Resizing the partitions can take up a lot of disk space. Make sure that you have sufficient storage to complete the resize operation.

## Starting the Resize

To resize your Riak cluster, use the `riak-admin cluster` command interface to submit, plan, and commit the change to your cluster. The command to submit a resize request has the following form:

```bash
riak-admin cluster resize-ring <new_size>
```

The following command would schedule changing the size of the ring to 64:

```bash
riak-admin cluster resize-ring 64

# Response
Success: staged resize ring request with new size: 64
```

Prior to committing any ring size-related changes, you will need to view the planned changes using the `plan` (as you would for any cluster-wide changes):

```bash
riak-admin cluster plan
```

This will result in output along the following lines:

```bash
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

# Response
Cluster changes committed
```

If you change your mind prior to committing, you can abort the pending changes using the `clear` command:

```bash
riak-admin cluster clear
```

## Monitoring Resize Progress

With the new plan committed, the resizing process can be monitored using the same means that one would use to monitor other handoff operations. You can use the `ring-status` command to view changes to the cluster that are either in progress or queued. You can also throttle the ring resize activity using `riak-admin transfer-limit` should you desire.

```bash
riak-admin ring-status

# Response:

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

-------------------------------------------------------------------------------
Owner:      dev2@127.0.0.1
Next Owner: $resize

Index: 45671926166590716193865151022383844364247891968
  Waiting on: [riak_kv_vnode]
  Complete:   [riak_pipe_vnode]

Index: 274031556999544297163190906134303066185487351808
  Waiting on: [riak_kv_vnode]
  Complete:   [riak_pipe_vnode]

Index: 502391187832497878132516661246222288006726811648
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 730750818665451459101842416358141509827966271488
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 959110449498405040071168171470060731649205731328
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 1187470080331358621040493926581979953470445191168
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 1415829711164312202009819681693899175291684651008
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

-------------------------------------------------------------------------------
Owner:      dev3@127.0.0.1
Next Owner: $resize

Index: 91343852333181432387730302044767688728495783936
  Waiting on: [riak_kv_vnode]
  Complete:   [riak_pipe_vnode]

Index: 319703483166135013357056057156686910549735243776
  Waiting on: [riak_kv_vnode]
  Complete:   [riak_pipe_vnode]

Index: 548063113999088594326381812268606132370974703616
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 776422744832042175295707567380525354192214163456
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 1004782375664995756265033322492444576013453623296
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 1233142006497949337234359077604363797834693083136
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

-------------------------------------------------------------------------------
Owner:      dev4@127.0.0.1
Next Owner: $resize

Index: 137015778499772148581595453067151533092743675904
  Waiting on: [riak_kv_vnode]
  Complete:   [riak_pipe_vnode]

Index: 365375409332725729550921208179070754913983135744
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 593735040165679310520246963290989976735222595584
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 822094670998632891489572718402909198556462055424
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 1050454301831586472458898473514828420377701515264
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 1278813932664540053428224228626747642198940975104
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

-------------------------------------------------------------------------------
Owner:      dev5@127.0.0.1
Next Owner: $resize

Index: 182687704666362864775460604089535377456991567872
  Waiting on: [riak_kv_vnode]
  Complete:   [riak_pipe_vnode]

Index: 411047335499316445744786359201454599278231027712
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 639406966332270026714112114313373821099470487552
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 867766597165223607683437869425293042920709947392
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 1096126227998177188652763624537212264741949407232
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

Index: 1324485858831130769622089379649131486563188867072
  Waiting on: [riak_kv_vnode,riak_pipe_vnode]

-------------------------------------------------------------------------------

============================== Unreachable Nodes ==============================
All nodes are up and reachable
```

Using `riak-admin transfers` will provide you more information about the partitions that are currently in progress. 

```bash
$> riak-admin transfers
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

You can verify that the resize operation is no longer running using the `transfers` command and verifying that there are no longer active or pending transfers:

```
riak-admin transfers

# Response:
No transfers active
```

You can also verify that there are the expected number of partitions in the ring by connecting to `riak attach` and running this snippet:

```erlang
length(riak_core_ring:all_owners(element(2,riak_core_ring_manager:get_my_ring()))).
```

```bash
riak attach
```

In the EShell:

```erlang
Remote Shell: Use "Ctrl-C a" to quit. q() or init:stop() will terminate the riak node.
Erlang R16B02 (erts-5.10.3) [source] [64-bit] [smp:8:8] [async-threads:10] [kernel-poll:false]

Eshell V5.10.3  (abort with ^G)
(dev1@127.0.0.1)1> length(riak_core_ring:all_owners(element(2,riak_core_ring_manager:get_my_ring()))).
64
(dev1@127.0.0.1)2> 
```

## Aborting a Ring Resize Already in Progress 

The process to abort a currently running resize is very similar to the process to set one up. Submit a `resize-ring abort` request, plan it, and commit it using `riak-admin cluster ...`

Submit the request:

```
$> riak-admin cluster resize-ring abort
Success: staged abort resize ring request
```

View the planned changes:

```
$> riak-admin cluster plan
=============================== Staged Changes ================================
Action         Details(s)
-------------------------------------------------------------------------------
resize-ring    abort. current size: 128
-------------------------------------------------------------------------------


NOTE: Applying these changes will result in 1 cluster transition

###############################################################################
                         After cluster transition 1/1
###############################################################################

================================= Membership ==================================
Status     Ring    Pending    Node
-------------------------------------------------------------------------------
valid      20.3%      --      'dev1@127.0.0.1'
valid      20.3%      --      'dev2@127.0.0.1'
valid      20.3%      --      'dev3@127.0.0.1'
valid      19.5%      --      'dev4@127.0.0.1'
valid      19.5%      --      'dev5@127.0.0.1'
-------------------------------------------------------------------------------
Valid:5 / Leaving:0 / Exiting:0 / Joining:0 / Down:0
```

Commit or clear the planned cluster changes.

```
$> riak-admin cluster commit
Cluster changes committed
```
