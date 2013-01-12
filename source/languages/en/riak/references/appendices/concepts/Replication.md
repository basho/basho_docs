---
title: Replication
project: riak
version: 0.10.0+
document: appendix
toc: true
audience: intermediate
keywords: [appendix, concepts]
---

Replication is fundamental and automatic in Riak, providing security that your data will still be there if a node in your Riak cluster goes down. All data stored in Riak will be replicated to a number of nodes in the cluster according to the n_val property set on the bucket.

<div class="note">

If you're looking for Riak Enterprise's Multi Data Center 
Replication, please click [[here|Multi-Data-Center-Replication-Architecture]].

</div>

## Selecting an N value (n_val)
By default, Riak chooses an n_val of 3 for you. This means that data stored in the bucket will be replicated to 3 different nodes. For this to be effective, you need at least 3 nodes in your cluster.

How to choose an N value depends largely on the application and the shape of the data. If your data is very transient and can be reconstructed easily by the application, choosing a lower N value will give you greater performance. If you need high assurance that data is available even after node failure, increasing the N value will help protect against loss. How many nodes do you expect will fail at any one time? Choose an N value larger than that and your data will still be accessible when they go down.

The N value also affects the behavior of read (GET) and write (PUT) requests. The tunable parameters you can submit with requests are bound by the N value. For example, if N=3, the maximum read quorum (known as “R”) you can request is also 3. If some nodes containing the data you are requesting are down, an R value larger than the number of available nodes with the data will cause the read to fail.

## Setting the N value (n_val)
To change the N value for a bucket issue a PUT request to the bucket with the new value:

```bash
curl -XPUT -H "Content-Type: application/json" -d '{"props":{"n_val":5}}' http://riak-host:8098/riak/bucket
```

Changing the N value after a bucket has data in it is **not recommended**. If you do change the value, especially increasing it, you might need to force read repair. Overwritten objects and newly stored objects will automatically be replicated to the correct number of nodes.

## Read Repair
Read repair occurs when a successful read occurs — that is, the quorum was met — but not all replicas from which the object was requested agreed on the value. There are two possibilities here for the errant nodes:

  1. The node responded with a “not found” for the object, meaning it doesn't have a copy.
  2. The node responded with a vector clock that is an ancestor of the vector clock of the successful read.

When this situation occurs, Riak will force the errant nodes to update their object values based on the value of the successful read.

### Forcing Read Repair
When you increase the n_val on the bucket, you may start to get failed read operations, especially if the R value you use is larger than the number of replicas that originally stored the object. Forcing read repair will solve this issue.

For each object that fails read (or the whole bucket, if you like), read the object using an R value less than or equal to the original number of replicas. For example, if your original n_val was 3 and you increased it to 5, perform your read operations with R=3 or less. This will cause the nodes that do not have the object(s) yet to respond with “not found”, invoking read repair.

## So what does N=3 really mean?
N=3 simply means that three copies of each piece of data will be stored in the cluster. That is, three different partitions/vnodes will receive copies of the data. **There are no guarantees that the three replicas will go to three separate physical nodes**; however, the built-in functions for determining where replicas go attempts to distribute the data evenly.

As nodes are added and removed from the cluster, the ownership of partitions changes and may result in an uneven distribution of the data. On some rare occasions, Riak will also aggressively reshuffle ownership of the partitions to achieve a more even balance.

For cases where the number of nodes is less than the N value, data will likely be duplicated on some nodes. For example, with N=3 and 2 nodes in the cluster, one node will likely have one replica, and the other node will have two replicas.

## Understanding replication by example
To better understand how data is replicated in Riak let's take a look at a put request for the bucket/key pair <<"my_bucket">>/<<"my_key">>. Specifically we`ll focus on two parts of the request, routing an object to a set of partitions and storing an object on a partition.

### Routing an object to a set of partitions

  * Assume we have 3 nodes
  * Assume we store 3 replicas per object (N=3)
  * Assume we have 8 partitions (ring_creation_size=8)

**It is not recommended to use such a small ring size; this is for demonstration purposes only.**

With only 8 partitions our ring will look approximately as follows (response from riak_core_ring_manager:get_my_ring/0 truncated for clarity):

```erlang
(dev1@127.0.0.1)3> {ok,Ring} = riak_core_ring_manager:get_my_ring().
[{0,'dev1@127.0.0.1'},
{182687704666362864775460604089535377456991567872, 'dev2@127.0.0.1'},
{365375409332725729550921208179070754913983135744, 'dev3@127.0.0.1'},
{548063113999088594326381812268606132370974703616, 'dev1@127.0.0.1'},
{730750818665451459101842416358141509827966271488, 'dev2@127.0.0.1'},
{913438523331814323877303020447676887284957839360, 'dev3@127.0.0.1'},
{1096126227998177188652763624537212264741949407232, 'dev1@127.0.0.1'},
{1278813932664540053428224228626747642198940975104, 'dev2@127.0.0.1'}]
```

The node handling this request hashes the Bucket/Key combination:

```erlang
(dev1@127.0.0.1)4> DocIdx = riak_core_util:chash_key({<<"my_bucket">>, <<"my_key">>}).
<<183,28,67,173,80,128,26,94,190,198,65,15,27,243,135,127,121,101,255,96>>
```

The DocIdx hash is a 160 bit integer:

```erlang
(dev1@127.0.0.1)5> <<I:160/integer>> = DocIdx.
<<183,28,67,173,80,128,26,94,190,198,65,15,27,243,135,127,121,101,255,96>>
(dev1@127.0.0.1)6> I.
1045375627425331784151332358177649483819648417632
```

The node looks up the hashed key in the ring which returns a list of “preferred” partitions for the given key.

```erlang
(node1@127.0.0.1)> Preflist = riak_core_ring:preflist(DocIdx, Ring).
[{1096126227998177188652763624537212264741949407232, 'dev1@127.0.0.1'},
{1278813932664540053428224228626747642198940975104, 'dev2@127.0.0.1'},
{0, 'dev1@127.0.0.1'},
{182687704666362864775460604089535377456991567872, 'dev2@127.0.0.1'},
{365375409332725729550921208179070754913983135744, 'dev3@127.0.0.1'},
{548063113999088594326381812268606132370974703616, 'dev1@127.0.0.1'},
{730750818665451459101842416358141509827966271488, 'dev2@127.0.0.1'},
{913438523331814323877303020447676887284957839360, 'dev3@127.0.0.1'}]
```

The node chooses the first N partitions from the list, the remaining partitions of the “preferred” list are retained as fallbacks to use if any of the target partitions are unavailable.

```erlang
(dev1@127.0.0.1)9> {Targets, Fallbacks} = lists:split(N, Preflist).
{[{1096126227998177188652763624537212264741949407232, 'dev1@127.0.0.1'},
{1278813932664540053428224228626747642198940975104, 'dev2@127.0.0.1'},
{0,'dev1@127.0.0.1'}],
[{182687704666362864775460604089535377456991567872, 'dev2@127.0.0.1'},
{365375409332725729550921208179070754913983135744, 'dev3@127.0.0.1'},
{548063113999088594326381812268606132370974703616, 'dev1@127.0.0.1'},
{730750818665451459101842416358141509827966271488, 'dev2@127.0.0.1'},
{913438523331814323877303020447676887284957839360, 'dev3@127.0.0.1'}]}
```

The partition information returned from the ring contains a partition identifier and the parent node of that partition:

```erlang
{1096126227998177188652763624537212264741949407232, 'dev1@127.0.0.1'}
```

The requesting node sends a message to each parent node with the object and partition identifier (pseudo-code for clarity):

```erlang
'dev1@127.0.0.1' ! {put, Object, 1096126227998177188652763624537212264741949407232}
'dev2@127.0.0.1' ! {put, Object, 1278813932664540053428224228626747642198940975104}
'dev1@127.0.0.1' ! {put, Object, 0}
```

If any of the target partitions fail the node sends the object to one of the fallbacks. When the message is sent to the fallback node the message references the object and original partition identifier. For example, if `dev2@127.0.0.1` were unavailable the requesting node would then try each of the fallbacks. The fallbacks in this example are:

```erlang
{182687704666362864775460604089535377456991567872, 'dev2@127.0.0.1'}
{365375409332725729550921208179070754913983135744, 'dev3@127.0.0.1'}
{548063113999088594326381812268606132370974703616, 'dev1@127.0.0.1'}
```

The next available fallback node would be `dev3@127.0.0.1`. The requesting node would send a message to the fallback node with the object and original partition identifier:

```erlang
'dev3@127.0.0.1' ! {put, Object, 1278813932664540053428224228626747642198940975104}
```

Note that the partition identifier in the message is the same that was originally sent to `dev2@127.0.0.1` only this time it is being sent to `dev3@127.0.0.1`. Even though `dev3@127.0.0.1` is not the parent node of that partition, it is smart enough to hold on to the object until `dev2@127.0.0.1` returns to the cluster.

## Processing partition requests
Processing requests per partition is fairly simple. Each node runs a single process (riak_kv_vnode_master) that distributes requests to individual partition processes (riak_kv_vnode). The riak_kv_vnode_master process maintains a list of partition identifiers and corresponding partition processes. If a process does not exist for a given partition identifier a new process is spawned to manage that partition.

The riak_kv_vnode_master process treats all requests the same and spawns partition processes as needed even when nodes receive requests for partitions they do not own. When a partition`s parent node is unavailable, requests are sent to fallback nodes (handoff). The riak_kv_vnode_master process on the fallback node spawns a process to manage the partition even though the partition does not belong to the fallback node.

The individual partition processes perform hometests throughout the life of the process. The hometest checks if the current node (node/0) matches the parent node of the partition as defined in the ring. If the process determines that the partition it is managing belongs on another node (the parent node) it will attempt to contact that node. If that parent node responds the process will handoff any objects it has processed for that partition and shutdown. If that parent node does not respond the process will continue to manage that partition and check the parent node again after a delay. The hometest is also run by partition processes to account for changes in the ring such as the addition or removal of nodes to the cluster.
