---
title: Scaling and Operating Riak Best Practices
project: riak
version: 1.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, best-practices]
moved: {
    '1.4.0-': '/cookbooks/best-practices'
}
---

## Disk Capacity

Filling up disks is bad, in general you should look to add capacity immediately when either:

 - The disk becomes greater than 80% full.
 - You have less than 10 days capacity remaining at current rates of growth.


## RAID Levels

Riak provides resilience through its built in redundancy.

 - RAID0 can be used to increase the performance at the expense of single node reliability.
 - RAID5/6 can be used to increase the reliability over RAID0 but still higher performance than single disks.
 - You should choose a RAID level (or no RAID) that you’re comfortable with.


## How much disk leeway to have

 - Adding new nodes instantly increases the total capacity of the cluster, but you should allow enough internal network capacity so that handing off existing data outpaces the arrival of new data.
 - Once you’ve reached a certain scale, where the amount of new data arriving is a small fraction of the clusters total capacity, you can add new nodes when you need them.
 - We would recommend having a week or two leeway, so if you estimate you are likely to run out of capacity you have plenty of time to add nodes and have handoff occur before the disks reach capacity.
 - For large volumes of storage it's probably prudent to add more capacity once you reach 80%.


## How much CPU capacity leeway to have

 - In a steady state, your peak CPU utilization ignoring other processes should be less than 30%.
 - This way you’ll have spare capacity to handle other process, like backups and handoff.


## How much Network Capacity leeway to have

 - Network traffic tends to be “bursty”, that is, it varies a lot, and quickly.
 - Your normal load, as averaged over a 10 minute period should be no more than 20% of the maximum capacity.
 - Riak generates 3-5 times the amount of intra-node traffic as inbound traffic, you should allow for this in your network design.


## When to add Nodes

You should add more nodes:

 - when you have reached 80% storage capacity,
 - when you have less than 10 days leeway before you expect the cluster to fill up, or
 - when current node IO/CPU activity is higher than average for extended period of time--for MapReduce operations especially.

An alternative to adding more nodes is to add more storage to the existing nodes. However, you should do this only if:

 - you’re confident that there is plenty of spare network and CPU capacity,  
   _AND_
 - you can upgrade storage _equally across all nodes_; otherwise, Riak will use no more storage on each node than is available on the node with the least storage. (Thus if one node uses 1 TB but the rest use 1.5 TB, Riak will still use only 1 TB on each node.)

## How to add Nodes

 - You should add as many additional nodes as you require in one operation.
 - Don’t add nodes one at a time if you’re adding multiple nodes.
 - You can limit the transfer rate so that priority is given to live customer traffic.


## Scaling

 - All large scale systems are bound by availability of some resource.
 - From a stability point of view the best state for a busy Riak cluster to be in is:
   - New network connections limited to ensure that:
   - Existing network connections consume most network bandwidth
   - CPU at < 30%
   - Disk IO at < 90%
 - You should use HAProxy or your application servers to limit new network connections to keep network and IO below 90% and CPU below 30%
