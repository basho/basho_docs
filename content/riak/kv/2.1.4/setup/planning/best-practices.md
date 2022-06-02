---
title: "Scaling and Operating Riak KV Best Practices"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Best Practices"
    identifier: "planning_best_practices"
    weight: 105
    parent: "planning"
toc: true
aliases:
  - /riak/2.1.4/ops/building/planning/best-practices
  - /riak/kv/2.1.4/ops/building/planning/best-practices
---

[use ref handoff]: {{<baseurl>}}riak/kv/2.1.4/using/reference/handoff
[config mapreduce]: {{<baseurl>}}riak/kv/2.1.4/configuring/mapreduce
[glossary aae]: {{<baseurl>}}riak/kv/2.1.4/learn/glossary/#active-anti-entropy-aae
[cluster ops add remove node]: {{<baseurl>}}riak/kv/2.1.4/using/cluster-operations/adding-removing-nodes

Riak KV is a database designed for easy operation and scaling. Below are some best practices that will enable you to improve performance and reliability at all stages in the life of your Riak cluster.

## Disk Capacity

Filling up disks is a serious problem in Riak. In general, you should
add capacity under the following conditions:

* a disk becomes more than 80% full
* you have fewer than 10 days of capacity remaining at current rates of
  growth

## RAID Levels

Riak provides resilience through its built-in redundancy.

* RAID0 can be used to increase the performance at the expense of
  single-node reliability
* RAID5/6 can be used to increase the reliability over RAID0 but still
  offers higher performance than single disks
* You should choose a RAID level (or no RAID) that you’re comfortable
  with

## Disk Leeway

* Adding new nodes instantly increases the total capacity of the
  cluster, but you should allow enough internal network capacity that
  [handing off][use ref handoff] existing data outpaces the arrival of new
  data.
* Once you’ve reached a scale at which the amount of new data arriving
  is a small fraction of the cluster's total capacity, you can add new
  nodes when you need them. You should be aware, however, that adding
  new nodes can actually _increase_ disk usage on existing nodes in the
  short term as data is rebalanced within the cluster.
* If you are certain that you are likely to run out of capacity, we
  recommend allowing a week or two of leeway so that you have plenty of
  time to add nodes and for [handoff][use ref handoff] to occur before the disks reach
  capacity
* For large volumes of storage it's usually prudent to add more capacity
  once a disk is 80% full

## CPU Capacity Leeway

* In a steady state, your peak CPU utilization, ignoring other
  processes, should be less than 30%
* If you provide sufficient CPU capacity leeway, you’ll have spare
  capacity to handle other processes, such as backups, [handoff][use ref handoff], and [active anti-entropy][glossary aae]

## Network Capacity Leeway

* Network traffic tends to be “bursty,” i.e. it tends to vary both quite
  a bit and quickly
* Your normal load, as averaged over a 10-minute period, should be no
  more than 20% of maximum capacity
* Riak generates 3-5 times the amount of intra-node traffic as inbound
  traffic, so you should allow for this in your network design

## When to Add Nodes

You should add more nodes in the following scenarios:

* you have reached 80% of storage capacity
* you have less than 10 days of leeway before you expect the cluster to
  fill up
* the current node's IO/CPU activity is higher than average for extended
  period of time, especially for [MapReduce][config mapreduce]
  operations

An alternative to adding more nodes is to add more storage to existing
nodes. However, you should do this only if:

* you’re confident that there is plenty of spare network and CPU
  capacity, _and_
* you can upgrade storage _equally across all nodes_. If storage is
  added in an unbalanced fashion, Riak will continue storing data
  equally across nodes, and the node with the smallest available storage
  space is likely to fail first. Thus, if one node uses 1 TB but the
  rest use 1.5 TB, Riak will overload the 1 TB node first.

The recommendations above should be taken only as general guidelines
because the specifics of your cluster will matter a great deal when
making capacity decisions. The following considerations are worth
bearing in mind:

* If your disks are 90% full but only filling up 1% per month, this
  might be a perfectly "safe" scenario. In cases like this, the velocity
  of adding new data is more important than any raw total.
* The burstiness of your write load is also an important consideration.
  If writes tend to come in large batches that are unpredictably timed,
  it can be more difficult to estimate when disks will become full,
  which means that you should probably over-provision storage as a
  precaution.
* If Riak shares disks with other processes or is on the system root
  mount point, i.e. `/`, we recommend leaving a little extra disk space
  in addition to the estimates discussed above, as other system
  processes might use disk space unexpectedly.

## How to Add Nodes

* You should add as many additional nodes as you require in one
  operation
* Don’t add nodes one at a time if you’re adding multiple nodes
* You can limit the transfer rate so that priority is given to live
  customer traffic

This process is explored in more detail in [Adding and Removing Nodes][cluster ops add remove node].

## Scaling

* All large-scale systems are bound by the availability of some
  resources
* From a stability point of view, the best state for a busy Riak cluster
  to maintain is the following:
  * New network connections are limited to ensure that existing network
    connections consume most network bandwidth
  * CPU at < 30%
  * Disk IO at < 90%
* You should use HAProxy or your application servers to limit new
  network connections to keep network and IO below 90% and CPU below
  30%.
