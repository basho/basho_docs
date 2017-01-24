---
title: "Riak Core Fundamentals"
description: "The fundamentals of Riak core in TS"
menu:
  riak_ts-1.5.1:
    name: "Core Fundamentals"
    identifier: "core_fundamentals_riakts"
    weight: 100
    parent: "using"
project: "riak_ts"
project_version: "1.5.1"
toc: true
aliases:
    - /riakts/1.5.1/using/core-fundamentals/
canonical_link: "https://docs.basho.com/riak/ts/latest/using/core-fundamentals"
---


[Riak KV]: /riak/kv/2.2.0/

Riak TS shares the same core codebase as [Riak KV], which allows you to operate a TS cluster much the same as you would operate a KV cluster.

This page will outline [key differences](#differences) between Riak KV and Riak TS, as well as provide [links](#links) to help you get started learning to operate a Riak TS cluster.

## Differences

Though they share a codebase, Riak TS differs from Riak KV in a few ways. Here's a handy table tracking features in Riak KV compared to Riak TS:

| Feature                  | Riak KV | Riak TS         |
| :------------------------|:--------| :---------------|
| active anti-entropy      | Yes     | No              |
| data types (CRDTs)       | Yes     | KV buckets only |
| global expiry            | Yes     | Yes             |
| Bitcask (backend)        | Default | No              |
| LevelDB (backend)        | Yes     | Default         |
| memory (backend)         | Yes     | No              |
| multi backend            | Yes     | No              |
| KV single-key operations | Yes     | KV buckets only |
| Map Reduce               | Yes     | KV buckets only |
| MDC                      | Yes     | v3 only         |
| Riak SQL                 | No      | Yes             |
| Riak security            | Yes     | Yes             |
| riak shell               | No      | Yes             |
| Riak search              | Yes     | No              |
| Secondary Indexes        | Yes     | KV buckets only |
| Statistics               | Yes     | Yes             |

There are some additional points it is important to note:

* In Riak TS, LevelDB is the only compatible backend.
* AAE is off by default and not currently supported in Riak TS.
* In Riak TS, `SELECT` does not invoke read-repair, but single key `GET` does.


## Links

Below, you will find links to Riak KV documents that are applicable and helpful for running a Riak TS cluster.

### Configuration

<a href="http://docs.basho.com/riak/kv/2.2.0/configuring/basic/" target="_blank">Basic Configuration</a> will help you set up your Riak core configuration.

<a href="http://docs.basho.com/riak/kv/2.2.0/configuring/managing/" target="_blank">Managing Configuration</a> will show you how to retrieve your configuration, check your settings, and debug your configuration.

<a href="http://docs.basho.com/riak/kv/2.2.0/configuring/reference/" target="_blank">Configuration Reference</a> provides you with everything you need to know about configuring Riak core.

<a href="http://docs.basho.com/riak/kv/2.2.0/configuring/load-balancing-proxy/" target="_blank">Load Balancing</a> will walk you through configuring a load balancer with your Riak cluster.


### Cluster Operations

<a href="http://docs.basho.com/riak/kv/2.2.0/using/running-a-cluster/" target="_blank">Running a Cluster</a> gives you a basic walkthrough of how to run a Riak cluster.

<a href="http://docs.basho.com/riak/kv/2.2.0/using/admin/" target="_blank">Cluster Administration</a> provides a series of links to information on various ways to administer your cluster.

<a href="http://docs.basho.com/riak/kv/2.2.0/using/cluster-operations/adding-removing-nodes/" target="_blank">Adding & Removing Nodes</a> walks you through the process of adding or removing nodes in your cluster.

<a href="http://docs.basho.com/riak/kv/2.2.0/using/cluster-operations/changing-cluster-info/" target="_blank">Changing Cluster Information</a> will show you how to change various parts of your cluster.

<a href="http://docs.basho.com/riak/kv/2.2.0/using/cluster-operations/replacing-node/" target="_blank">Replace a Node</a> is a step-by-step guide for how to replace a node in your cluster.

<a href="http://docs.basho.com/riak/kv/2.2.0/using/cluster-operations/inspecting-node/" target="_blank">Inspect a Node</a> shows you the steps and tools for inspecting nodes in your cluster.

<a href="http://docs.basho.com/riak/kv/2.2.0/using/cluster-operations/logging/" target="_blank">Logging</a> will provide you the steps for enabling and disabling debug logging.

<a href="http://docs.basho.com/riak/kv/2.2.0/using/cluster-operations/backing-up/" target="_blank">Backing Up</a> is a how-to guide for backing up your data.

<a href="http://docs.basho.com/riak/kv/2.2.0/using/cluster-operations/handoff/" target="_blank">Handoff</a> will tell you everything you need to know to enable and disable handoff.


### Repair, Tuning, and Reference

<a href="http://docs.basho.com/riak/kv/2.2.0/using/repair-recovery/" target="_blank">Repair & Recovery</a> will cover all of the important topics of what can go wrong and what you can do to fix it.

<a href="http://docs.basho.com/riak/kv/2.2.0/using/performance/" target="_blank">Performance</a> will give you all the information you need to tune your cluster configurations to optimize performance.

<a href="http://docs.basho.com/riak/kv/2.2.0/using/reference/" target="_blank">Reference</a> will provide you with explanations of various core functions, such as logging and handoff.
