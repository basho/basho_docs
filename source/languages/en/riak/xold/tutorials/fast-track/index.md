---
title: The Riak Fast Track
project: riak
version: 0.10.0+
toc: false
document: tutorial
audience: beginner
keywords: [tutorial, fast-track]
next: "[[What is Riak?|What is Riak]]"
interest: false
versions: false
---

Riak is a distributed database built for the following reasons.

Purpose | Description
-------|-------
**Availability** | Riak replicates and retrieves data intelligently so it is available for read and write operations, even in failure conditions
**Fault-tolerance** | You can lose access to many nodes due to network partition or hardware failure and never lose data
**Operational simplicity** | Add new machines to your Riak cluster easily without incurring a larger operational burden - the same ops tasks apply to small clusters as large clusters
**Scalability** | Riak automatically distributes data around the cluster and yields a near-linear performance increase as you add capacity

## What is the Riak Fast Track?

The Riak Fast Track aims to get you up and running with Riak as quickly as possible, so that you can learn by doing.  It presents a series of modules that walk you through installing Riak, getting a four node cluster up and running, and performing basic operations that illustrate Riak’s core concepts. 

The Fast Track is designed for people with little or no experience with Riak, but can still be useful for more experienced users as well. From start to finish, this will probably take you around 45 minutes. 

## What does the Fast Track Cover?

The Fast Track takes you through these topics.

 Covers | Description
--------|-------------
[[What is Riak]] | A high level overview of Riak and its architecture
[[Building a Development Environment]] | Instructions on setting up a development cluster on your Machine
[[Basic Riak API Operations]] | A review of standard API operations
[[Loading Data and Running MapReduce Queries]] | Importing data and running simple MapReduce queries
[[Tunable CAP Controls in Riak]] | How to tune Riak for consistency and availability
