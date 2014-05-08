---
title: Active Anti-Entropy
project: riak
version: 1.3.0+
document: appendix
audience: intermediate
keywords: [aae, active anti-entropy]
---

In a [[clustered|Clusters]], [[eventually consistent|Eventual
Consistency]] system like Riak, conflicts between object replicas stored
on different nodes are an expected byproduct of node failure, concurrent
client updates, physical data loss and corruption, and other events that
distributed systems are built to handle.

## Read Repair

In versions of Riak prior to 1.3, replica conflicts were were healed via
[[read repair|Riak Glossary#read-repair]] alone, which is a _passive_
anti-entropy mechanism that heals object conflicts only when a read
request reaches Riak from a client. If the [[vnode|Riak Glossary#vnode]]
coordinating the request determines that different nodes hold divergent
values for the object, the repair process will begin.

The advantage of using read repair alone is that it is less expensive
for CPU and network resources. The drawback of this approach, however,
is that the healing process only reaches those objects that are read by
clients. 

_Active_ anti-entropy was added in Riak versions 1.3 and later to enable
conflict resolution to run as a background process that heals 

## Entropy Detection

## Correction