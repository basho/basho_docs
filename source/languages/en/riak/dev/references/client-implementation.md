---
title: Client Implementation Guide
project: riak
version: 1.2.0+
document: tutorials
toc: true
audience: advanced
keywords: [interface, client]
---

This section details aspects of the Riak client interfaces and can be
used as a guide for understanding how to interact with Riak and how to
implement a compliant client library or application.  Below are some
high-level recommendations for implementing well-behaved clients.

## Hide Transport Details

Although Riak's two interfaces do not have complete feature-parity,
client libraries should make an effort to hide implementation details
of the protocols from applications, presenting a uniform interface.
This will reduce the amount of code changes needed to select a
different transport mechanism for operational reasons.

## Retry Requests

As described in the [[Eventual Consistency]] section, there are many
scenarios that can result in temporary inconsistency, which may cause
the appearance of stale data or sibling objects.  To react
appropriately to unexpected or unsuccessful results, clients should
retry requests a small number of times before failing to the
caller. For example, when fetching a key that might be inconsistent
among its replicas, this gives [[read repair|Replication#Read-Repair]]
a chance to update the stale copies. Retries also may give the client
an opportunity to reconnect to the Riak node (or a different node in
the cluster) when the original connection is lost for whatever reason.
A number of operations in Riak are idempotent and thus can be retried
without side-effects.

## Sibling Resolution

In order to give applications the opportunity to recover from
conflicting or partitioned writes to a key, Riak can be configured to present the
client multiple versions of the value, also known as
[[siblings|Vector Clocks#Siblings]].  It then becomes the
responsibility of the application to resolve those siblings in a way
that is meaningful to the application domain.  Clients should provide
a way to encapsulate resolution behavior such that it can be run
automatically when sibling values are detected, potentially multiple
times in the course of a fetch or storage operation.  Without sibling
resolution,
[[the number of stored versions will continually grow|Vector Clocks#Siblings]],
resulting in degraded performance across the cluster in the form of
extremely high per-operation latencies or apparent unresponsiveness.

## Read-before-Write & Vector Clocks

Riak will return an encoded [[vector clock|Vector Clocks]] with every
"fetch" or "read" request that does not result in a "not found"
response. This vector clock tells Riak
how to resolve concurrent writes, essentially representing the "last
seen" version of the object to which the client made modifications. In
order to prevent
[[sibling explosion|Vector Clocks#Sibling]], clients should
always use this vector clock when updating an object.
Therefore, it is essential that
keys are fetched before being written (except in the case where Riak
selects the key or there is _a priori_ knowledge that the key is new).
Client libraries that make this automatic will reduce operational
issues by limiting sibling explosion.  Clients may also choose to
perform automatic [[Sibling Resolution|Client Implementation Guide#Sibling-Resolution]] on read.

## Discourage Expensive Operations

A number of operations (e.g. [[HTTP List Keys]]), while useful, are
expensive to run in production. A well-behaved client should expose
all functionality, but warn the developer when they choose to perform
those expensive operations.

## Nagle's Algorithm

In most cases &mdash; especially when using the [[PBC API]], which tends to
use small messages &mdash; clients should set the `TCP_NODELAY` flag on
opened socket connections to Riak, which disables
[Nagle's Algorithm](http://en.wikipedia.org/wiki/Nagle%27s_algorithm). Latency
profiles having a minimum of 40 milliseconds often indicate the
presence of Nagle on either end of the connection.  If the client
application has significant round-trip latency to the Riak cluster,
disabling Nagle will have little effect, but for well-connected
clients it can significantly reduce latency.
