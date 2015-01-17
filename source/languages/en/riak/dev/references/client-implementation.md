---
title: Client Implementation Guide
project: riak
version: 1.2.0+
document: tutorials
toc: true
audience: advanced
keywords: [interface, client]
moved: {
  '1.4.0-': '/tutorials/Client-Implementation-Guide'
}
---

This document details aspects of the Riak client interfaces and can be
used as a general guide for understanding how to interact with Riak and
how to implement a compliant client library or application. Below are
some high-level recommendations for implementing well-behaved clients.

## Hide Transport Details

Although Riak's two interfaces do not have complete feature-parity,
client libraries should make an effort to hide implementation details of
the protocols from applications, presenting a uniform interface. This
will reduce the amount of code changes needed to select a different
transport mechanism for operational reasons.

## Protocol Buffers API

While Riak continues to have a fully featured [[HTTP API]] for the sake
of backwards compatibility, we strongly recommend building new clients
to use the [[Protocol Buffers|PBC API]] API instead, primarily because
internal tests at Basho have shown performance gains of 25% or more when
using Protocol Buffers instead of HTTP.

For a general introduction to Protocol Buffers, we recommend checking
out [Google's official
documentation](http://code.google.com/p/protobuf/). In essence, using
Protocol Buffers involves the following steps:

1. Finding a Protocol Buffers message generator in the language of your
choice and converting Riak's [`.proto`
files](https://github.com/basho/riak_pb/tree/develop/src) to native
code.
1. Once you've generated all of the necessary messages, you'll need to
implement a transport layer to interface with Riak. A full list of
Riak-specific Protocol Buffers messages can be found on the [[PBC API]]
page. The official [Python
client](https://github.com/basho/riak-python-client), for example, has a
single
[`RiakPbcTransport`](https://github.com/basho/riak-python-client/blob/3c7530f047a3b29)
class that handles all message building, sending, and receiving, while
the official [Java client](https://github.com/basho/riak-java-client)
takes a more piecemeal approach to messages (as shown by the
[`FetchOperation`](https://github.com/basho/riak-java-client/blob/a74a9b99eda5e5f79c4be16dd432e04317b45e84/src/main/java/com/basho/riak/client/core/operations/FetchOperation.java))
class, which handles reads from Riak.
1. Once the transport layer is in place, you can begin building
higher-level abstractions for your client.

The drawback behind using Protocol Buffers is that it's not as widely
known as HTTP and has a bit of a learning curve for those who aren't
used to it. The good news, however, is that Google offers official
support for C++, Java, and Python and [many other
languages](https://github.com/google/protobuf/wiki/Third-Party-Add-ons)
have strong community support.

## Retry Requests

As described in the [[Eventual Consistency]] document, there are many
scenarios that can result in temporary inconsistency, which may cause
the appearance of stale data or [[sibling objects|Causal
Context#Siblings]]. To react appropriately to unexpected or unsuccessful
results, clients should enable users to retry requests a specifiable
number of times before failing to the caller. For example, when fetching
a key that might be inconsistent among its replicas, this gives [[read
repair|Active Anti-Entropy#Read-Repair-vs-Active-Anti-Entropy]] a chance
to update the stale copies. Retries also may give the client an
opportunity to reconnect to the Riak node (or a different node in the
cluster) when the original connection is lost for whatever reason. A
number of operations in Riak
are idempotent and thus can be retried without side-effects.

## Sibling Resolution

In order to give applications the opportunity to recover from
conflicting or partitioned writes to a key, Riak can be configured to
present the client multiple versions of the value, also known as
[[siblings|Causal Context#Siblings]]. It then becomes the responsibility
of the application to resolve those siblings in a way that is meaningful
to the application domain. Clients should provide a way to encapsulate
resolution behavior such that it can be run automatically when sibling
values are detected, potentially multiple times in the course of a fetch
or storage operation. Without sibling resolution, [[the number of stored
versions will continually grow|Causal Context#Siblings]], resulting in
degraded performance across the cluster in the form of extremely high
per-operation latencies or apparent unresponsiveness.

## Read-before-Write & Causal Context

Riak will return an encoded [[causal context]] with every "fetch" or
"read" request that does not result in a `not found` response. This
vector clock tells Riak how to resolve concurrent writes, essentially
representing the "last seen" version of the object to which the client
made modifications. In order to prevent [[sibling explosion|Causal
Context#Sibling]], clients should always use this vector clock when
updating an object. Therefore, it is essential that keys are fetched
before being written (except in the case where Riak selects the key or
there is _a priori_ knowledge that the key is new). Client libraries
that make this automatic will reduce operational issues by limiting
sibling explosion.  Clients may also choose to perform automatic
[[Sibling Resolution|Client Implementation Guide#Sibling-Resolution]] on
read.

## Discourage Expensive Operations

A number of operations (e.g. [[HTTP List Keys]]), while useful, are
expensive to run in production. A well-behaved client should expose all
functionality, but warn the developer when they choose to perform those
expensive operations.

## Nagle's Algorithm

In most cases --- especially when using the [[PBC API]], which tends to
use small messages --- clients should set the `TCP_NODELAY` flag on
opened socket connections to Riak, which disables [Nagle's
Algorithm](http://en.wikipedia.org/wiki/Nagle%27s_algorithm). Latency
profiles having a minimum of 40 milliseconds often indicate the presence
of Nagle on either end of the connection.  If the client application has
significant round-trip latency to the Riak cluster, disabling Nagle will
have little effect, but for well-connected clients it can significantly
reduce latency.

## Compatibility with Riak 2.0

The release of [[Riak 2.0]] has brought a variety of fundamental changes
to Riak that client builders and maintainers should be aware of,
including a variety of new features, such as [[security|Authentication
and Authorization]] and [[Riak Data Types|Using Data Types]. The
sections below will list some of those changes and suggest approaches to
addressing them, including some examples from our official [[client
libraries]].

###
