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

## Read-before-Write and Causal Context

Riak will return an encoded [[causal context]] with every "fetch" or
"read" request that does not result in a `not found` response. This
context (which is simply a non-human-readable byte string) tells Riak
how to resolve concurrent writes, essentially representing the "last
seen" version of the object to which the client made modifications. In
order to prevent [[sibling explosion|Causal Context#Sibling]], clients
should always return this causal context to Riak when updating an
object. Because of this, it is essential that object be fetched before
being written (except in the case where Riak selects the key or there is
_a priori_ knowledge that the object is new). Client libraries should
either provide means for easily performing such [[update
operations|Object Updates]] or perform read-before-write operations
automatically. This can reduce operational issues by limiting [[sibling
explosion|Causal Context#Sibling-Explosion]].  Clients may also choose
to perform automatic [[sibling resolution|Client Implementation
Guide#Sibling-Resolution]] on read.

## Discourage Expensive Operations

A number of operations (e.g. [[listing buckets|HTTP List Buckets]] or
[[lising keys|HTTP List Keys]]), while useful, are extrenely expensive
to run in production. A well-behaved client should expose all
functionality but provide some sort of warning to the developer when
they choose to perform those expensive operations.

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

## Nodes and Clusters

When writing Riak clients, it's important to remember that Riak _always_
functions as a [[clustered|Clusters]] (i.e. [[multi-node|Riak
Glossary#Node]]) system, and connecting clients need to be built to
interact with all nodes in the cluster on the basis of each node's host
and port.

While it's certainly possible to build clients to only interact with a
single node, this would mean that your client's users will need to
create their own cluster interaction logic. Instead, you should build
your client to do things like the following:

* periodically [[ping|PBC Ping]] nodes to make sure they're still online
* recognize when nodes are no longer responding and then stop sending
    requests to those nodes (until they come back online)
* provide a load-balancing scheme to spread interactions across nodes
    (or provide multiple schemes, or an interface whereby users can
    register their own schemes)

In general, you should think of the cluster interaction level as a kind
of stateful registry of healthy, responsive nodes. In some systems, it
might also be necessary to to have configurable parameters for
connections to Riak, e.g. minimum and/or maximum concurrent connections.

## Bucket Types

Prior to Riak 2.0, the location of objects in Riak was determined by
[[bucket|Buckets]] and [[key|Keys and Objects#Keys]]. In version 2.0,
[[bucket types|Using Bucket Types]] were introduced as a third
namespacing layer in addition to buckets and keys. Connecting clients
now need to either specify a bucket type or use the [[default|Using
Bucket Types#Default-Bucket-Properties]] type for all K/V operations.
Although creating, listing, modifying, and activating bucket types can
be accomplished only via the [[command line|Using Bucket
Types#Managing-Bucket-Types-Through-the-Command-Line]], your client
should provide an interface for seeing which bucket properties are
associated with a bucket type. More information can be found in our
documentation on [[bucket types and Protocol Buffers|PBC Get Bucket
Type]].

One of the changes to be aware of when building clients is that Riak has
changes its querying structure to accommodate bucket types. When
performing K/V operations, you now need to specify a bucket type _in
addition_ to a bucket and key. This means that the structure of all K/V
operations needs to be modified to allow for this. We'd also recommend
enabling users to perform K/V operations without specifying a bucket
type, in which case the `default` type is used. In the official Python
client, for example, the following two reads are equivalent:

```python
client.bucket('fruits').get('apple')
client.bucket_type('default').bucket('fruits').get('apple')
```

## Dealing with Objects and Content Types

One of the trickier things about dealing with objects in Riak is that
objects can be of any data type you choose (Riak Data Types are a
different matter, and covered in the [[section below]]). You can store
JSON, XML, raw binaries, strings, mp3s, MPEGs (though you should
probably consider [Riak
CS](http://docs.basho.com/riak/latest/dev/references/protocol-buffers/get-bucket-type/)
for larger files like that), and so on. While this makes Riak an
extremely flexible database, it means that clients need to be able to
work with a wide variety of content types.

All objects stored in Riak must have a specified content type, e.g.
`application/json`, `text/plain`, `application/octet-stream`, etc. While
a Riak client may not need to be able to handle _all_ data types, a
client intended for wide use should be able handle at least the
following:

* JSON
* XML
* plain text
* binaries

You should also strongly consider building automatic type handling into
your client. When the official Ruby and Python clients, for example,
read JSON from Riak, they automatically convert that JSON to hashes and
dicts (respectively). The Java client, to give another example,
automatically converts
[POJOs](http://en.wikipedia.org/wiki/Plain_Old_Java_Object) to JSON by
default and enables you to automatically convert stored JSON to [custom
POJO classes](http://en.wikipedia.org/wiki/Plain_Old_Java_Object) when
fetching objects, which enables you to easily interact with Riak in a
type-specific way. If you're writing a client in a language with strong
type safety, this might be a good thing to offer users.

Another important thing to bear in mind: all of your client interactions
with Riak should be [UTF-8](http://en.wikipedia.org/wiki/UTF-8)
complient, not just for the data stored in objects but also for things
like bucket, key, and bucket type names. In other words, your client
should be able to store an object in the bucket `Möbelträgerfüße` with
the key `tête-à-tête`.
