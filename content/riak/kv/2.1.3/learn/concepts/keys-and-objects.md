---
title: "Keys and Objects"
description: ""
project: "riak_kv"
project_version: "2.1.3"
menu:
  riak_kv-2.1.3:
    name: "Keys and Objects"
    identifier: "learn_concepts_keys_objects"
    weight: 106
    parent: "learn_concepts"
toc: true
aliases:
  - /riak/2.1.3/theory/concepts/keys-and-values
  - /riak/kv/2.1.3/theory/concepts/keys-and-values
---

[concept buckets]: {{<baseurl>}}riak/kv/2.1.3/learn/concepts/buckets
[concept causal context vc]: {{<baseurl>}}riak/kv/2.1.3/learn/concepts/causal-context/#vector-clocks

In an RDBMS, data is organized by tables that are individually
identifiable entities. Within those tables exist rows of a data
organized into columns. It is possible to retrieve or update entire
tables, individual rows, or a group of columns within a set of
rows. In contrast, Riak has a simpler data model in which the Object
(explained below) is both the largest and smallest data element. When
performing any fetch or update operation in Riak, the entire Riak
Object must be retrieved or modified; there are no partial fetches or
updates.

## Keys

Keys in Riak are simply binary values (or strings) used to identify
Objects. From the perspective of a client interacting with Riak,
each bucket appears to represent a separate keyspace. It is important
to understand that Riak treats the bucket-key pair as a single entity
when performing fetch and store operations (see: [Buckets][concept buckets]).

## Objects

Objects are the only unit of data storage in Riak. Riak Objects are
essentially structs identified by bucket and key and composed of the
following parts: a bucket, key, vector clock, and a list of
metadata-value pairs. Normally, objects have only one metadata-value
pair, but when there are more than one, the object is said to have
"siblings". These siblings may occur both within a single node and
across multiple nodes, and do occur when either more than one actor
updates an object, a network partition occurs, or a stale vector clock
is submitted when updating an object (see: [Vector Clocks][concept causal context vc]).
