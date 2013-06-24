---
title: Buckets
project: riak
version: 0.10.0+
document: appendix
audience: intermediate
keywords: [appendix, concepts]
moved: {
  '1.4.0-': '/references/appendices/concepts/Buckets/'
}
---

Buckets are used to define a virtual keyspace and provide the ability to define
isolated non-default configuration. Buckets might be compared to tables or
folders in relational databases or file systems, respectively. Buckets with
default configuration are essentially free, while non-default configuration will
be gossiped around the ring.

## Configuration

For each bucket a number of configuration properties can be selectively defined,
overriding the defaults.

### n_val

*integer* (default: `3`). Specifies the number of copies of each object to be
stored in the cluster. See [[Replication]].

### allow_mult

*boolean* (default: `false`). Determines whether sibling values can be created.
See [[Siblings|Vector Clocks#Siblings]].

### last_write_wins

*boolean* (default: `false`). Indicates if an object's vector clocks will be
used to decide the canonical write based on time of write in the case of a
conflict. See [[Conflict resolution|Concepts#Conflict-resolution]].

### r, pr, w, dw, pw, rw

`all`, `quorum`, `one`, or an *integer* (default: `quorum`). Sets for reads and
writes the number of responses required before an operation is considered
successful. See [[Reading Data|Concepts#Reading-Data]] and [[Writing and
Updating Data|Concepts#Writing and Updating Data]].

### precommit

A list of erlang or javascript functions to be executed before writing an
object. See [[Pre-Commit Hooks|Using Commit Hooks#Pre-Commit-Hooks]].

### postcommit

A list of erlang functions to be executed after writing an object. See
[[Post-Commit Hooks|Using Commit Hooks#Post-Commit-Hooks]].

For more details on setting bucket properties see [[Configuration
Files|Configuration Files#default_bucket_props]],
[[HTTP Set Bucket Properties]], or the documentation for your client driver.

### backend
Specify which named backend to use for the bucket when using `riak_kv_multi_backend`.
