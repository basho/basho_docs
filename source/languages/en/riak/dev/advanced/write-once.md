---
title: Write Once Buckets
project: riak
version: 2.1.0+
document: tutorial
audience: intermediate
keywords: [developers, buckets]
---

Riak 2.1.0 introduces the concept of Write Once Buckets, buckets whose entries
are intended to be written exactly once, and never updated or over-written.
Buckets of this type circumvent the normal "coordinated PUT" path, which would
otherwise result in a read on the coordinating vnode before the write. Avoiding
coordinated puts results in higher throughput and lower put latency, at the cost
of different semantics in the degenerate case of sibling resolution.

## Configuration

When the new `write_once` bucket type property is set to `true`, buckets using
that type will treat all key/value entries as semantically "write once"; once
written, entries should not be modified or overwritten by the user.

The `write_once` property is a boolean property applied to a bucket type and may
only be set at bucket creation time. Once a bucket type has been set with this
property and activated, the `write_once` property may not be modified.

The `write_once` property is incompatible with specification of [[Data
Types|Using Data Types]] and [[Strong Consistency|Using Strong Consistency]].
Any attempt to create a bucket type with the `write_once` property set to `true`
and with data types or the strong consistency property set will fail.

The `write_once` property may not be set on the default bucket type, and may not
be set on individual buckets.

The `lww` and `allow_mult` properties are ignored, if the `write_once` property
is set to `true`.

The following example shows how to configure a bucket type with the `write_once`
property:

```
prompt$ riak-admin bucket-type create my-bucket-type '{"props": {"write_once": true}}'
my-bucket-type created

prompt$ riak-admin bucket-type activate my-bucket-type
my-bucket-type has been activated

prompt$ riak-admin bucket-type status my-bucket-type
my-bucket-type is active
...
write_once: true
...
```

## Quorum

The write path used by Write Once Buckets supports the `W`, `PW`, and `DW`
configuration values. However, if `DW` is specified, then the value of `W` is
taken to be the maximum of the `W` and `DW` values. For example, for `n_val 3`,
if `DW` is set to `all`, then `W` will be `3`.

This write additionally supports the `sloppy_quroum` property. If set to
`false`, only primary nodes will be selected for calculation of write quorum
nodes.

## Runtime

The Write Once path circumvents the normal coordinated PUT code path, and
instead sends write requests directly to all vnodes (or vnode proxies) in the
effective preference list for the write operation.

In place of the `put_fsm` used in the normal path, we introduce a collection of
new intermediate worker processes (implementing `gen_server` behavior). The role
of these intermediate processes is to dispatch put requests to vnode(proxie)s in
the preflist and to aggregate replies. Unlike the `put_fsm`, the write once
workers are long-lived for the lifecyle of the `riak_kv` application. They are
therefore stateful and store request state in a state-local dictionary.

The relationship between the `riak_client`, write once workers, and vnode
proxies is illustrated in the following diagram:

<br>
![Write Once](/images/write_once.png)
<br>

## Siblings

As mentioned, entries in Write Once Buckets are intended to be written only once
-- users who are not abusing the semantics of the bucket type should not be
updating or over-writing entries in buckets of this type. However, it is
possible for users to mis-use the API, accidentally or otherwise, which might
result in incomparable entries for the same key.

In the case of siblings, Write Once Buckets will resolve the conflict by
choosing the "least" entry, where sibling ordering is based on a deterministic
SHA-1 hash of the objects. While this algorithm is repeatable and deterministic
at the database level, it will have the appearance to user of "random write
wins".

## Handoff

The Write Once path supports handoff scenarios, such that if a handoff occurs
during PUTs in a Write Once Bucket, the values that have been written will be
handed off to the newly added Riak node.

## Asynchronous Writes

For backends that support asynchronous writes, the Write Once path will dispatch
a write request to the backend, and handle the response asynchronously. This
behavior allows the vnode to free itself for other work, instead of waiting on
the write response from the backend.

At the time of writing, the only backend that supports asynchronous writes is
leveldb. Riak will automatically fall back to synchronous writes with all other
backends.

<div class="note"><div class="title">Note on the `multi` backend</div>
The `multi` backend does not support asynchronous write. Therefore, if leveldb
is used with the `multi` backend, it will be used in synchronous mode.
</div>
