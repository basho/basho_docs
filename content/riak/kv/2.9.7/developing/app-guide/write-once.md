---
title: "Write Once"
description: ""
project: "riak_kv"
project_version: 2.9.7
menu:
  riak_kv-2.9.7:
    name: "Write Once"
    identifier: "app_guide_write_once"
    weight: 102
    parent: "developing_app_guide"
toc: true
version_history:
  in: "2.1.0+"
aliases:
  - /riak/2.9.7/dev/advanced/write-once
  - /riak/kv/2.9.7/dev/advanced/write-once
---

[glossary vnode]: {{<baseurl>}}riak/kv/2.9.7/learn/glossary/#vnode
[bucket type]: {{<baseurl>}}riak/kv/2.9.7/developing/usage/bucket-types
[Riak data types]: {{<baseurl>}}riak/kv/2.9.7/developing/data-types
[strong consistency]: {{<baseurl>}}riak/kv/2.9.7/developing/app-guide/strong-consistency

Write-once buckets are buckets whose entries are intended to be written exactly once and never updated or overwritten. Buckets of this type circumvent the normal "coordinated PUT" path, which would otherwise result in a read on the coordinating vnode before the write. Avoiding coordinated PUTs results in higher throughput and lower PUT latency, though at the cost of different semantics in the degenerate case of sibling resolution.

{{% note %}}
Write-once buckets do not support Riak commit hooks.  Because Riak objects are
inserted into the realtime queue using a postcommit hook, realtime replication
is unavailable for write-once buckets.  Fullsync replication will, however,
replicate the data.
{{% /note %}}

## Configuration

When the new `write_once` [bucket type][bucket type] parameter is set to
`true`, buckets of type will treat all key/value entries as semantically "write
once;" once written, entries should not be modified or overwritten by the user.

The `write_once` property is a boolean property applied to a bucket type and
may only be set at bucket creation time. Once a bucket type has been set with
this property and activated, the `write_once` property may not be modified.

The `write_once` property is incompatible with [Riak data types][Riak data types]
and [strong consistency][strong consistency], This means that if you attempt
to create a bucket type with the `write_once` property set to `true`, any
attempt to set the `datatype` parameter or to set the `consistent` parameter
to `true` will fail.

The `write_once` property may not be set on the default bucket type, and may
not be set on individual buckets. If you set the `lww` or `allow_mult`
parameters on a write-once bucket type, those settings will be ignored, as
sibling values are disallowed by default.

The following example shows how to configure a bucket type with the
`write_once` property:

```bash
riak-admin bucket-type create my-bucket-type '{"props": {"write_once": true}}'
# my-bucket-type created

riak-admin bucket-type activate my-bucket-type
# my-bucket-type has been activated

riak-admin bucket-type status my-bucket-type
# my-bucket-type is active
...
write_once: true
...
```

## Quorum

The write path used by write-once buckets supports the `w`, `pw`, and `dw`
configuration values. However, if `dw` is specified, then the value of `w` is
taken to be the maximum of the `w` and `dw` values. For example, for an `n_val`
of 3, if `dw` is set to `all`, then `w` will be `3`.

This write additionally supports the `sloppy_quorum` property. If set to
`false`, only primary nodes will be selected for calculation of write quorum
nodes.

## Runtime

The write-once path circumvents the normal coordinated PUT code path, and
instead sends write requests directly to all [vnodes][glossary vnode] (or
vnode proxies) in the effective preference list for the write operation.

In place of the `put_fsm` used in the normal path, we introduce a collection of
new intermediate worker processes (implementing `gen_server` behavior). The
role of these intermediate processes is to dispatch put requests to vnode or
vnode proxies in the preflist and to aggregate replies. Unlike the `put_fsm`,
the write-once workers are long-lived for the lifecycle of the `riak_kv`
application. They are therefore stateful and store request state in a state-
local dictionary.

The relationship between the `riak_client`, write-once workers, and vnode
proxies is illustrated in the following diagram:

<br>
![Write Once]({{<baseurl>}}images/write_once.png)
<br>

## Client Impacts

Since the write-once code path is optimized for writes of data that will not
be updated and therefore may potentially issue asynchronous writes, some
client features might not work as expected.  For example, PUT requests asking
for the object to be returned will behave like requests that do not
request the object to be returned when they are performed against write-once
buckets.


## Siblings

As mentioned, entries in write-once buckets are intended to be written only
once---users who are not abusing the semantics of the bucket type should not be
updating or over-writing entries in buckets of this type. However, it is
possible for users to misuse the API, accidentally or otherwise, which might
result in incomparable entries for the same key.

In the case of siblings, write-once buckets will resolve the conflict by
choosing the "least" entry, where sibling ordering is based on a deterministic
SHA-1 hash of the objects. While this algorithm is repeatable and deterministic
at the database level, it will have the appearance to the user of "random write
wins."

{{% note %}}
As mentioned in [Configuration](#configuration), write-once buckets and Riak
Data Types are incompatible because of this.
{{% /note %}}


## Handoff

The write-once path supports handoff scenarios, such that if a handoff occurs
during PUTs in a write-once bucket, the values that have been written will be
handed off to the newly added Riak node.

## Asynchronous Writes

For backends that support asynchronous writes, the write-once path will
dispatch a write request to the backend and handle the response
asynchronously. This behavior allows the vnode to free itself for other work
instead of waiting on the write response from the backend.

At the time of writing, the only backend that supports asynchronous writes is
LevelDB. Riak will automatically fall back to synchronous writes with all other
backends.

{{% note title="Note on the `multi` backend" %}}
The [Multi]({{<baseurl>}}riak/kv/2.9.7/setup/planning/backend/multi) backend does not
support asynchronous writes. Therefore, if LevelDB is used with the Multi
backend, it will be used in synchronous mode.
{{% /note %}}




