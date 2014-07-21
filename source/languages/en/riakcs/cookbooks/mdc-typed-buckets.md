---
title: Multi-Datacenter Version 3 Replication with Bucket Types
project: riakcs
header: riakee
version: 2.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, repl, configuration, bucket-types, typed-buckets]
---

While Riak Enterprise supports realtime and fullsync replication of typed buckets, there are a few things that need to be verified before replication occurs.

## Comparison Scenarios

Before transmitting objects, source and sink buckets are compared. Upon comparison, one of the following sequences of events will occur:

1. If source and sink buckets aren't using typed buckets, objects _are_ replicated.
2. If source and sink buckets are using typed buckets that aren't activated on either side, objects are _not_ replicated.
3. If source and sink buckets are using different bucket type names, objects are _neither compared nor replicated_.
4. If source and sink buckets are using the same bucket type name but the source or sink bucket type has not yet been activated, objects are _not_ replicated and a warning is appended to the logs.
5. If source and sink buckets are using the same bucket type name, the following properties must be identical on either side for replication to occur, otherwise a warning is appended to the logs:

* `n_val`
* `allow_mult`
* `lww`
* `consistent`
* `datatype`
  
**Note**: Changes to typed buckets on the source and/or sink cluster will be verified upon each fullsync or realtime replication object transfer.

## Error Log Messages

The following error message may be displayed during fullsync replication if typed bucket properties do not match:
<!-- TODO: THE FOLLOWING MAY CHANGE BEFORE THE 2.0 RELEASE -->

```log
Remote and local bucket properties differ for type <type_name>
```

The following error message may be displayed during realtime replication if typed bucket properties do not match:
<!-- TODO: THE FOLLOWING MAY CHANGE BEFORE THE 2.0 RELEASE -->

```log
Bucket is of a type that is not equal on both the source and sink; not writing object.
```

