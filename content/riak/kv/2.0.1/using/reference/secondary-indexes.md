---
title: "Secondary Indexes Reference"
description: ""
project: "riak_kv"
project_version: "2.0.1"
menu:
  riak_kv-2.0.1:
    name: "Secondary Indexes"
    identifier: "managing_ref_2i"
    weight: 110
    parent: "managing_ref"
toc: true
aliases:
  - /riak/2.0.1/dev/advanced/2i
  - /riak/kv/2.0.1/dev/advanced/2i
---

[usage bucket types]: {{<baseurl>}}riak/kv/2.0.1/developing/usage/bucket-types
[use ref strong consistency]: {{<baseurl>}}riak//2.0.1/using/reference/strong-consistency

> **Note: Riak Search preferred for querying**
>
> If you're interested in non-primary-key-based querying in Riak, i.e. if
you're looking to go beyond straightforward K/V operations, we now
recommend [Riak Search]({{<baseurl>}}riak/kv/2.0.1/developing/usage/search/) rather than secondary indexes for a variety of reasons. Riak Search has a far more capacious querying API and can be used with all of Riak's storage backends.

This document provides implementation and other details for Riak's
[secondary indexes]({{<baseurl>}}riak/kv/2.0.1/developing/usage/secondary-indexes/) \(2i) feature.

## How It Works

Secondary indexes use **document-based partitioning**, a system where
indexes reside with each document, local to the [vnode]({{<baseurl>}}riak/kv/2.0.1/learn/glossary/#vnode). This
system is also a local index. Secondary indexes are a list of key/value
pairs that are similar to HTTP headers. At write time, objects are
tagged with index entries consisting of key/value metadata. This
metadata can be queried to retrieve the matching keys.

![Secondary Index]({{<baseurl>}}images/Secondary-index-example.png)

Indexes reside on multiple machines. Since indexes for an object are
stored on the same partition as the object itself, query-time
performance issues might arise. When issuing a query, the system must
read from a "covering" set of partitions and then merge the results.
The system looks at how many replicas of data are stored---the N value
or `n_val`---and determines the minimum number of partitions that it
must examine (1 / `n_val`) to retrieve a full set of results, also
taking into account any offline nodes.

An application can modify the indexes for an object by reading an
object, adding or removing index entries, and then writing the object.
Finally, an object is automatically removed from all indexes when it is
deleted. The object's value and its indexes should be thought of as a
single unit. There is no way to alter the indexes of an object
independently from the value of an object, and vice versa. Indexing is
atomic, and is updated in real time when writing an object. This means
that an object will be present in future index queries as soon as the
write operation completes.

Riak stores 3 replicas of all objects by default, although this can be
changed [using bucket types][usage bucket types], which manage buckets' [replication properties]({{<baseurl>}}riak/kv/2.0.1/developing/app-guide/replication-properties). The system is capable of generating a full set of results
from one third of the system’s partitions as long as it chooses the
right set of partitions. The query is sent to each partition, the index
data is read, and a list of keys is generated and then sent back to the
requesting node.

> **Note on 2i and strong consistency**
>
> Secondary indexes do not currently work with the [strong consistency][use ref strong consistency] feature introduced in Riak version 2.0. If you store objects in [strongly consistent buckets]({{<baseurl>}}riak/kv/2.0.1/developing/app-guide/strong-consistency/#creating-a-strongly-consistent-bucket-type) and attach
secondary index metadata to those objects, you can still perform
strongly consistent operations on those objects but the secondary
indexes will be ignored.
