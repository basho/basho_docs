---
title: Secondary Indexes
project: riak
version: 0.14.0+
document: tutorials
toc: true
audience: beginner
keywords: [querying, 2i]
prev: "[[MapReduce]]"
up:   "[[Querying Riak]]"
next: "[[Searching|Riak Search]]"
interest: [
"[[2i Configuration|Secondary Indexes - Configuration]]",
"[[2i Examples|Secondary Indexes - Examples]]"
]
---

## Introduction
Secondary Indexing (2i) in Riak gives developers the ability, at write time, to tag an object stored in Riak with one or more queryable values.

Since the KV data is completely opaque to 2i, the user must tell 2i exactly what attribute to index on and what its index value should be, via key/value metadata. This is different from Search, which parses the data and builds indexes based on a schema. Riak 2i currently requires the LevelDB backend.

## Features

* Allows two types of secondary attributes: integers and strings (aka binary).
* Allows querying by exact match or range on one index.
* Query results can be used as input to a MapReduce query.

## When to Use Secondary Indexes

* When you want to find data based on terms other than an objects’ bucket/key pair. With 2i, indexes can be created by adding metadata to Riak objects.
* When the value being stored is an opaque blob, like a binary file, and you want to index it via added attributes.
* When you want or need an easy-to-use search mechanism. Secondary indexing does not require a schema (as in Search) and comes with a basic query interface.
* When you want or need anti-entropy. Since 2i is just metadata on the KV object and the indexes reside on the same node, 2i piggybacks off of read-repair.

## When Not to Use Secondary Indexes 

* If your ring size exceeds 512 partitions: 2i can cause performance issues in large clusters. (See "How it Works").
* When you need more than the exact match and range searches that 2i supports.
* When you want to use composite queries. A query like "last_name=zezeski AND state=MD" would have to be split into two queries and the results merged (or involve MapReduce).
* When pagination is a requirement. 2i has no support for pagination (all results are always returned). This can be handled to some degree through the use of MapReduce, but adds complexity.
* When totally ordered result sets are a requirement. Result sets in 2i are only partially ordered. Total ordering must be handled by the client or via MapReduce.

## How It Works 

Secondary Indexes use document-based partitioning — also known as a local index, wherein the indexes reside with each document, local to the vnode. Secondary indexes are a list of key/value pairs that are similar to http headers. At write time, objects are tagged with index entries consisting of key/value metadata. This metadata can be queried to get the matching keys. 

![Secondary Index](/images/Secondary-index-example.png)

Indexes reside across multiple machines. A covering a set of vnodes must be queried and the results merged. Since indexes for an object are stored on the same partition as the object itself, query-time performance issues might occur. When issuing a query, the system must read from a "covering" set of partitions. The system looks at how many replicas of data are stored (n value) and determines the minimum number of partitions that it must examine (1/n) to retrieve a full set of results, also taking into account any offline nodes.

An application can modify the indexes for an object by reading an object, adding or removing index entries, and then writing the object. Finally, an object is automatically removed from all indexes when it is deleted. The object's value and its indexes should be thought of as a single unit. There is no way to alter the indexes of an object independently from the value of an object, and vice versa. Indexing is atomic, and is updated in real-time when writing an object. This means that an object will be present in future index queries as soon as the write operation completes.
 
Riak stores 3 replicas of all objects by default. The system is capable of generating a full set of results from one-third of the system’s partitions as long as it chooses the right set of partitions. The query is sent to each partition, the index data is read, and a list of keys is generated and then sent back to the requesting node.

## Query Interfaces and Examples

In this example, a bucket/key pair of “users/john_smith” is used to store user data. The user would like to add a twitter handle and email address as secondary indexes:

### Inserting the object with Secondary Indexes:

```bash
curl -X POST \
-H 'x-riak-index-twitter_bin: jsmith123' \
-H 'x-riak-index-email_bin: jsmith@basho.com' \
-d '...user data...' \ 
http://localhost:8098/buckets/users/keys/john_smith
```

* The object has been stored with a primary bucket/key of: users/john_smith
* The object now has a secondary index called “twitter_bin” with a value of: jsmith123
* The object now has a secondary index called “email_bin” with a value of: jsmith@basho.com

### Querying the object with Secondary Indexes:

Query the twitter handle...

```bash
curl localhost:8098/buckets/users/index/twitter_bin/jsmith123
```

Response... 

```text
{"keys":["john_smith"]}
```
