---
title: Searching
project: riak
version: 0.14.0+
document: tutorials
toc: true
audience: beginner
keywords: [querying, search]
prev: "[[Secondary Indexes]]"
up:   "[[Querying Riak]]"
interest: [
"[[Search Schema|Riak Search - Schema]]",
"[[Search Indexing|Riak Search - Indexing]]",
"[[Querying Search|Riak Search - Querying]]",
"[[KV Data Search|Riak Search - Indexing and Querying Riak KV Data]]",
"[[Search Settings|Riak Search - Settings]]"
]
---

## Introduction

Riak Search is a distributed, full-text search engine that is built on Riak Core and included as part of Riak open source. Search provides the most advanced query capability next to MapReduce, but is far more concise; easier to use, and in most cases puts far less load on the cluster.

Search indexes Riak KV objects as they're written using a precommit hook. Based on the object’s mime type and the Search schema you’ve set for its bucket, the hook will automatically extract and analyze your data and build indexes from it. The Riak Client API is used to invoke Search queries that return a list of bucket/key pairs matching the query. Currently the PHP, Python, Ruby, and Erlang client libraries support integration with Riak Search.

## Features

* Support for various mime types (JSON, XML, plain text, Erlang, Erlang binaries) for automatic data extraction
* Support for various analyzers (to break text into tokens) including a white space analyzer, an integer analyzer, and a no-op analyzer
* Robust, easy-to-use query language
* Exact match queries
  * Wildcards
  * Inclusive/exclusive range queries o AND/OR/NOT support
  * Grouping
  * Prefix matching
  * Proximity searches
  * Term boosting
* Solr-like interface via HTTP (not [[Solr|http://lucene.apache.org/solr]] compatible)
* Protocol buffers interface
* Scoring and ranking for most relevant results
* Search queries as input for MapReduce jobs

## When to Use Search 

* When collecting, parsing, and storing data like user bios, blog posts, journal articles, etc. to facilitate fast and accurate information retrieval in addition to some reasonable form of ranking.
* When indexing JSON data. Extractors pull the data apart depending on mime type, analyzers tokenize the fields, and the schema describes what fields to index, how to analyze them and how to store them.
* When fast information retrieval with a powerful query language is needed.

## When Not to Use Search 

* When only simple tagging of data is needed with exact match and range queries. In this case [[Secondary Indexes|Secondary-Indexes]] would be easier.
* When the data is not easily analyzed by Search. For example mp3, video, or some other binary format. In this case a Secondary Index interface is recommended.
* When built-in anti-entropy/consistency is required. At this time Search has no read-repair mechanism. If Search index data is lost, the entire data set must be re-indexed.

## Query Interfaces and Examples

_Querying via the Command Line_

*   bin/search-cmd search books "title:\"See spot run\""
This will display a list of Document ID values matching the query.

_Querying via the Solr-like Interface_

*  curl http://localhost:8098/solr/customers/select?q=first_name:john
This will search the “customers” bucket for objects that had the text “john” in their “first_name”

## How It Works 


### Architecture, Partitioning and Indexing

When Search is enabled on your Riak cluster, another set of vnodes, equal to the number of KV vnodes, will be started which will be used to handle Search requests. Search is enabled on a per-bucket basis by setting the search property to true. If search is enabled for a bucket, indexes are generated on all objects as they are written to it using the pre-commit hook. Index data stored on Search vnodes is replicated across the cluster using the same general mechanism as Riak KV, but using timestamps rather than vector clocks to increase performance.

![Enabling a Node for Search](/images/riak_search_enabling_physical_node.png)

Search uses term-based partitioning – also known as a global index. For example, let’s say we have a set of 5 documents with a total of 3 unique words (the set union). In this example, the term “dog” can be found in documents 1 and 2 – or in Riak’s case, keys 1 & 2. The term “and” can be found in documents 1, 2, 3,and 4, etc.

![Search Document Table](/images/riak_search_document_table.png)

At index time, Riak Search analyzes a document and stores postings in the index. The system consults a schema (defined per-index) to determine required fields, the unique key, the default analyzer, and which analyzer should be used for each field. Custom analyzers can be created in either Java or Erlang. Field aliases (grouping multiple fields into one field) and dynamic fields (wildcard field matching) are supported.

After analyzing a document into an index, the system uses a consistent hash to partition the index entries (called postings) by term across the cluster. This is called term-based partitioning and is a key difference from other commonly used distributed indexes. Term-based partitioning was chosen because it can provide higher query throughput and does currently outperform 2i in that regard. (This can come at the expense of higher-latency queries for especially large result sets.)

### Persistence

For a backing store, the Riak Search team developed merge\_index. The merge\_index store takes inspiration from the Lucene file format, Bitcask (our standard backing store for Riak KV), and SSTables (from Google's BigTable paper), and was designed to have a simple, easily-recoverable data structure, to allow simultaneous reads and writes with no performance degredation, and to be forgiving of write bursts while taking advantage of low-write periods to perform data compactions and optimizations.

### Replication

A search index has an `n_val` setting that determines how many copies of the data exist. Copies are written across different partitions located on different physical nodes.

The underlying data for Riak Search lives in Riak KV and replicates in precisely the same manner. However, the Search index, created from the underlying data, replicates differently for technical reasons.

* Riak Search uses timestamps, rather than vector clocks, to resolve version conflicts. This leads to fewer guarantees about your data (as depending on wall-clock time can cause problems if the clock is wrong) but was a necessary tradeoff for performance reasons.
* Riak Search does not use quorum values when writing (indexing) data. The data is written in a fire and forget model. Riak Search *does* use hinted-handoff to remain write-available when a node goes offline.
* Riak Search does not use quorum values when reading (querying) data. Only one copy of the data is read, and the partition is chosen based on what will create the most efficient query plan overall.

## Operations 

Operationally, Riak Search is very similar to Riak KV. An administrator can add nodes to a cluster on the fly with simple commands to increase performance or capacity. Index and query operations can be run from any node. Multiple replicas of data are stored, allowing the cluster to continue serving full results in the face of machine failure. Partitions are handed off and replicated across clusters using the same mechanisms as Riak KV.

Riak Search is a superset of Riak KV, so if you are running Riak Search, then you are automatically running a Riak KV cluster. You don't need to set up a separate Riak KV cluster to use Riak Search.

## Major Components

Riak Search is comprised of:

* *Riak Core* -  Dynamo-inspired distributed-systems framework
* *Riak KV* - Distributed Key/Value store inspired by Amazon's Dynamo.
  * *Bitcask* -  Default storage backend used by Riak KV.
* *Riak Search* - Distributed index and full-text search engine.
  * *Merge Index* - Storage backend used by Riak Search. This is a pure Erlang storage format based roughly on ideas borrowed from other storage formats including log structured merge trees, sstables, bitcask, and the Lucene file format.
  * *Riak Solr* - Adds a subset of Solr HTTP interface capabilities to Riak Search.
