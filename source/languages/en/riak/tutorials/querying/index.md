---
title: Querying Riak
project: riak
version: 0.10.0+
document: tutorials
toc: true
audience: beginner
keywords: [mapreduce, search, indexes, comparison]
next: "[[Basic Operations]]"
body_id: query-riak
---

* [[Basic Operations]]
* [[MapReduce Overview]]
* [[Secondary Indexes]]
* [[Searching|Riak Search]]

## Comparing MapReduce, RiakSearch, and Secondary Indexes

&nbsp; | MapReduce | Riak Search | Secondary Indexes
-------|----------|-------------|------------------
*Query Types* | Ad-hoc queries composed of an arbitrary number of Map phases and Reduce phases | SOLR style queries supporting combinations of free-text, wildcards, proximity, and boolean operators | Equality and range query support
*Index Locality* | N/A | Indexes for terms are replicated to N vnodes (i.e. term-based partitioning) and stored in a merge index backend, regardless of the Riak KV backend used | Indexes are located on the same vnodes as the object (i.e. document-based partitioning) and stored in the LevelDB backend along with the document
*Vnodes Queried* | Depends on input 1 per term queried; 1/N for trailing wildcard | 1/N of all KV vnodes per a request
*Supported Data Types* | Any datatype with Erlang MapReduce functions; valid UTF8 JSON with Javascript functions. [[Links]] per the specification | Integer, Date, and Text | Binary and Integer
*Extraction* | Map phases can be used to extract data for later Map and Reduce phases | Tokenization performed by one of the provided analyzers (Whitespace, Standard, Integer, and No-Op) or a Custom Analyzer | Indexed values are submitted as metadata on the object, thus the application is responsible for tokenization
*Anti-Entropy / Fault Tolerance* | N/A | If a Search partition is lost or corrupted, the "repair" feature in Riak Console will rebuild partition data from adjacent indexes. | Anti-entropy is carried over from KV; if a partition is lost, secondary indexes will be rebuilt along side the KV data by read repair
*Limitations* | MapReduce operations are performed in memory and must be completed within the timeout period | Querying low cardinality terms could tax a small subset of vnodes; composite queries are expensive; documents must be structured (JSON or XML) or plain text | Only available with the LevelDB backend; no composite queries
*Suggested Use Cases* | Performing calculations based on a known set of bucket-key pairs | Searching objects with full-text data | Retrieving all objects tagged with a particular term
*Poor Use Cases* | Performing complex operations on large numbers of objects (e.g. analyzing every object in a bucket) | Searching for common (low cardinality) terms in documents | Searching prosaic text
