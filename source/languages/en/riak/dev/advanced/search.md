---
title: Advanced Search
project: riak
version: 1.0.0+
document: guide
toc: true
audience: advanced
keywords: [developers, search, kv]
moved: {
  '1.4.0-': '/tutorials/querying/Riak-Search'
}
---

## How Search Works

### Architecture, Partitioning and Indexing

When Search is enabled on your Riak cluster, another set of vnodes, equal to the number of KV vnodes, will be started which will be used to handle Search requests. Search is enabled on a per-bucket basis by setting the search property to true. If search is enabled for a bucket, indexes are generated on all objects as they are written to it using the pre-commit hook. Index data stored on Search vnodes is replicated across the cluster using the same general mechanism as Riak KV, but using timestamps rather than vector clocks to increase performance.

![Enabling a Node for Search](/images/riak_search_enabling_physical_node.png)

Search uses term-based partitioning – also known as a global index. For example, let’s say we have a set of 5 documents with a total of 3 unique words (the set union). In this example, the term “dog” can be found in documents 1 and 2 – or in Riak’s case, keys 1 & 2. The term “and” can be found in documents 1, 2, 3,and 4, etc.

![Search Document Table](/images/riak_search_document_table.png)

At index time, Riak Search analyzes a document and stores postings in the index. The system consults a schema (defined per-index) to determine required fields, the unique key, the default analyzer, and which analyzer should be used for each field. Custom analyzers can be created in either Java or Erlang. Field aliases (grouping multiple fields into one field) and dynamic fields (wildcard field matching) are supported.

After analyzing a document into an index, the system uses a consistent hash to partition the index entries (called postings) by term across the cluster. This is called term-based partitioning and is a key difference from other commonly used distributed indexes. Term-based partitioning was chosen because it can provide higher query throughput and does currently outperform 2i in that regard. (This can come at the expense of higher-latency queries for especially large result sets.)

### Persistence

For a backing store, the Riak Search team developed merge\_index. The merge\_index store takes inspiration from the Lucene file format, Bitcask (our standard backing store for Riak KV), and SSTables (from Google's BigTable paper), and was designed to have a simple, easily-recoverable data structure, to allow simultaneous reads and writes with no performance degradation, and to be forgiving of write bursts while taking advantage of low-write periods to perform data compactions and optimizations.

### Replication

A search index has an `n_val` setting that determines how many copies of the data exist. Copies are written across different partitions located on different physical nodes.

The underlying data for Riak Search lives in Riak KV and replicates in precisely the same manner. However, the Search index, created from the underlying data, replicates differently for technical reasons.

* Riak Search uses timestamps, rather than vector clocks, to resolve version conflicts. This leads to fewer guarantees about your data (as depending on wall-clock time can cause problems if the clock is wrong) but was a necessary tradeoff for performance reasons.
* Riak Search does not use quorum values when writing (indexing) data. The data is written in a fire and forget model. Riak Search *does* use hinted-handoff to remain write-available when a node goes offline.
* Riak Search does not use quorum values when reading (querying) data. Only one copy of the data is read, and the partition is chosen based on what will create the most efficient query plan overall.

## Major Components

Riak Search is comprised of:

* *Riak Core* -  Dynamo-inspired distributed-systems framework
* *Riak KV* - Distributed Key/Value store inspired by Amazon's Dynamo.
  * *Bitcask* -  Default storage backend used by Riak KV.
* *Riak Search* - Distributed index and full-text search engine.
  * *Merge Index* - Storage backend used by Riak Search. This is a pure Erlang storage format based roughly on ideas borrowed from other storage formats including log structured merge trees, sstables, bitcask, and the Lucene file format.
  * *Riak Solr* - Adds a subset of Solr HTTP interface capabilities to Riak Search.

## Query Scoring

Documents are scored using roughly [these formulas](http://lucene.apache.org/core/old_versioned_docs/versions/3_0_2/api/all/org/apache/lucene/search/Similarity.html)

The key difference is in how Riak Search calculates the Inverse Document Frequency. The equations described on the /Similarity/ page require knowledge of the total number of documents in a collection. Riak Search does not maintain this information for a collection, so instead uses the count of the total number of documents associated with each term in the query.


## Stop Words

Riak Search implements Stop Words, much as you might find in Solr itself:

[[http://wiki.apache.org/solr/AnalyzersTokenizersTokenFilters#solr.StopFilterFactory]]

The source code for Riak Search's "default analyzer factory" can be found here:

[[http://github.com/basho/riak_search/blob/master/src/text_analyzers.erl]]

In short, the following words will be skipped when indexing. The official list is maintained in the source file, linked above:


```erlang
is_stopword(Term) when length(Term) == 2 ->
    ordsets:is_element(Term, ["an", "as", "at", "be", "by", "if", "in", "is", "it", "no", "of", "on", "or", "to"]);
is_stopword(Term) when length(Term) == 3 ->
    ordsets:is_element(Term, ["and", "are", "but", "for", "not", "the", "was"]);
is_stopword(Term) when length(Term) == 4 ->
    ordsets:is_element(Term, ["into", "such", "that", "then", "they", "this", "will"]);
is_stopword(Term) when length(Term) == 5 ->
    ordsets:is_element(Term, ["their", "there", "these"]);
```

If you plan to use these words in an 'exact phrase' query, you may want to use another analyzer factory.  Be aware, however, that these words can quickly clog up your index.  Future analyzers can fill the void, while maintaining some degree of efficiency.

As an example of how this might affect your queries: If you search using the exact phrase:

```bash
?q=\"the dog is\"
```

With the aforementioned stop words enabled, your query is reduced to "dog".  Perhaps not what you had in mind.

## Indexing Search

Indexing a document is the act of:

1. Reading a document.
2. Splitting the document into one or more fields.
3. Splitting the fields into one or more terms.
4. Normalizing the terms in each field.
5. Writing the {Field, Term, DocumentID} postings to an index.

You can find a detailed list of indexing commands in the [[Search Indexing Reference]].

## Search, KV and MapReduce

Riak Search supports indexing and querying of data stored in Riak KV.  Out of the box, simple indexing of plain text, XML, and JSON data can be enabled in an instant.

<div class="info">
<div class="title">Riak Search and MapReduce</div>
Riak Search isn't just able to index data from Riak KV, it can also be used to [[feed data into Riak's MapReduce|Advanced Search#Search, KV and MapReduce]].
</div>

### Setting up Indexing

Riak Search indexing of KV data must be enabled on a per-KV-bucket basis.  To enable indexing for a bucket, simply add the Search precommit hook to that bucket's properties.

Adding the Search precommit hook to a bucket from the command line is easy:

```bash
bin/search-cmd install my_bucket_name
```

Any other method you would normally use to set bucket properties can also be used to enable the Search precommit hook as well.  For example, using curl to install over HTTP:

```bash
curl -XPUT -H "content-type:application/json" http://localhost:8098/riak/demo2 -d @- << EOF
{"props":{"precommit":[{"mod":"riak_search_kv_hook","fun":"precommit"}]}}
EOF
```

Note, though, that you may want to read the bucket properties first, so you don't clobber any precommit hook already in place.

With the precommit hook installed, Riak Search will index your data each time that data is written.

### Datatypes

Riak Search is able to handle several standard data encodings with zero configuration.  Simply set the Content-Type metadata on your objects to the appropriate mime-type.  Out of the box, XML, JSON, and plain-text encodings are supported.

#### JSON Encoded Data

If your data is in JSON format, set your Content-Type to "application/json", "application/x-javascript", "text/javascript", "text/x-javascript", or "text/x-json".

Specifying that your data is in JSON format will cause Riak Search to use the field names of the JSON object as index field names.  Nested objects will use underscore ('_') as a field name separator. (The underscore was chosen because the character is not currently reserved by Lucene syntax. People have suggested using a period, but we have that reserved for other things.)

For example, storing the following JSON object in a Search-enabled bucket:

```javascript
{
 "name":"Alyssa P. Hacker",
 "bio":"I'm an engineer, making awesome things.",
 "favorites":{
              "book":"The Moon is a Harsh Mistress",
              "album":"Magical Mystery Tour"
             }
}
```

Would cause four fields to be indexed: "name", "bio", "favorites_book", and "favorites_album".  You could later query this data with queries like, "bio:engineer AND favorites_album:mystery".

#### XML Encoded Data

If your data is in XML format, set your Content-Type to "application/xml" or "text/xml".

Specifying that your data is in plain-text format will cause Riak Search to use tag names as index field names.  Nested tags separate their names with underscores.  Attributes are stored in their own fields, the names of which are created by appending an at symbol ('@') and the attribute name to the tag name.

For example, storing the following XML object in a Search-enabled bucket:

```xml
<?xml version="1.0"?>
<person>
   <name>Alyssa P. Hacker</name>
   <bio>I'm an engineer, making awesome things.</bio>
   <favorites>
      <item type="book">The Moon is a Harsh Mistress</item>
      <item type="album">Magical Mystery Tour</item>
   </favorites>
</person>
```

Would cause four fields to be indexed: "person_name", "person_bio", "person_favorites_item", and "person_favorite_item@type".  The values of the "..._item" and "..._item@type" fields will be the concatenation of the two distinct elements in the object ("The Moon is a Harsh Mistress Magical Mystery Tour" and "book album", respectively).  You could later query this data with queries like, "person_bio:engineer AND person_favorites_item:mystery".

#### Erlang Data

If your object contains Erlang terms, you can set your Content-Type to "application/x-erlang". This expects either an Erlang term that is a proplist or a nested proplist. In the case of a proplist, the key is used as the field name, and the value as the field value. When the object's value is a nested proplist, field names are constructed by concatenating the nested keys together with underscores in between.

#### Plain-text Data

If your data is plain text, set your Content-Type to "text/plain". The plain-text decoder is also used if no Content-Type is found.

Specifying that your data is in plain-text format will cause Riak Search to index all of the text in the object's value under a single field, named "value".  Queries can be explicit about searching this field, as in "value:seven AND value:score", or omit the default field name, as in "seven AND score".

#### Other Data Encodings

If your data is not in JSON, XML, or plain-text, or you would like field name or value extraction to behave differently, you may also write your own extractor.

To set the extractor via HTTP

```bash
curl -XPUT -H 'content-type: application/json' \
    http://host:port/riak/bucket \
    -d '{"props":{"search_extractor":{"mod":"my_extractor", "fun":"extract", "arg":"my_arg"}}}'
```

An extractor should export a function `extract` which takes two arguments.  The first is the Riak object to index.  The second is the static argument specified in the `search_extractor` property value or `undefined` if none is given.  The function should return a list of 2-tuples which represent field-name/value pairs.  Both the field-name and value should be of type binary.

```erlang
[
 {<<"field1">>,<<"value1">>},
 {<<"field2">>,<<"value2">>}
]
```

The modules `riak_search_kv_json_extractor`, `riak_search_kv_xml_extractor`, and `riak_search_kv_raw_extractor` should be referred to for examples.

#### Field Types

If you read the "Other Data Encodings" section about writing your own encoder, you may have been surprised to find that all fields should be extracted as strings.  The reason for this is that it's the schema's job to say what the types of the fields are.

If you do not specify a schema, the default will be used.  The default schema indexes all fields as string values, unless they end in "_num" or "_dt", or any of the other dynamic fields defined in the Schema documentation.

You may define your own schema for your KV indexes, in the same manner as you would define a schema for non-KV indexes.  Just make sure the field names match those produced by the extractor in use.
