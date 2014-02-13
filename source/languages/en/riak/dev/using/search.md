---
title: Using Search
project: riak
version: 1.0.0+
document: tutorials
toc: true
audience: beginner
keywords: [developers, search, kv]
---

{{#2.0.0+}}
<div class="info">This document refers to the new Riak Search 2.0 with [[Solr|http://lucene.apache.org/solr/]] integration (codenamed Yokozuna). For information about the <em>deprecated</em> Riak Search, visit [[Using Legacy Search]].</div>

You must first [[enable Riak Search|Riak Search Settings]] in your environment to use it.

## Introduction

Riak Search 2.0 is a new open-source project integrated with Riak. It allows for distributed, scalable, fault tolerant, transparent indexing and querying of Riak values. It's easy to use. After connecting a bucket (or bucket type) to a Solr index, you simply write values (such as JSON, XML, plain text, datatypes, etc.) into Riak as normal, and then query those indexed values using the Solr API.

### Why Riak Search

Some of Riak's core strengths lie in its scalability, fault tolerance, and ease of operations. However, Riak models its data in key-value objects, and a key-value store is something that few would claim is easy to query. This is the driving force behind Riak Search, to provide an integrated, scalable mechanism to build ad hoc queries against values stored in a Riak cluster, while holding true to Riak's core strengths.

![Yokozuna](/images/yokozuna.png)

## Simple Setup

Riak Search 2.0 is an integration of Solr (for indexing and querying) and Riak (for storage and distribution). There are a few points of interest that a user of Riak Search will have to keep in mind in order to properly store and later query for values.

1. **Schemas** explain to Solr how to index fields
2. **Indexes** are named Solr indexes which you will query against
3. **Bucket-index association** signals to Riak *when* to index values

Riak Search must first be configured with a Solr *schema*, so Solr knows how to index value fields. If you don't define one, you're provided with a default schema named `_yz_default`. The examples in this document will presume the default. You can read more about creating a custom schema in [[Advanced Search Schema|Advanced Search#Schemas]], which you'll want to do in a production environment.

Next, you must create a named Solr index through Riak Search. This index represents a collection of similar data that you connect with to perform queries. When creating an index, you can optionally provide a schema. If you do not, the default schema will be used. Here we'll `curl` create an index named `famous` with the default schema.

<div class="info">All `curl` examples in this document require that you first
set an environment variable named `RIAK_HOST`, which points to a Riak base
URL, such as `RIAK_HOST="http://localhost:8098"`.</div>

```curl
export RIAK_HOST="http://localhost:8098"

curl -XPUT "$RIAK_HOST/search/index/famous"
```
```ruby
client.create_search_index("famous")
```
```python
client.create_search_index('famous')
```
```erlang
riakc_pb_socket:create_search_index(Pid, <<"famous">>)
```

Note that the above command is exactly the same as the following, which explicitly defines the default schema.

```curl
curl -XPUT "$RIAK_HOST/search/index/famous" \
     -H'content-type:application/json' \
     -d'{"schema":"_yz_default"}'
```
```ruby
client.create_search_index("famous", "_yz_default")
```
```python
client.create_search_index('famous', '_yz_default')
```
```erlang
riakc_pb_socket:create_search_index(Pid, <<"famous">>, <<"_yz_default">>, [])
```

The last setup item you need to perform is to *associate a bucket* (or bucket type) with a Solr index. You only need do this once per bucket (or bucket type). This setting tells Riak Search which bucket's values are to be indexed on a write to Riak. For example, to associate a bucket named `cats` with the `famous` index, you can set the bucket property `search_index`.


```curl
curl -XPUT "$RIAK_HOST/buckets/cats/props" \
     -H'content-type:application/json' \
     -d'{"props":{"search_index":"famous"}}'
```
```ruby
bucket = Riak::Bucket.new(client, "cats")
bucket.props = {"search_index" => "famous"}
```
```python
bucket = client.bucket('cats')
bucket.set_property('search_index', 'famous')
```
```erlang
ok = riakc_pb_socket:set_search_index(Pid, <<"cats">>, <<"famous">>)
```

With your schema, Solr index, and association all set up, you're ready to start using Riak Search.

### Bucket Types

In order to lower the overhead of using Riak Search in a cluster within the
default bucket namespace, [[Bucket Types|Using Bucket Types]] should be used.
Below are the few additional steps required to associate a search index with a
bucket type:

```bash
$ riak-admin bucket-type create animals '{"props":{}}'
$ riak-admin bucket-type activate animals
```

Then you can *associate the bucket* with an index as you did before, but this time requiring a bucket type as well as a bucket name (in this case, the type is *animals*).

```curl
curl -XPUT "$RIAK_HOST/types/animals/buckets/cats/props" \
     -H'content-type:application/json' \
     -d'{"props":{"search_index":"famous"}}'
```

Creating and using indexes is the same we've seen thusfar.

### Security

Security is a new feature of Riak that lets an administrator limit access to certain resources. In the case of search, your options are to limit administration of schemas or indexes (permission `search.admin`) to certain users, and to limit querying (permission `search.query`) to any index or a specific one. The example below shows the various options.

```bash
riak-admin security grant search.admin ON schema TO username
riak-admin security grant search.admin ON index TO username
riak-admin security grant search.query ON index TO username
riak-admin security grant search.query ON index famous TO username
```

## Indexing Values

With a Solr schema, index, and association in place, we're ready to start using Riak Search. First, populate the `cat` bucket with values.

Depending on the driver you use, you may have to specify the `Content-Type`,
which for this example is `application/json`. In the case of Ruby and Python
the content type is automatically set for you based on the object given.

```curl
curl -XPUT "$RIAK_HOST/buckets/cats/keys/liono" \
     -H'content-type:application/json' \
     -d'{"name_s":"Liono", "age_i":30, "leader_b":true}'

curl -XPUT "$RIAK_HOST/buckets/cats/keys/cheetara" \
     -H'content-type:application/json' \
     -d'{"name_s":"Cheetara", "age_i":28, "leader_b":false}'

curl -XPUT "$RIAK_HOST/buckets/cats/keys/snarf" \
     -H'content-type:application/json' \
     -d'{"name_s":"Snarf", "age_i":43}'

curl -XPUT "$RIAK_HOST/buckets/cats/keys/panthro" \
     -H'content-type:application/json' \
     -d'{"name_s":"Panthro", "age_i":36}'
```
```ruby
bucket = client.bucket("cats")

cat = bucket.get_or_new("liono")
cat.data = {"name_s" => "Liono", "age_i" => 30, "leader_b" => true}
cat.store

cat = bucket.get_or_new("cheetara")
cat.data = {"name_s" => "Cheetara", "age_i" => 28, "leader_b" => false}
cat.store

cat = bucket.get_or_new("snarf")
cat.data = {"name_s" => "Snarf", "age_i" => 43}
cat.store

cat = bucket.get_or_new("panthro")
cat.data = {"name_s" => "Panthro", "age_i" => 36}
cat.store
```
```python
bucket = client.bucket('cats')

cat = bucket.new('liono', {'name_s': 'Liono', 'age_i': 30, 'leader_b': True})
cat.store()

cat = bucket.new('cheetara', {'name_s':'Cheetara', 'age_i':28, 'leader_b': True})
cat.store()

cat = bucket.new('snarf', {'name_s':'Snarf', 'age_i':43})
cat.store()

cat = bucket.new('panthro', {'name_s':'Panthro', 'age_i':36})
cat.store()
```
```erlang
CO = riakc_obj:new(<<"cats">>, <<"liono">>,
    <<"{\"name_s\":\"Liono\", \"age_i\":30, \"leader_b\":true}">>,
    "application/json"),
riakc_pb_socket:put(Pid, CO),

C1 = riakc_obj:new(<<"cats">>, <<"cheetara">>,
    <<"{\"name_s\":\"Cheetara\", \"age_i\":28, \"leader_b\":false}">>,
    "application/json"),
riakc_pb_socket:put(Pid, C1),

C2 = riakc_obj:new(<<"cats">>, <<"snarf">>,
    <<"{\"name_s\":\"Snarf\", \"age_i\":43}">>,
    "application/json"),
riakc_pb_socket:put(Pid, C2),

C3 = riakc_obj:new(<<"cats">>, <<"panthro">>,
    <<"{\"name_s\":\"Panthro\", \"age_i\":36}">>,
    "application/json"),
riakc_pb_socket:put(Pid, C3),
```

If you've used Riak before, you may have noticed that this is no different
than setting values without Riak Search. This is one of the design goals of
Riak Search:

> Write it like Riak. Query it like Solr.

But how does Riak Search know how to index values, given that values are
opaque in Riak? For that, we employ extractors.

### Extractors

*Extractors* are modules in Riak that accept a Riak value with a certain content type, and convert it into a list of fields capable of being indexed by Solr. This is done transparently and automatically as part of the indexing process.

Our current example uses the JSON extractor, but Riak Search also extracts indexable fields from the following content types:

* JSON (`application/json`)
* XML (`application/xml`, `text/xml`)
* Plain text (`text/plain`)
* Datatypes
  * counter (`application/riak_counter`)
  * map (`application/riak_map`)
  * set (`application/riak_set`)
* noop (unknown content type)

In the examples we've seen, the JSON field `name_s` is translated to a Solr index document field insert. Solr will index any field that it recognizes, based on the index's schema. The default schema (`_yz_default`) uses the suffix to decide the field type (`_s` represents a string, `_i` is an integer, `_b` is binary and so on).

If the content type allows for nested values (JSON and XML), the extractors will flatten each field, seperated by dots. For example, if you have this XML:

```xml
<person>
  <pets>
    <pet>
      <name_s>Spot</name_s>
    </pet>
  </pets>
</person>
```

The XML extractor will convert it to the Solr field `person.pets.pet.name_s` with value `Spot`.

Lists of values are assumed to be Solr multi-valued fields.

```json
{"people_ss":["Ryan", "Eric", "Brett"]}
```

The above JSON will insert a list of three values into Solr to be indexed: `people_ss=Ryan`, `people_ss=Eric`, `people_ss=Brett`.

### Automatic Fields

When a Riak object is indexed, Riak Search automatically inserts a few extra fields as well. These are necessary for a variety of technical reasons, and for the most part you don't need to think about them. However, there are a few fields which you may find useful, all prefixed with `_yz`: `_yz_rk` (Riak key), `_yz_rt` (Riak bucket type), `_yz_rb` (Riak bucket), and `_yz_err` (extraction error). 

You can query by these fields, just like any other normal Solr fields. But most of the time you'll use `_yz_rk` as a query result, which tells you the Riak key that matches the query you just ran. Let's see this in detail by running some queries in the next section.


## Querying

After the schema, index, association, population/extraction/indexing has taken place, comes the fun part of querying for data.

### Simple Query

The basic query parameter is `q` via HTTP, or the first parameter of your chosen driver's `search` function. All Distributed Solr queries are supported, which actually includes most of the single-node solr queries. This example is searching for all documents where the `name_s` value begins with *Lion* by means of a glob (wildcard) match.

```curl
curl "$RIAK_HOST/search/famous?wt=json&q=name_s:Lion*" | jsonpp
```
```ruby
results = client.search("famous", "name_s:Lion*")
p results
p results['docs']
```
```python
import pprint
results = bucket.search('name_s:Lion*')
pprint.pprint(results)
pprint.pprint(results['docs'])
```
```erlang
{ok, Results} = riakc_pb_socket:search(Pid, <<"famous">>, <<"name_s:Lion*">>),
io:fwrite("~p~n", [Results]),
Docs = Results#search_results.docs,
io:fwrite("~p~n", [Docs]).
```

The response to a query will be an object containing details about the response, such as a query's max score and a list of documents which match the given query. It's worth nothing two things: a) the documents returned are search documents (a set of solr field/values), not a Riak value, and b) the HTTP response is a direct Solr response, while the drivers use protocol buffers and are encoded with different field names.

This is a common HTTP `response` value.

```json
{
  "numFound": 1,
  "start": 0,
  "maxScore": 1.0,
  "docs": [
    {
      "leader_b": true,
      "age_i": 30,
      "name_s": "Liono",
      "_yz_id": "default_cats_liono_37",
      "_yz_rk": "liono",
      "_yz_rt": "default",
      "_yz_rb": "cats"
    }
  ]
}
```

The most important field returned is `docs`, which is the list of objects that each contain fields about matching index documents. The values you'll use most often are `_yz_rk`, `_yz_rb` and `score`, which respectively represent the matching Riak key, Riak bucket, and the similarity of the matching doc to the query via [Lucene scoring](https://lucene.apache.org/core/4_6_0/core/org/apache/lucene/search/package-summary.html#scoring).

In this example the query fields are returned because they're stored in Solr. This depends on your schema. If they are not stored, you'll have to perform a seperate Riak GET operation to retrieve the value using the `_yz_rk` value.

```curl
curl "$RIAK_HOST/buckets/cats/keys/liono"
{"name_s":"Liono", "age_i":30, "leader_b":true}
```
```ruby
doc = results["docs"].first
bucket = Riak::Bucket.new(client, doc["_yz_rb"]) # cats
object = bucket.get( doc["_yz_rk"] )             # liono
p object.data
# {"name_s" => "Liono", "age_i" => 30, "leader_b" => true}
```
```python
doc = results['docs'][0]
bucket = client.bucket(doc['_yz_rb']) # cats
object = bucket.get(doc['_yz_rk'])    # liono
pprint.pprint(object.data)
# {"name_s": "Liono", "age_i": 30, "leader_b": true}
```
```erlang
[{Index,Doc}|_] = Docs,
Bucket = proplists:get_value(<<"_yz_rb">>, Doc),  %% <<"cats">>
Key = proplists:get_value(<<"_yz_rk">>, Doc),     %% <<"liono">>
{ok, Obj} = riakc_pb_socket:get(Pid, Bucket, Key),
Val = riakc_obj:get_value(Obj),
io:fwrite("~s~n", [Val]).
%% {"name_s":"Liono", "age_i":30, "leader_b":true}
```

This was one simple glob query example. There are many query options, a more complete list can be found by digging into [searching solr](https://cwiki.apache.org/confluence/display/solr/Searching). Let's look at a few others.

#### Range Queries

Searches within a [range](https://cwiki.apache.org/confluence/display/solr/The+Standard+Query+Parser#TheStandardQueryParser-DifferencesbetweenLuceneQueryParserandtheSolrStandardQueryParser) of numerical or date/[datemath](http://lucene.apache.org/solr/4_6_0/solr-core/org/apache/solr/util/DateMathParser.html) values.

To find the ages of all famous cats who are 30 or younger: `age_i:[0 TO 30]`. If you wanted to find all cats 30 or older, you could include a glob as a top end of the range: `age_i:[30 TO *]`.

```curl
curl "$RIAK_HOST/search/famous?wt=json&q=age_i:%5B30%20TO%20*%5D" | jsonpp
```
```ruby
client.search("famous", "age_i:[30 TO *]")
```
```python
bucket.search('age_i:[30 TO *]')
```
```erlang
riakc_pb_socket:search(Pid, <<"famous">>, <<"age_i:[30 TO *]">>),
```

<!-- TODO: pubdate:[NOW-1YEAR/DAY TO NOW/DAY+1DAY] -->

#### Boolean

You can perform logical conjunctive, disjunctive, and negative operations on query elements as, repectively, `AND`, `OR` and `NOT`. Let's say we want to see who is capable of being a US Senator (at least 30 years old, and a leader). It requires a conjunctive query: `leader_b:true AND age_i:%5B25%20TO%20*%5D`.

```curl
curl "$RIAK_HOST/search/famous?wt=json&q=leader_b:true%20AND%20age_i:%5B25%20TO%20*%5D" | jsonpp
```
```ruby
client.search("famous", "leader_b:true AND age_i:[30 TO *]")
```
```python
bucket.search('leader_b:true AND age_i:[30 TO *]')
```
```erlang
riakc_pb_socket:search(Pid, <<"famous">>, <<"leader_b:true AND age_i:[30 TO *]">>),
```

#### Pagination

A common requirement you may face is paginating searches, where an ordered set of matching documents are returned in non-overlapping sequential subsets (in other words, *pages*). This is easy to do with the `start` and `rows` parameters, where `start` is the number of documents to skip over (the offset) and `rows` are the number of results to return in one go.

For example, assuming we want two results per page, getting the second page is easy, where `start` is calculated as _rows per page * (page number - 1)_.

```curl
ROWS_PER_PAGE=2
PAGE=2
START=$(($ROWS_PER_PAGE * ($PAGE-1)))

curl "$RIAK_HOST/search/famous?wt=json&q=*:*&start=$START&rows=$ROWS_PER_PAGE" | jsonpp
```
```ruby
ROWS_PER_PAGE=2
page = 2
start = ROWS_PER_PAGE * (page - 1)

client.search("famous", "*:*", {:start => start, :rows => ROWS_PER_PAGE})
```
```python
ROWS_PER_PAGE=2
page = 2
start = ROWS_PER_PAGE * (page - 1)

bucket.search('*:*', start=start, rows=ROWS_PER_PAGE)
```
```erlang
-define(ROWS_PER_PAGE, 2).

Page = 2,
Start = ?ROWS_PER_PAGE * (Page - 1),

riakc_pb_socket:search(Pid, <<"famous">>, <<"*:*">>, [{start, Start},{rows, ?ROWS_PER_PAGE}]),
```

### MapReduce

Riak Search allows for piping search results as inputs for MapReduce jobs. This is a useful crossection for performing a post calculations of results, or aggregations of ad-hoc queries. The Riak Search MapReduce integration works similar to regular mapreduce, with the notable exception that your input is not a bucket, but index and query arguments to the `yokozuna` module and `mapred_search` function (an Erlang module:function pair that adds the Riak Search hook to MapReduce).

```json
{
  "inputs":{
    "module":"yokozuna",
    "function":"mapred_search",
    "arg":["famous","NOT leader_b:true"]
  },
  "query":[
    {
      "map":{
        "language":"javascript",
        "keep":false,
        "source":"function(v) { return [1]; }"
      }
    },
    {
      "reduce":{
        "language":"javascript",
        "keep":true,
        "name":"Riak.reduceSum"
      }
    }
  ]
}
```

In this example we're searching for all famous cats which are not leaders, and counting up the results using Javascript for both map and reduce. It should return the reduced sum of `[3]`.

```curl
curl -XPOST "$RIAK_HOST/mapred" \
  -H'content-type:application/json' \
  -d '{"inputs":{"module":"yokozuna","function":"mapred_search","arg":["famous","NOT leader_b:true"]},"query":[{"map":{"language":"javascript","keep":false,"source":"function(v) { return [1]; }"}},{"reduce":{"language":"javascript","keep":true,"name":"Riak.reduceSum"}}]}'
```

<!--
#### Functions

Cats age about 7 times faster than humans, so to calculate an age in "cat years", you can multiply the cat's age by 7 using the `product` function. It's only one out of many built-in [Solr Functions](https://cwiki.apache.org/confluence/display/solr/Function+Queries#FunctionQueries-AvailableFunctions).

```curl
curl "$RIAK_HOST/search/famous?wt=json&q=*:*&fl=_yz_rk,age_i:product(age_i,7)" | jsonpp
```
 -->

## Feature List

Riak Search 2.0 is more than a distributed search engine like [SolrCloud](https://cwiki.apache.org/confluence/display/solr/SolrCloud) or [ElasticSearch](http://www.elasticsearch.org/). It's a searchable integration with Riak. This greatly simplifies usage by offloading the task of indexing values to Riak.

Riak Search's features and enhancements are numerous.

* Support for various MIME types (JSON, XML, plain text, datatypes) for automatic data extraction
* Support for [various language](https://cwiki.apache.org/confluence/display/solr/Language+Analysis) specific [analyzers, tokenizers, and filters](https://cwiki.apache.org/confluence/display/solr/Overview+of+Analyzers%2C+Tokenizers%2C+and+Filters)
* Robust, easy-to-use [query languages](https://cwiki.apache.org/confluence/display/solr/Other+Parsers) like lucene (default) and dismax.
* Queries: exact match, globs, inclusive/exclusive range queries, AND/OR/NOT, prefix matching, proximity searches, term boosting, sorting, pagination
* Protocol Buffer interface and Solr interface via HTTP
* Scoring and ranking for most relevant results
* Query result [highlighting](https://cwiki.apache.org/confluence/display/solr/Highlighting)
* Search queries as input for MapReduce jobs
* Active Anti Entropy for automatic index repair

{{/2.0.0+}}
{{#2.0.0-}}

You must first [[enable Riak Search|Riak Search Settings]] in your environment to use it.

## Introduction

Riak Search is a distributed, full-text search engine that is built on Riak Core and included as part of Riak open source. Search provides the most advanced query capability next to MapReduce, but is far more concise; easier to use, and in most cases puts far less load on the cluster.

Search indexes Riak KV objects as they're written using a precommit hook. Based on the object’s mime type and the Search schema you’ve set for its bucket, the hook will automatically extract and analyze your data and build indexes from it. The Riak Client API is used to invoke Search queries that return a list of bucket/key pairs matching the query. Currently the PHP, Python, Ruby, and Erlang client libraries support integration with Riak Search.

### Features

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

### When to Use Search

* When collecting, parsing, and storing data like user bios, blog posts, journal articles, etc. to facilitate fast and accurate information retrieval in addition to some reasonable form of ranking.
* When indexing JSON data. Extractors pull the data apart depending on mime type, analyzers tokenize the fields, and the schema describes what fields to index, how to analyze them and how to store them.
* When fast information retrieval with a powerful query language is needed.

### When Not to Use Search

* When only simple tagging of data is needed with exact match and range queries. In this case [[Secondary Indexes|Using Secondary Indexes]] would be easier.
* When the data is not easily analyzed by Search. For example mp3, video, or some other binary format. In this case a Secondary Index interface is recommended.
* When built-in anti-entropy/consistency is required. At this time Search has no read-repair mechanism. If Search index data is lost, the entire data set must be re-indexed.

## Indexing Data

Before you can search for values, you first must index them. In its standard form, Riak Search requires you to index a value manually. You can find a detailed list of indexing commands in the [[Search Indexing Reference]].

If you want a simpler, but less explicit form of indexing, check out the [[Search, KV and MapReduce|Advanced Search#Search-KV-and-MapReduce]] section of [[Advanced Search]].


<!-- Was "Riak Search - Querying" -->

## Query Interfaces

### Querying via Command Line

Riak Search comes equiped with a command line tool `search-cmd` for testing
your query syntax.

This example will display a list of document ID values matching the title "See spot run".

```bash
bin/search-cmd search books "title:\"See spot run\""
```

### Solr

Riak Search supports a Solr-compatible interface for searching documents via HTTP. By default, the select endpoint is located at `http://hostname:8098/solr/select`.

Alternatively, the index can be included in the URL, for example `http://hostname:8098/solr/INDEX/select`.

The following parameters are supported:

  * `index=INDEX`: Specifies the default index name.
  * `q=QUERY`: Run the provided query.
  * `df=FIELDNAME`: Use the provided field as the default. Overrides the "default_field" setting in the schema file.
  * `q.op=OPERATION`: Allowed settings are either "and" or "or". Overrides the "default_op" setting in the schema file. Default is "or".
  * `start=N`: Specify the starting result of the query. Useful for paging. Default is 0.
  * `rows=N`: Specify the maximum number of results to return. Default is 10.
  * `sort=FIELDNAME`: Sort on the specified field name after the given rows are found. Default is "none", which causes the results to be sorted in descending order by score.
  * `wt=FORMAT`: Choose the format of the output.  Options are "xml" and "json".  The default is "xml".
  * `filter=FILTERQUERY`: Filters the search by an additional query scoped to [[inline fields|Advanced Search#Fields-and-Field-Level-Properties]].
  * `presort=key|score`: Sorts all of the results by bucket key, or the search score, before the given rows are chosen. This is useful when paginating to ensure the results are returned in a consistent order.
      <div class="info">
      <div class="title">Limitations on Presort</div>

      When paginating results with **presort**, note that the results may only be sorted by the search **score** or sorted by the **key order**. There is currently no way to pre-sort on an arbitrary field. This means that if you with to paginate on some field, build your keys to include that field value then use `presort=key`.
      </div>

To query data in the system with Curl:

```curl
$ curl "http://localhost:8098/solr/books/select?start=0&rows=10000&q=prog*"
```

### Riak Client API

The Riak Client APIs have been updated to support querying of Riak Search. See the client documentation for more information. Currently, the Ruby, Python, PHP, and Erlang clients are supported.

The API takes a default search index as well as as search query, and returns a list of bucket/key pairs. Some clients transform this list into objects specific to that client.

### MapReduce

The Riak Client APIs that integrate with Riak Search also support using a search query to generate inputs for a MapReduce operation. This allows you to perform powerful analysis and computation across your data based on a search query. See the client documentation for more information. Currently, the Java, Ruby, Python, PHP, and Erlang clients are supported.

Kicking off a MapReduce query with the same result set over HTTP would use a POST body like this:

```json
{
  "inputs": {
             "bucket":"mybucket",
             "query":"foo OR bar"
            },
  "query":...
 }
```

or

```json
{
  "inputs": {
             "bucket":"mybucket",
             "query":"foo OR bar",
             "filter":"field2:baz"
            },
  "query":...
 }
```

The phases in the "query" field should be exactly the same as usual.  An initial map phase will be given each object matching the search for processing, but an initial link phase or reduce phase will also work.

The query field specifies the search query.  All syntax available in other Search interfaces is available in this query field.  The optional filter field specifies the query filter.

The old but still functioning syntax is:

```json
{
  "inputs": {
             "module":"riak_search",
             "function":"mapred_search",
             "arg":["customers","first_name:john"]
            },
  "query":...
 }
```

The "arg" field of the inputs specification is always a two-element list.  The first element is the name of the bucket you wish to search, and the second element is the query to search for.


## Query Syntax

Search queries use the same syntax as [Lucene](http://lucene.apache.org/java/2_9_1/queryparsersyntax.html) and supports most Lucene operators including term searches, field searches, boolean operators, grouping, lexicographical range queries, and wildcards (at the end of a word only).

### Terms and Phrases

A query can be as simple as a single term (ie: "red") or a series of terms surrounded by quotes called a phrase ("See spot run"). The term (or phrase) is analyzed using the default analyzer for the index.

The index schema contains a {{default_operator}} setting that determines whether a phrase is treated as an AND operation or an OR operation. By default, a phrase is treated as an OR operation. In other words, a document is returned if it matches any one of the terms in the phrase.

### Fields

You can specify a field to search by putting it in front of the term or phrase to search. For example:


```bash
color:red
```

Or:

```bash
title:"See spot run"
```

You can further specify an index by prefixing the field with the index name. For example:

```bash
products.color:red
```

Or:

```bash
books.title:"See spot run"
```

If your field contains special characters, such as ('+','-','/','[',']','(',')',':' or space), then either surround the phrase in single quotes, or escape each special character with a backslash.

```bash
books.url:'http://mycompany.com/url/to/my-book#foo'
```

-or-

```bash
books.url:http\:\/\/mycompany.com\/url\/to\/my\-book\#foo
```

### Wildcard Searches

Terms can include wildcards in the form of an asterisk ( * ) to allow prefix matching, or a question mark ( ? ) to match a single character.

Currently, the wildcard must come at the end of the term in both cases, and must be preceded by a minimum of two characters.

For example:

* "bus*" will match "busy", "business", "busted", etc.
* "bus?" will match "busy", "bust", "busk", etc.


### Proximity Searches

Proximity searching allows you to find terms that are within a certain number of words from each other. To specify a proximity search, use the tilde argument on a phrase.

For example:

```bash
"See spot run"~20
```

Will find documents that have the words "see", "spot", and "run" all within the same block of 20 words.

### Range Searches

Range searches allow you to find documents with terms in between a specific range. Ranges are calculated lexicographically.  Use square brackets to specify an inclusive range, and curly braces to specify an exclusive range.

The following example will return documents with words containing "red" and "rum", plus any words in between.

```bash
"field:[red TO rum]"
```

The following example will return documents with words in between "red" and "rum":

```bash
"field:{red TO rum}"
```

### Boosting a Term

A term (or phrase) can have its score boosted using the caret operator along with an integer boost factor.

In the following example, documents with the term "red" will have their score boosted:

```bash
red^5 OR blue
```

### Boolean Operators - AND, OR, NOT

Queries can use the boolean operators AND, OR, and NOT. The boolean operators must be capitalized.

The following example return documents containing the words "red" and "blue" but not "yellow".

```bash
red AND blue AND NOT yellow
```

The required ( + ) operator can be used in place of "AND", and the prohibited ( - ) operator can be used in place of "AND NOT". For example, the query above can be rewritten as:

```bash
+red +blue -yellow
```

### Grouping

Clauses in a query can be grouped using parentheses. The following query returns documents that contain the terms "red" or "blue", but not "yellow":

```bash
(red OR blue) AND NOT yellow
```

{{/2.0.0-}}
