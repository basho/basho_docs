---
title: Using Search
project: riak
version: 1.0.0+
document: tutorials
toc: true
audience: beginner
keywords: [developers, search, kv]
---

<div class="note">
<div class="title">Note on Search 2.0 vs. Legacy Search</div>
This document refers to the new Riak Search 2.0 with
[[Solr|http://lucene.apache.org/solr/]] integration (codenamed
Yokozuna). For information about the deprecated Riak Search, visit [[the
old Using Riak Search
docs|http://docs.basho.com/riak/1.4.10/dev/using/search/]].
</div>

You must first [[enable Riak Search|Riak Search Settings]] in your
environment to use it.

## Introduction

Riak Search 2.0 is a new open-source project integrated with Riak. It
allows for distributed, scalable, fault-tolerant, transparent indexing
and querying of Riak values. It's easy to use. After connecting a bucket
(or [[bucket type|Using Bucket Types]]) to a Solr index, you simply
write values (such as JSON, XML, plain text, [[Riak Data Types|Using
Data Types]], etc.) into Riak as normal, and then query those indexed
values using the Solr API.

### Why Riak Search

Some of Riak's core strengths lie in its scalability, fault tolerance,
and ease of operations. However, Riak models its data in key/value
objects, and a key/value store is something that few would claim is easy
to query. This is the driving force behind Riak Search: to provide an
integrated, scalable mechanism to build ad hoc queries against values
stored in a Riak cluster, while holding true to Riak's core strengths.

![Yokozuna](/images/yokozuna.png)

## Simple Setup

Riak Search 2.0 is an integration of Solr (for indexing and querying)
and Riak (for storage and distribution). There are a few points of
interest that a user of Riak Search will have to keep in mind in order
to properly store and later query for values.

1. **Schemas** explain to Solr how to index fields
2. **Indexes** are named Solr indexes against which you will query
3. **Bucket-index association** signals to Riak *when* to index values

Riak Search must first be configured with a Solr *schema* so that Solr
knows how to index value fields. If you don't define one, you're
provided with a default schema named `_yz_default`. The examples in this
document will presume the default. You can read more about creating a
custom schema in [[Search Schema]], which you'll want to do in a
production environment.

Next, you must create a named Solr index through Riak Search. This index
represents a collection of similar data that you connect with to perform
queries. When creating an index, you can optionally provide a schema. If
you do not, the default schema will be used. Here we'll `curl` create an
index named `famous` with the default schema.

<div class="note">
<div class="title">Note on index names</div>
Note that index names may only be
[ASCII](http://en.wikipedia.org/wiki/ASCII) values from 32-127 (spaces,
standard puncutation, digits, and word characters). This may change in
the future to allow full [Unicode](http://en.wikipedia.org/wiki/Unicode)
support.
</div>

All `curl` examples in this document assume that you have set an
environment variable named `RIAK_HOST`, which points to a Riak base URL,
such as `http://localhost:8098`. The approriate value for `RIAK_HOST`
will depend on your [[configuration|Configuration
Files#Client-Interfaces]].

```java
YokozunaIndex famousIndex = new YokozunaIndex("famous", "_yz_default");
StoreIndex storeIndex =
    new StoreIndex.Builder(famousIndex).build();
client.execute(storeIndex);
```

```ruby
client.create_search_index('famous', '_yz_default')
```

```python
client.create_search_index('famous', '_yz_default')
```

```erlang
riakc_pb_socket:create_search_index(Pid, <<"famous">>, <<"_yz_default">>, []).
```

```curl
export RIAK_HOST="http://localhost:8098"

curl -XPUT $RIAK_HOST/search/index/famous
```

<div class="note">
<div class="title">Getting started with Riak clients</div>
If you are connecting to Riak using one of Basho's official [[client
libraries]], you can find more information about getting started with
your client in our [[quickstart guide|Five-Minute
Install#setting-up-your-riak-client]].
</div>

Note that the above command is exactly the same as the following, which
explicitly defines the default schema.

```java
YokozunaIndex famousIndex = new YokozunaIndex("famous", "_yz_default");
StoreIndex storeIndex = new StoreIndex.Builder(famousIndex)
        .build();
client.execute(storeIndex);
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

```curl
curl -XPUT $RIAK_HOST/search/index/famous \
     -H 'Content-Type: application/json' \
     -d '{"schema":"_yz_default"}'
```

The last setup item you need to perform is to associate either a bucket
or a [[bucket type|Using Bucket Types]] with a Solr index. You only need
do this once per bucket type, and all buckets within that type will use
the same Solr index. For example, to associate a bucket type named
`animals` with the `famous` index, you can set the bucket type property
`search_index`. If a Solr index is to be used by only *one* Riak bucket,
you can set the `search_index` property on that bucket. If more than one
bucket is to share a Solr index, a bucket type should be used. More on
bucket types in the section directly below.

### Bucket Types

Since Riak 2.0, Basho suggests that you [[use bucket types|Using Bucket
Types]] to namespace all buckets you create. Bucket types have a lower
overhead within the cluster than the default bucket namespace, but
require an additional setup step on the command line.

When creating a new bucket type, you can create a bucket type without
any properties and set individual buckets to be indexed. The step below
creates and activates the bucket type:

```bash
riak-admin bucket-type create animals '{"props":{}}'
riak-admin bucket-type activate animals
```

And this step applies the index to the `cats` bucket, which bears the
`animals` bucket type we just created and activated:

```curl
curl -XPUT $RIAK_HOST/types/animals/buckets/cats/props \
     -H 'Content-Type: application/json' \
     -d '{"props":{"search_index":"famous"}}'
```

Another possibility is to set the `search_index` as a default property
of the bucket type. This means _any_ bucket under that type will
inherit that setting and have its values indexed.

```bash
riak-admin bucket-type create animals '{"props":{"search_index":"famous"}}'
riak-admin bucket-type activate animals
```

If you ever need to turn off indexing for a bucket, set the
`search_index` property to the `_dont_index_` sentinel.


### Bucket Properties

Although we recommend that you use all new buckets under a bucket type,
if you have existing data with a type-free bucket (or under the
`default` bucket type) you can set the `search_index` property for a
bucket using the [[HTTP API]].

```curl
curl -XPUT "$RIAK_HOST/buckets/cats/props" \
     -H'content-type:application/json' \
     -d'{"props":{"search_index":"famous"}}'
```

### Security

Security is a new feature of Riak that lets an administrator limit
access to certain resources. In the case of search, your options are to
limit administration of schemas or indexes (permission `search.admin`)
to certain users, and to limit querying (permission `search.query`) to
any index or a specific one. The example below shows the various
options.

```bash
riak-admin security grant search.admin on schema to username
riak-admin security grant search.admin on index to username
riak-admin security grant search.query on index to username
riak-admin security grant search.query on index famous to username
```

Those permissions can also be revoked:

```bash
riak-admin security revoke search.admin on schema from username
riak-admin security revoke search.admin on index from username
riak-admin security revoke search.query on index from username
riak-admin security revoke search.query on index famous from username
```

## Indexing Values

With a Solr schema, index, and association in place, we're ready to
start using Riak Search. First, populate the `cat` bucket with values,
in this case information about four cats: Liono, Cheetara, Snarf, and
Panthro.

Depending on the driver you use, you may have to specify the content
type, which for this example is `application/json`. In the case of Ruby
and Python the content type is automatically set for you based on the
object given.

```java
Namespace animalsBucket = new Namespace("animals");
String json = "application/json";

RiakObject liono = new RiakObject()
        .setContentType(json)
        .setValue(BinaryValue.create("{\"name_s\":\"Lion-o\",\"age\":30,\"leader\":true)"));
RiakObject cheetara = new RiakObject()
        .setContentType(json)
        .setValue(BinaryValue.create("{\"name_s\":\"Cheetara\",\"age\":30,\"leader\":false)"));
RiakObject snarf = new RiakObject()
        .setContentType(json)
        .setValue(BinaryValue.create("{\"name_s\":\"Snarf\",\"age\":43"));
RiakObject panthro = new RiakObject()
        .setContentType(json)
        .setValue(BinaryValue.create("{\"name_s\":\"Panthro\",\"age_i\":36}"));
Location lionoLoc = new Location(animalsBucket, "liono");
Location cheetaraLoc = new Location(animalsBucket, "cheetara");
Location snarfLoc = new Location(animalsBucket, "snarf");
Location panthroLoc = new Location(animalsBucket, "panthro");

StoreValue lionoStore = new StoreValue.Builder(liono).withLocation(lionoLoc).build();
StoreValue cheetaraStore = new StoreValue.Builder(cheetara).withLocation(cheetaraLoc).build();
StoreValue snarfStore = new StoreValue.Builder(snarf).withLocation(snarfLoc).build();
StoreValue panthroStore = new StoreValue.Builder(panthro).withLocation(panthroLoc).build();

client.execute(lionoStore);
// The other storage operations can be performed the same way
```

```ruby
bucket = client.bucket("animals")

cat = bucket.get_or_new("liono")
cat.data = {"name_s" => "Lion-o", "age_i" => 30, "leader_b" => true}
cat.store(type: "cats")

cat = bucket.get_or_new("cheetara")
cat.data = {"name_s" => "Cheetara", "age_i" => 28, "leader_b" => false}
cat.store(type: "cats")

cat = bucket.get_or_new("snarf")
cat.data = {"name_s" => "Snarf", "age_i" => 43}
cat.store(type: "cats")

cat = bucket.get_or_new("panthro")
cat.data = {"name_s" => "Panthro", "age_i" => 36}
cat.store(type: "cats")
```

```python
bucket = client.bucket_type('animals').bucket('cats')

cat = bucket.new('liono', {'name_s': 'Lion-o', 'age_i': 30, 'leader_b': True})
cat.store()

cat = bucket.new('cheetara', {'name_s':'Cheetara', 'age_i':28, 'leader_b': True})
cat.store()

cat = bucket.new('snarf', {'name_s':'Snarf', 'age_i':43})
cat.store()

cat = bucket.new('panthro', {'name_s':'Panthro', 'age_i':36})
cat.store()
```

```erlang
CO = riakc_obj:new({<<"animals">>, <<"cats">>}, <<"liono">>,
    <<"{\"name_s\":\"Lion-o\", \"age_i\":30, \"leader_b\":true}">>,
    "application/json"),
riakc_pb_socket:put(Pid, CO),

C1 = riakc_obj:new({<<"animals">>, <<"cats">>}, <<"cheetara">>,
    <<"{\"name_s\":\"Cheetara\", \"age_i\":28, \"leader_b\":false}">>,
    "application/json"),
riakc_pb_socket:put(Pid, C1),

C2 = riakc_obj:new({<<"animals">>, <<"cats">>}, <<"snarf">>,
    <<"{\"name_s\":\"Snarf\", \"age_i\":43}">>,
    "application/json"),
riakc_pb_socket:put(Pid, C2),

C3 = riakc_obj:new({<<"animals">>, <<"cats">>}, <<"panthro">>,
    <<"{\"name_s\":\"Panthro\", \"age_i\":36}">>,
    "application/json"),
riakc_pb_socket:put(Pid, C3),
```

```curl
curl -XPUT "$RIAK_HOST/types/animals/buckets/cats/keys/liono" \
     -H'content-type:application/json' \
     -d'{"name_s":"Lion-o", "age_i":30, "leader_b":true}'

curl -XPUT "$RIAK_HOST/types/animals/buckets/cats/keys/cheetara" \
     -H'content-type:application/json' \
     -d'{"name_s":"Cheetara", "age_i":28, "leader_b":false}'

curl -XPUT "$RIAK_HOST/types/animals/buckets/cats/keys/snarf" \
     -H'content-type:application/json' \
     -d'{"name_s":"Snarf", "age_i":43}'

curl -XPUT "$RIAK_HOST/types/animals/buckets/cats/keys/panthro" \
     -H'content-type:application/json' \
     -d'{"name_s":"Panthro", "age_i":36}'
```

If you've used Riak before, you may have noticed that this is no
different from setting values without Riak Search. This is how we would
sum up the design goals of Riak Search:

#### Write it like Riak, query it like Solr

But how does Riak Search know how to index values, given that values are
opaque in Riak? For that, we employ extractors.

### Extractors

*Extractors* are modules in Riak that accept a Riak value with a certain
content type, and convert it into a list of fields capable of being
indexed by Solr. This is done transparently and automatically as part of
the indexing process.

Our current example uses the JSON extractor, but Riak Search also
extracts indexable fields from the following content types:

* JSON (`application/json`)
* XML (`application/xml`, `text/xml`)
* Plain text (`text/plain`)
* [[Riak Data Types|Using Data Types]]
  * counter (`application/riak_counter`)
  * map (`application/riak_map`)
  * set (`application/riak_set`)
* noop (unknown content type)

In the examples we've seen, the JSON field `name_s` is translated to a
Solr index document field insert. Solr will index any field that it
recognizes, based on the index's schema. The default schema
(`_yz_default`) uses the suffix to decide the field type (`_s`
represents a string, `_i` is an integer, `_b` is binary and so on).

If the content type allows for nested values (e.g. JSON and XML), the
extractors will flatten each field, seperated by dots. For example, if
you have this XML:

```xml
<person>
  <pets>
    <pet>
      <name_s>Spot</name_s>
    </pet>
  </pets>
</person>
```

XML The extractor will convert it to the Solr field
`person.pets.pet.name_s` with value `Spot`.

Lists of values are assumed to be Solr multi-valued fields.

```json
{"people_ss":["Ryan", "Eric", "Brett"]}
```

The above JSON will insert a list of three values into Solr to be
indexed: `people_ss=Ryan`, `people_ss=Eric`, `people_ss=Brett`.

### Automatic Fields

When a Riak object is indexed, Riak Search automatically inserts a few
extra fields as well. These are necessary for a variety of technical
reasons, and for the most part you don't need to think about them.
However, there are a few fields which you may find useful:

- `_yz_rk` (Riak key)
- `_yz_rt` (Riak bucket type)
- `_yz_rb` (Riak bucket)
- `_yz_err` (extraction error)

You can query by these fields, just like any other normal Solr fields.
But most of the time you'll use `_yz_rk` as a query result, which tells
you the Riak key that matches the query you just ran. Let's see this in
detail by running some queries in the next section.

## Querying

After the schema, index, association, population/extraction/indexing has
taken place, comes the fun part of querying for data.

### Simple Query

The basic query parameter is `q` via HTTP, or the first parameter of
your chosen driver's `search` function. All distributed Solr queries are
supported, which actually includes most of the single-node Solr queries.
This example searches for all documents in which the `name_s` value
begins with `Lion` by means of a glob (wildcard) match.

```java
SearchOperation searchOp = new SearchOperation
        .Builder(BinaryValue.create("famous"), "name_s:Lion*")
        .build();
cluster.execute(searchOp);
// This will display the actual results as a List of Maps:
List<Map<String, List<String> results = searchOp.get().getAllResults();
// This will display the number of results:
System.out.println(results);
```

```ruby
results = client.search("famous", "name_s:Lion*")
p results
p results['docs']
```

```python
results = client.fulltext_search('famous', 'name_s:Lion*')
print results
print results['docs']
```

```erlang
{ok, Results} = riakc_pb_socket:search(Pid, <<"famous">>, <<"name_s:Lion*">>),
io:fwrite("~p~n", [Results]),
Docs = Results#search_results.docs,
io:fwrite("~p~n", [Docs]).

%% Please note that this example relies on an Erlang record definition
%% for the search_result record found here:
%% https://github.com/basho/riak-erlang-client/blob/master/include/riakc.hrl
```

```curl
curl $RIAK_HOST/search/query/famous?wt=json&q=name_s:Lion* | jsonpp
```

The response to a query will be an object containing details about the
response, such as a query's max score and a list of documents which
match the given query. It's worth noting two things:

- The documents returned are search documents (a set of Solr
  field/values), not a Riak value
- The HTTP response is a direct Solr response, while the drivers use
  Protocol Buffers and are encoded with different field names

This is a common HTTP `response` value:

```json
{
  "numFound": 1,
  "start": 0,
  "maxScore": 1.0,
  "docs": [
    {
      "leader_b": true,
      "age_i": 30,
      "name_s": "Lion-o",
      "_yz_id": "default_cats_liono_37",
      "_yz_rk": "liono",
      "_yz_rt": "default",
      "_yz_rb": "cats"
    }
  ]
}
```

The most important field returned is `docs`, which is the list of
objects that each contain fields about matching index documents. The
values you'll use most often are `_yz_rt` (Riak bucket type), `_yz_rb`
(Riak bucket), `_yz_rk` (Riak key), and `score` which represent the
similarity of the matching doc to the query via [Lucene
scoring](https://lucene.apache.org/core/4_6_0/core/org/apache/lucene/search/package-summary.html#scoring).

In this example the query fields are returned because they're stored in
Solr. This depends on your schema. If they are not stored, you'll have
to perform a separate Riak GET operation to retrieve the value using the
`_yz_rk` value.

```java
// Using the results object from above
Map<String, List<String> doc = results.get(0);
String bucketType = doc.get("_yz_rt").get(0);
String bucket = doc.get("yz_rb").get(0);
String key = doc.get("_yz_rk").get(0);
Namespace namespace = new Namespace(bucketType, bucket);
Location objectLocation = new Location(namespace, key);
FetchValue fetchOp = new FetchValue.Builder(objectLocation)
        .build();
RiakObject obj = client.execute(fetchOp).getValue(RiakObject.class);
System.out.println(obj.getValue());

// {"name_s": "Lion-o", "age_i": 30, "leader_b": true}
```

```ruby
doc = results['docs'].first
btype = Riak::BucketType.new(client, doc["_yz_rt"]) # animals
bucket = Riak::Bucket.new(client, doc["_yz_rb"])    # cats
object = bucket.get( doc["_yz_rk"] )                # liono
p object.data

# {"name_s" => "Lion-o", "age_i" => 30, "leader_b" => true}
```

```python
doc = results['docs'][0]
bucket = client.bucket_type(doc['_yz_rt']).bucket(doc['_yz_rb']) # animals/cats
object = bucket.get(doc['_yz_rk'])    # liono
print object.data

# {"name_s": "Lion-o", "age_i": 30, "leader_b": true}
```

```erlang
[{Index,Doc}|_] = Docs,
BType  = proplists:get_value(<<"_yz_rt">>, Doc),  %% <<"animals">>
Bucket = proplists:get_value(<<"_yz_rb">>, Doc),  %% <<"cats">>
Key    = proplists:get_value(<<"_yz_rk">>, Doc),  %% <<"liono">>
{ok, Obj} = riakc_pb_socket:get(Pid, {BType, Bucket}, Key),
Val = riakc_obj:get_value(Obj),
io:fwrite("~s~n", [Val]).

%% {"name_s":"Lion-o", "age_i":30, "leader_b":true}
```

```curl
curl "$RIAK_HOST/types/animals/buckets/cats/keys/liono"

# Response:

{"name_s":"Lion-o", "age_i":30, "leader_b":true}
```

This was one simple glob query example. There are many query options, a
more complete list of which can be found by digging into [searching
Solr](https://cwiki.apache.org/confluence/display/solr/Searching). Let's
look at a few others.

#### Range Queries

Searches within a
[range](https://cwiki.apache.org/confluence/display/solr/The+Standard+Query+Parser#TheStandardQueryParser-DifferencesbetweenLuceneQueryParserandtheSolrStandardQueryParser)
of numerical or
date/[datemath](http://lucene.apache.org/solr/4_6_0/solr-core/org/apache/solr/util/DateMathParser.html)
values.

To find the ages of all famous cats who are 30 or younger: `age_i:[0 TO
30]`. If you wanted to find all cats 30 or older, you could include a
glob as a top end of the range: `age_i:[30 TO *]`.

```java
String index = "famous";
String query = "age_i:[30 TO *]";
SearchOperation searchOp = new SearchOperation
        .Builder(BinaryValue.create(index), query)
        .build();
cluster.execute(searchOp);
SearchOperation.Response results = searchOp.get();
```

```ruby
client.search("famous", "age_i:[30 TO *]")
```

```python
client.fulltext_search('famous', 'age_i:[30 TO *]')
```

```erlang
riakc_pb_socket:search(Pid, <<"famous">>, <<"age_i:[30 TO *]">>),
```

```curl
curl "$RIAK_HOST/search/query/famous?wt=json&q=age_i:%5B30%20TO%20*%5D" | jsonpp
```

<!-- TODO: pubdate:[NOW-1YEAR/DAY TO NOW/DAY+1DAY] -->

#### Boolean

You can perform logical conjunctive, disjunctive, and negative
operations on query elements as, repectively, `AND`, `OR`, and `NOT`.
Let's say we want to see who is capable of being a US Senator (at least
30 years old, and a leader). It requires a conjunctive query:
`leader_b:true AND age_i:[25 TO *]`.

```java
String index = "famous";
String query = "leader_b:true AND age_i:[30 TO *]";
Search searchOp = new Search.Builder(index, query).build();
cluster.execute(searchOp);
SearchOperation.Response results = searchOp.get();
```

```ruby
client.search("famous", "leader_b:true AND age_i:[30 TO *]")
```

```python
client.fulltext_search('famous', 'leader_b:true AND age_i:[30 TO *]')
```

```erlang
riakc_pb_socket:search(Pid, <<"famous">>, <<"leader_b:true AND age_i:[30 TO *]">>),
```

```curl
curl $RIAK_HOST/search/query/famous?wt=json&q=leader_b:true%20AND%20age_i:%5B25%20TO%20*%5D | jsonpp
```


### Deleting Indexes

Indexes may be deleted if they have no buckets associated with them:

```java
String index = "famous";
YzDeleteIndexOperation deleteOp = new YzDeleteIndexOperation.Builder(index)
        .build();
cluster.execute(deleteOp);
```

```ruby
client.delete_search_index('famous')
```

```python
client.delete_search_index('famous')
```

```erlang
riakc_pb_socket:delete_search_index(Pid, <<"famous">>, []),
```

```curl
curl -XDELETE $RIAK_HOST/search/index/famous
```

If an does have a bucket associated with it, then that index's
`search_index` property must be changed to either a different index name
or to the sentinel value `_dont_index_`.

```curl
curl -XPUT $RIAK_HOST/types/animals/buckets/cats/props \
  -H 'Content-Type: application/json' \
  -d '{"props":{"search_index":"_dont_index_"}}'
```

#### Pagination

A common requirement you may face is paginating searches, where an
ordered set of matching documents are returned in non-overlapping
sequential subsets (in other words, *pages*). This is easy to do with
the `start` and `rows` parameters, where `start` is the number of
documents to skip over (the offset) and `rows` are the number of results
to return in one go.

For example, assuming we want two results per page, getting the second
page is easy, where `start` is calculated as _rows per page * (page
number - 1)_.

```java
int rowsPerPage = 2;
int page = 2;
int start = rowsPerPage * (page - 1);

SearchOperation searchOp = new SearchOperation
        .Builder(BinaryValue.create("famous"), "*:*")
        .withStart(start)
        .withNumRows(rowsPerPage)
        .build();
client.execute(searchOp);
StoreOperation.Response response = searchOp.get();
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

client.fulltext_search('famous', '*:*', start=start, rows=ROWS_PER_PAGE)
```

```erlang
-define(ROWS_PER_PAGE, 2).

Page = 2,
Start = ?ROWS_PER_PAGE * (Page - 1),

riakc_pb_socket:search(Pid, <<"famous">>, <<"*:*">>, [{start, Start},{rows, ?ROWS_PER_PAGE}]),
```

```curl
ROWS_PER_PAGE=2
PAGE=2
START=$(($ROWS_PER_PAGE * ($PAGE-1)))

curl $RIAK_HOST/search/query/famous?wt=json&q=*:*&start=$START&rows=$ROWS_PER_PAGE | jsonpp
```

Just be careful what you sort by.

####  Pagination Warning

Distributed pagination in Riak Search cannot be used reliably when
sorting on fields that can have different values per replica of the same
object, namely: `score`, `_yz_id`. In the case of sorting by these
fields, you may receive redundant objects. In the case of `score`, the
top-N can return different results over multiple runs.

If you are paginating simply to get all keys that match and don't care
about the score, then you can sort on type-bucket-key (eg. `_yz_rt asc`,
`_yz_rb asc`, `_yz_rk asc`) to get consistent results.

If you want to sort by score without repeating results then you must set
`rows` >= `numFound`. This requires having some idea of how many rows
will match before running the query.

[This issue](https://github.com/basho/yokozuna/issues/355) is caused by
the way Search must minimally distribute a query across multiple Solr
nodes (called a *coverage plan*), and then filter duplicate results to
retrieve a full result set. Since this plan is frequently recalculated,
successive page queries may use a different plan, and thus calculate
alternate `score`s or filter different `_yz_id` values. We have plans to
fix this shortcoming in the next version of Riak.

## Riak Data Types and Search

Although [[Riak Data Types|Data Types]] function differently from other
Riak objects in some respects, when you're using Search you can think of
them as normal Riak objects with special metadata attached (metadata
that you don't need to worry about as a user). Riak's [[counters|Data
Types#Counters]], [[sets|Data Types#Sets]], and [[maps|Data Types#Maps]]
can be indexed and have their contents searched just like other Riak
objects.

### Data Type Content Types

Like all objects stored in Riak, Riak Data Types are assigned content
types. Unlike other Riak objects, this happens automatically. When you
store, say, a counter in Riak, it will automatically be assigned the
type `application/riak_counter`. The table below provides the full list
of content types:

Data Type | Content Type
:---------|:------------
Counters | `application/riak_counter`
Sets | `application/riak_set`
Maps | `application/riak_map`

When using Search, you won't need to worry about this, as Riak Data
Types are automatically indexed on the basis of these content types.

### Data Type Schemas

There are two types of schemas related to Riak Data Types:

* **Top-level schemas** relate to Data Types that are stored at the key
    level (counters and sets)
* **Embedded schemas** relate to Data Types nested inside of maps
    (flags, counters, registers, and sets)

As you can see from the [default Search
schema](https://github.com/basho/yokozuna/blob/develop/priv/default_schema.xml#L96),
each of the Data Types has its own default schema, with the exception of
maps, which means that the `_yz_default` schema will automatically index
Data Types on the basis of their assigned content type. This means that
there is no extra work involved in indexing Riak Data Types. You can
simply store them and begin querying, provided that they are properly
indexed, which is covered in the [[examples|Using
Search#Riak-Data-Types-and-Search]] section below.

As mentioned above, there are no default schemas available for maps.
This is because maps are essentially carriers for the other Data Types.
Even when maps are embedded within other maps, all of the data that you
might wish to index and search is contained in counters, sets,
registers, and flags.

The sections immediately below provide the default schemas for each Riak
Data Type. Because you will not need to manipulate these default schemas
to search Data Types, they are provided only for reference.

#### Top-level Schemas

The default schema for [[counters|Data Types#Counters]] indexes each
counter as an integer.

```xml
<field name="counter" type="int" indexed="true" stored="true" multiValued="false" />
```

Constructing queries for counters involves prefacing the query with
`counter`. Below are some examples:

Query | Syntax
:-----|:------
Counters with a value over 10 | `counter:[10 TO *]`
Counters with a value below 10 and above 50 | `counter:[* TO 10] AND counter:[50 TO *]`
Counters with a value of 15 | `counter:15`
All counters within the index | `counter:*`

The schema for [[sets|Data Types#Sets]] indexes each element of a set as
a string and indexes the set itself as multi-valued.

```xml
<field name="set" type="string" indexed="true" stored="false" multiValued="true" />
```

To query sets, preface the query with `set`. The table below shows some
examples:

Query | Syntax
:-----|:------
Sets that contain the value `apple` | `set:apple`
Sets that contain an item beginning with `level` | `set:level*`
Sets that contain both `apple` and `orange` | `set:apple AND set:orange`
All sets within the index | `set:*`

#### Embedded Schemas

For searching within [[maps|Data Types#Maps]], there are four schemas
for embedded, aka dynamic, fields. Flags are indexed as booleans:

```xml
<dynamicField name="*_flag" type="boolean" indexed="true" stored="true" multiValued="false" />
```

Counters, like their top-level counterparts, are indexed as integers:

```xml
<dynamicField name="*_counter" type="int" indexed="true" stored="true" multiValued="false" />
```

Registers are indexed as strings, but unlike sets they are not
multi-valued.

```xml
<dynamicField name="*_register" type="string" indexed="true" stored="true" multiValued="false" />
```

Finally, sets at the embedded level are indexed as multi-valued strings.

```xml
<dynamicField name="*_set" type="string" indexed="true" stored="true" multiValued="true" />
```

To query embedded fields, you must provide the name of the field. The
table below provides some examples:

Query | Syntax
:-----|:------
Maps containing a set called `hobbies` | `hobbies_set:*`
Maps containing a `score` counter over 50 | `score_counter:[50 TO *]`
Maps containing disabled `advanced` flags | `advanced_flag:false`
Maps containing enabled `advanced` flags and `score` counters under 10 | `advanced_flag:true AND score_counter:[* TO 10]`

You can also query maps within maps, which is covered in the **Querying
maps within maps** section below.

## Data Types and Search Examples

In this section, we'll start with two simple examples, one involving
counters and the other involving sets. Later on, we'll introduce a
slightly more complex map example.

### Counters Example

Let's say that we're storing scores in a multiplayer online game in
Riak. The game is called Boulderdash and it involves smashing digital
boulders armed with nothing but witty retorts and arcane trivia
knowledge. We'll create and activate a [[bucket type|Using Bucket
Types]] for [[storing counters|Using Data Types#Counters]] simply called
`counters`, like so:

```bash
riak-admin bucket-type create counters '{"props":{"datatype":"counter"}}'
riak-admin bucket-type activate counters
```

Now, we'll create a search index called `scores` that uses the default
schema (as in some of the examples above):

```java
YokozunaIndex scoresIndex = new YokozunaIndex("scores", "_yz_default");
StoreIndex storeIndex = new StoreIndex.Builder(scoresIndex)
        .build();
client.execute(storeIndex);
```

```ruby
client.create_search_index('scores', '_yz_default')
```

```python
client.create_search_index('scores', '_yz_default')
```

```erlang
riakc_pb_socket:create_search_index(Pid, <<"famous">>, <<"_yz_default">>, []).
```

```bash
curl -XPUT $RIAK_HOST/search/index/hobbies \
  -H 'Content-Type: application/json' \
  -d '{"schema":"_yz_default"}'
```

Now, we can modify our `counters` bucket type to associate that bucket
type with our `scores` index:

```bash
riak-admin bucket-type update counters '{"props":{"search_index":"scores"}}'
```

At this point, all of the counters that we stored in any bucket with the
bucket type `counters` will be indexed in our `scores` index. So let's
start playing with some counters. All counters will be stored in the
bucket `players`, while the key for each counter will be the username of
each player:

```java
Namespace peopleBucket = new Namespace("counters", "people");

Location christopherHitchensCounter = new Location(peopleBucket, "christ_hitchens");
CounterUpdate cu = new CounterUpdate(10);
UpdateCounter update = new UpdateCounter.Builder(christopherHitchensCounter, cu)
        .build();
client.execute(update);

Location joanRiversCounter = new Location(peopleBucket, "joan_rivers");
CounterUpdate cu = new CounterUpdate(25);
UpdateCounter update = new UpdateCounter.Builder(joanRiversCounter, cu)
        .build();
client.execute(update);
```

```ruby
bucket = client.bucket('people')

christopher_hitchens_counter = Riak::Crdt::Counter.new(bucket, 'chris_hitchens', 'counters')
christopher_hitchens_counter.increment(10)

joan_rivers_counter = Riak::Crdt::Counter.new(bucket, 'joan_rivers', 'counters')
joan_rivers_counter.increment(25)
```

```python
from riak.datatypes import Counter

bucket = client.bucket_type('counters').bucket('people')

christopher_hitchens_counter = Counter(bucket, 'chris_hitchens')
christopher_hitchens_counter.increment(10)
christopher_hitchens_counter.store()

joan_rivers_counter = Counter(bucket, 'joan_rivers')
joan_rivers_counter.increment(25)
joan_rivers_counter.store()
```

So now we have two counters, one with a value of 10 and the other with a
value of 25. Let's query to see how many counters have a value greater
than 20, just to be sure:

```java
String index = "scores";
String query = "counter:[20 TO *]";
SearchOperation searchOp = new SearchOperation.Builder(BinaryValue.create(index), query)
        .build();
cluster.execute(searchOp);
SearchOperation.Response results = searchOp.get();
```

```ruby
results = client.search('scores', 'counter:[20 TO *]')
# This should return a Hash with fields like 'num_found' and 'docs'

results['num_found']
# 2
```

```python
results = client.fulltext_search('scores', 'counter:[20 TO *]')
# This should return a dict with fields like 'num_found' and 'docs'

results['num_found']
# 1
```

And there we are: only one of our two stored sets has a value over 20.
To find out which set that is, we can dig into our results:

```java
// Using the "results" object from above:
int numberFound = results.numResults();
Map<String, List<String>> foundObject = results.getAllResults().get(0);
String key = foundObject.get("_yz_rk").get(0); // "joan_rivers"
String bucket = foundObject.get("_yz_rb").get(0); // "people"
String bucketType = foundObject.get("_yz_rt").get(0); // "counters"
```

```ruby
doc = results['docs'][0]

# The key
doc['_yz_rk'] # 'joan_rivers'

# The bucket
doc['_yz_rb'] # 'people'

# The bucket type
doc['_yz_rt'] # 'counters'
```

```python
doc = results['docs'][0]

# The key
doc['_yz_rk'] # 'joan_rivers'

# The bucket
doc['_yz_rb'] # 'people'

# The bucket type
doc['_yz_rt'] # 'counters'
```

Alternatively, we can see how many counters have values below 15:

```java
String index = "scores";
String query = "counter:[* TO 15]";
SearchOperation searchOp = new SearchOperation
        .Builder(BinaryValue.create("scores"), "counter:[* TO 15]")
        .build();
cluster.execute(searchOp);
SearchOperation.Response results = searchOp.get();
```

```ruby
results = client.search('scores', 'counter:[* TO 15]')
```

```python
results = client.fulltext_search('scores', 'counter:[* TO 15]')
```

Or we can see how many counters have a value of 17 exactly:

```java
// Using the same method as above, just changing the query:
String query = "counter:17";
```

```ruby
results = client.search('scores', 'counter:17')
```

```python
results = client.fulltext_search('scores', 'counter:17')
```

### Sets Example

Let's say that we're storing information about the hobbies of a group of
people in sets. We'll create and activate a [[bucket type|Using Bucket
Types]] for [[storing sets|Using Data Types#Sets]] simply called `sets`,
like so:

```bash
riak-admin bucket-type create sets '{"props":{"datatype":"set"}}'
riak-admin bucket-type activate sets
```

Now, we'll create a Search index called `hobbies` that uses the default
schema (as in some of the examples above):

```java
YokozunaIndex hobbiesIndex = new YokozunaIndex("hobbies");
StoreIndex storeIndex =
  new StoreIndex.Builder(hobbiesIndex).build();
client.execute(storeIndex);
```

```ruby
client.create_search_index('hobbies', '_yz_default')
```

```bash
curl -XPUT $RIAK_HOST/search/index/hobbies \
  -H 'Content-Type: application/json' \
  -d '{"schema": "_yz_default"}'
```

Now, we can modify our `sets` bucket type to associate that bucket type
with our `hobbies` index:

```bash
riak-admin bucket-type update sets '{"props":{"search_index":"hobbies"}}'
```

Now, all of the sets that we store in any bucket with the bucket type
`sets` will be automatically indexed as a set. So let's say that we
store three sets for two different people describing their respective
hobbies, in the bucket `people`:

```java
Namespace peopleBucket = new Namespace("sets", "people");

Location mikeDitkaSet = new Location(peopleBucket, "ditka");
SetUpdate su1 = new SetUpdate()
        .add("football")
        .add("winning");
UpdateSet update1 = new UpdateSet.Builder(mikeDitkaSet, su1).build();

Location ronnieJamesDioSet = new Location(peopleBucket, "dio");
SetUpdate su2 = new SetUpdate()
        .add("wailing")
        .add("rocking")
        .add("winning");
UpdateSet update2 = new UpdateSet.Builder(ronnieJamesDioSet, su2).build();

client.execute(update1);
client.execute(update2);
```

```ruby
bucket = client.bucket('people')

mike_ditka_set = Riak::Crdt::Set.new(bucket, 'ditka', 'sets')
mike_ditka_set.add('football')
mike_ditka_set.add('winning')

ronnie_james_dio_set = Riak::Crdt::Set.new(bucket, 'dio', 'sets')
ronnie_james_dio_set.add('wailing')
ronnie_james_dio_set.add('rocking')
ronnie_james_dio_set.add('winning')
```

```python
from riak.datatypes import Set

bucket = client.bucket_type('sets').bucket('people')

mike_ditka_set = Set(bucket, 'ditka')
mike_ditka_set.add('football')
mike_ditka_set.add('winning')
mike_ditka_set.store()

ronnie_james_dio_set = Set(bucket, 'dio')
ronnie_james_dio_set.add('wailing')
ronnie_james_dio_set.add('rocking')
ronnie_james_dio_set.add('winning')
ronnie_james_dio_set.store()
```

Now, we can query our `hobbies` index to see if anyone has the hobby
`football`:

```java
// Using the same method explained above, just changing the query:
String query = "set:football";
```

```ruby
results = client.search('hobbies', 'set:football')
# This should return a dict with fields like 'num_found' and 'docs'
```

```python
results = client.fulltext_search('hobbies', 'set:football')
# This should return a dict with fields like 'num_found' and 'docs'
```

Let's see how many sets contain the element `football`:

```java
// Using the same method explained above for getting search results:
int numberFound = results.numResults(); // 1
```

```ruby
results['num_found']
# 1
```

```python
results['num_found']
# 1
```

Success! We stored two sets, only one of which contains the element
`football`. Now, let's see how many sets contain the element `winning`:

```java
// Using the same method explained above, just changing the query:
String query = "set:winning";

// Again using the same method from above:
int numberFound = results.numResults(); // 2
```

```ruby
results = client.search('hobbies', 'set:winning')
results['num_found']
# 2
```

```python
results = client.fulltext_search('hobbies', 'set:winning')
results['num_found']
# 2
```

Just as expected, both sets we stored contain the element `winning`.

### Maps Example

This example will build on the example in the [[Using Data Types]]
tutorial. That tutorial walks you through storing CMS-style user data in
Riak [[maps|Using Data Types#Maps]], and we'd suggest that you
familiarize yourself with that tutorial first. More specifically, user
data is stored in the following fields in each users's map:

* first name in a `first_name` register
* last name in a `last_name` register
* whether the user is an enterprise customer in an `enterprise_customer`
  flag
* the number of times the user has visited the company page in a
  `page_visits` counter
* a list of the user's interests in an `interests` set

First, let's create and activate a bucket type simply called `maps` that
is set up to store Riak maps:

```bash
riak-admin bucket-type create maps '{"props":{"datatype":"map"}}'
riak-admin bucket-type activate maps
```

Now, let's create a search index called `customers` using the default
schema:

```java
YokozunaIndex customersIndex = new YokozunaIndex("customers", "_yz_default");
StoreIndex storeIndex =
  new StoreIndex.Builder(customersIndex).build();
client.execute(storeIndex);
```

```ruby
client.create_search_index('customers', '_yz_default')
```

```python
client.create_search_index('customers', '_yz_default')
```

```curl
curl -XPUT $RIAK_HOST/search/index/customers \
  -H 'Content-Type: application/json' \
  -d '{"schema":"_yz_default"}'
```

With our index created, we can associate our new `customers` index with
our `maps` bucket type:

```bash
riak-admin bucket-type update maps '{"props":{"search_index":"customers"}}'
```

Now we can create some maps along the lines suggested above:

```java
Namespace customersBucket = new Namespace("maps", "customers");

Location idrisElbaMap = new Location(customersBucket, "idris_elba");
MapUpdate mu = new MapUpdate()
        .update("first_name", new RegisterUpdate("Idris"))
        .update("last_name", new RegisterUpdate("Elba"))
        .update("enterprise_customer", new FlagUpdate(false))
        .update("page_visits", new CounterUpdate(10))
        .update("interests", new SetUpdate().add("acting", "being Stringer Bell"));

Location joanJettMap = new Location(customersBucket, "joan_jett");
MapUpdate mu2 = new MapUpdate()
        .update("first_name", new RegisterUpdate("Joan"))
        .update("last_name", new RegisterUpdate("Jett"))
        // Joan Jett is not an enterprise customer, so we don't need to
        // explicitly disable the "enterprise_customer" flag, as all
        // flags are disabled by default
        .update("page_visits", new CounterUpdate(25))
        .update("interests", new SetUpdate().add("loving rock and roll").add("being in the Blackhearts"));

UpdateMap update1 = new UpdateMap.Builder(idrisElbaMap, mu1).build();
UpdateMap update2 = new UpdateMap.Builder(joanJettMap, mu2).build();
client.execute(update1);
client.execute(update2);
```

```ruby
bucket = client.bucket('customers')

idris_elba = Riak::Crdt::Map.new(bucket, 'idris_elba', 'maps')

idris_elba.batch do |ie|
  ie.registers['first_name'] = 'Idris'
  ie.registers['last_name'] = 'Elba'
  ie.flags['enterprise_customer'] = true
  ie.counters['page_visits'].increment(10)
  ['acting', 'being Stringer Bell'].each do |interest|
    ie.sets['interests'].add(interest)
  end
end

joan_jett = Riak::Crdt::Map.new(bucket, 'joan_jett', 'maps')
joan_jett.batch do |jj|
  jj.registers['first_name'] = 'Joan'
  jj.registers['last_name'] = 'Jett'
  ## Joan Jett is not an enterprise customers, so we don't need to
  ## explicitly disable this flag, as all flags are disabled by default
  jj.counters['page_visits'].increment(10)
  ['loving rock and roll', 'being in the Blackhearts'].each do |interest|
    jj.sets['interests'].add(interest)
  end
end
```

```python
bucket = client.bucket_type('maps').bucket('customers')

idris_elba = Map(bucket, 'idris_elba')
idris_elba.registers['first_name'].assign('Idris')
idris_elba.registers['last_name'].assign('Elba')
idris_elba.flags['enterprise_customer'].enable()
idris_elba.counters['page_visits'].increment(10)
for interest in ['acting', 'being Stringer Bell']:
    idris_elba.sets['interests'].add(interest)
idris_elba.store()

joan_jett = Map(bucket, 'joan_jett')
joan_jett.registers['first_name'].assign('Joan')
joan_jett.registers['last_name'].assign('Jett')
# Joan Jett is not an enterprise customers, so we don't need to
# explictly disable this flag, as all flags are disabled by default
idris_elba.counters['page_visits'].increment(25)
for interest in ['loving rock and roll', 'being in the Blackhearts']:
    joan_jett.sets['interests'].add(interest)
joan_jett.store()
```

#### Searching counters within maps

We now have two maps stored in Riak that we can query. Let's query to
see how many users have page visit counters above 15. Unlike the
counters example above, we have to specify _which_ counter we're
querying:

```java
// Using the same method explained above, just changing the query:
String query = "page_visits_counter:[15 TO *]";

// Again using the same method from above:
int numberFound = results.numResults(); // 1
```

```ruby
results = client.search('customers', 'page_visits_counter:[15 TO *]')
results['num_found']
# 1
```

```python
results = client.fulltext_search('customers', 'page_visits_counter:[15 TO *]')
results['num_found']
# 1
```

As expected, one of our two stored maps has a `page_visits` counter
above 15. Let's make sure that we have the right result:

```java
// Using the same method from above:
String query = "page_visits_counter:[15 TO *]";

// Again using the same method from above:
String registerValue =
  results.getAllResults().get(0).get("first_name_register").get(0); // Joan
```

```ruby
results['docs'][0]['first_name_register']
# 'Joan'
```

```python
results['docs'][0]['first_name_register']
# u'Joan'
```

Success! Now we can test out searching sets.

#### Searching sets within maps

Each of the maps we stored thus far had an `interests` set. First, let's
see how many of our maps even _have_ sets called `interests` using a
wildcard query:

```java
// Using the same method from above:
String query = "interests_set:*";
```

```ruby
results = client.search('customers', 'interests_set:*')
# 2
```

```python
results = client.fulltext_search('customers', 'interests_set:*')
results['num_found']
# 2
```

As expected, both stored maps have an `interests` set. Now let's see how
many maps have items in `interests` sets that begin with `loving`:

```java
// Using the same method from above:
String query = "interests_set:loving*";

// Again using the same method from above:
int numberFound = results.numResults(); // 1
String registerValue =
  results.getAllResults().get(0).get("first_name_register").get(0); // Joan
```

```ruby
results = client.search('customers', 'interests_set:loving*')
results['num_found'] # 1
results['docs'][0]['first_name_register'] # 'Joan'
```

```python
results = client.fulltext_search('customers', 'interests_set:loving*')
results['num_found'] # 1
results['docs'][0]['first_name_register'] # u'Joan'
```

As expected, only our Joan Jett map has one item in its `interests` set
that starts with `loving`.

#### Searching maps within maps

Before we can try to search maps within maps, we need to actually store
some. Let's add a `alter_ego` map to both of the maps we've stored thus
far. Each person's alter ego will have a first name only.

```java
Location idrisElbaMap = new Location(customersBucket, "idris_elba");
MapUpdate alterEgoUpdate = new MapUpdate()
        .update("name", new RegisterUpdate("John Luther"));
UpdateMap addSubMap = new UpdateMap.Builder(idrisElbaMap, alterEgoUpdate);
client.execute(addSubMap);
```

```ruby
idris_elba.maps['alter_ego'].registers['name'] = 'John Luther'

joan_jett.maps['alter_ego'].registers['name'] = 'Robert Plant'
```

```python
idris_elba.maps['alter_ego'].registers['name'].assign('John Luther')
idris_elba.store()

joan_jett.maps['alter_ego'].registers['name'].assign('Robert Plant')
joan_jett.store()
```

Querying maps within maps involves construct queries that separate the
different levels of depth with a single dot. Here's an example query for
finding maps that have a `name` register embedded within an `alter_ego`
map:

```java
// Using the same method from above:
String query = "alter_ego_map.name_register:*";

// Again using the same method from above:
int numberFound = results.numResults(); // 1
```

```ruby
results = client.search('customers', 'alter_ego_map.name_register:*')
results['num_found'] # 1
```

```python
results = client.fulltext_search('customers', 'alter_ego_map.name_register:*')
results['num_found'] # 1
```

Once we know how to query embedded fields like this, we can query those
just like any other. Let's find out which maps have an `alter_ego`
sub-map that contains a `name` register that ends with `PLant`, and
display that customer's first name:

```java
// Using the same method from above:
String query = "alter_ego_map.name_register:*Plant";

// Again using the same method from above:
int numberFound = results.numResults(); // 1
String registerValue =
  results.getAllResults().get(0).get("first_name_register").get(0); // Joan
```

```ruby
results = client.search('customers', 'alter_ego_map.name_register:*Plant')
results['num_found'] # 1
results['docs'][0]['first_name_register'] # 'Joan'
```

```python
results = client.fulltext_search('customers', 'alter_ego_map.name_register:*Plant')
results['num_found'] # 1
results['docs'][0]['first_name_register'] # u'Joan
```

Success! We've now queried not just maps but also maps within maps.

### MapReduce

Riak Search allows for piping search results as inputs for
[[MapReduce|Using MapReduce]] jobs. This is a useful crossection for
performing post-calculations of results or aggregations of ad-hoc
queries. The Riak Search MapReduce integration works similarly to
regular MapReduce, with the notable exception that your input is not a
bucket, but rather index and query arguments to the `yokozuna` module
and `mapred_search` function (an Erlang `module:function` pair that adds
the Riak Search hook to MapReduce).

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

In this example we're searching for all famous cats that are not
leaders and counting up the results using Javascript for both map and
reduce. It should return the reduced sum of `[3]`.

```curl
curl -XPOST $RIAK_HOST/mapred \
  -H 'Content-Type: application/json' \
  -d '{"inputs":{"module":"yokozuna","function":"mapred_search","arg":["famous","NOT leader_b:true"]},"query":[{"map":{"language":"javascript","keep":false,"source":"function(v) { return [1]; }"}},{"reduce":{"language":"javascript","keep":true,"name":"Riak.reduceSum"}}]}'
```

<!--
#### Functions

Cats age about 7 times faster than humans, so to calculate an age in
"cat years", you can multiply the cat's age by 7 using the `product`
function. It's only one out of many built-in [Solr
Functions](https://cwiki.apache.org/confluence/display/solr/Function+Queries#FunctionQueries-AvailableFunctions).

```curl
curl "$RIAK_HOST/search/query/famous?wt=json&q=*:*&fl=_yz_rk,age_i:product(age_i,7)" | jsonpp
```
-->

## Feature List

Riak Search 2.0 is more than a distributed search engine like
[SolrCloud](https://cwiki.apache.org/confluence/display/solr/SolrCloud)
or [ElasticSearch](http://www.elasticsearch.org/). It's a searchable
integration with Riak. This greatly simplifies usage by offloading the
task of indexing values to Riak.

Riak Search's features and enhancements are numerous.

* Support for various MIME types (JSON, XML, plain text, [[Riak Data
  Types|Using Data Types]]) for automatic data extraction
* Support for [various
  language](https://cwiki.apache.org/confluence/display/solr/Language+Analysis)-specific
  [analyzers, tokenizers, and
  filters](https://cwiki.apache.org/confluence/display/solr/Understanding+Analyzers%2C+Tokenizers%2C+and+Filters)
* Robust, easy-to-use [query languages](https://cwiki.apache.org/confluence/display/solr/Other+Parsers)
  like Lucene (default) and DisMax.
* Queries: exact match, globs, inclusive/exclusive range queries,
  AND/OR/NOT, prefix matching, proximity searches, term boosting,
  sorting, pagination
* Protocol Buffer interface and Solr interface via HTTP
* Scoring and ranking for most relevant results
* Query result [highlighting](https://cwiki.apache.org/confluence/display/solr/Highlighting)
* Search queries as input for MapReduce jobs
* [[Active Anti-Entropy]] for automatic index repair
