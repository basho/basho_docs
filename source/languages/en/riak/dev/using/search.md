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
YokozunaIndex famousIndex = new YokozunaIndex("famous");
StoreSearchIndex storeSearchIndex =
    new StoreSearchIndex.Builder(famousIndex).build();
client.execute(StoreSearchIndex);
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
StoreSearchIndex storeSearchIndex = new StoreSearchIndex.Builder(famousIndex)
        .build();
client.execute(StoreSearchIndex);
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
SearchOperation searchOp =
    new SearchOperation.Builder(BinaryValue.create("famous"), "name_s:Lion*").build();
cluster.execute(searchOp);
searchOp.await();
Map<String, String> results = searchOp.get().getAllResults();
List<Map<String, String>> docs = results.get("docs");
System.out.println(docs);
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

```curl
curl "$RIAK_HOST/search/query/famous?wt=json&q=age_i:%5B30%20TO%20*%5D" | jsonpp
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

<!-- TODO: pubdate:[NOW-1YEAR/DAY TO NOW/DAY+1DAY] -->

#### Boolean

You can perform logical conjunctive, disjunctive, and negative
operations on query elements as, repectively, `AND`, `OR`, and `NOT`.
Let's say we want to see who is capable of being a US Senator (at least
30 years old, and a leader). It requires a conjunctive query:
`leader_b:true AND age_i:[25 TO *]`.

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

```curl
curl -XDELETE $RIAK_HOST/search/index/famous
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

##### A Pagination Warning

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
  [analyzers, tokenizers, and filters](https://cwiki.apache.org/confluence/display/solr/Overview+of+Analyzers%2C+Tokenizers%2C+and+Filters)
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
