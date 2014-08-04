---
title: Search Schema
project: riak
version: 0.14.0+
document: cookbook
toc: true
audience: intermediate
keywords: [search, schema]
moved: {
  '1.4.0-': '/cookbooks/Riak-Search---Schema'
}
---

<div class="info">This document refers to the new Riak Search 2.0 with [[Solr|http://lucene.apache.org/solr/]] integration (codenamed Yokozuna). For information about the deprecated Riak Search Schema, visit [[the old Riak Search Schema|http://docs.basho.com/riak/1.4.8/dev/advanced/search-schema/]].</div>

Riak Search is built for ease-of-use, namely, the philosophy that you write values into Riak, and you query for values using Solr. Riak Search does a lot of work under the covers to convert your values (plain text, JSON, XML, datatypes) into something that can be indexed and searched later. However, you still have to explain to Riak/Solr how to index a value. Are you providing an array of strings? Or an integer? Or a date? Is your text in English or Russian? The way you explain to Riak Search how a value is to be indexed is by defining a **Solr schema**.

## Setting a Schema

If you just want to get started quickly, and already know all about creating your own Riak Search schema (it's similar, but not exactly the same as a standard Solr schema, so read on).

Here's how you can create a custom schema named `cartoons`, where the schema xml data is stored in a file named `cartoons.xml`

```curl
curl -XPUT "http://localhost:8098/search/schema/cartoons" \
  -H'content-type:application/xml' \
  --data-binary @cartoons.xml
```
```ruby
schema_data = File.read("cartoons.xml")
client.create_search_schema("cartoons", schema_data)
```
```python
xml_file = open('cartoons.xml', 'r')
schema_data = xml_file.read()
client.create_search_schema('cartoons', schema_data)
xml_file.close()
```
```erlang
{ok, SchemaData} = file:read_file("cartoons.xml"),
riakc_pb_socket:create_search_schema(Pid, <<"cartoons">>, SchemaData).
```


## Creating a Custom Schema

The first step in creating a custom schema is to define exactly what fields you must index. Part of that step is understanding how Riak Search extractors function.

### Extractors

The extractors of Riak Search are modules responsible for pulling out a list of fields and values from a Riak object. How this is achieved depends on the object's content type, but the two common cases are JSON and XML, which operate similarly, so our examples will use JSON.

The following JSON object represents the character [Lion-o](http://en.wikipedia.org/wiki/List_of_ThunderCats_characters#Lion-O) from the Thundercats. He has a name, age, is the team leader, and has a list of aliases in other languages.

```json
{
  "name":"Lion-o",
  "age":30,
  "leader":true,
  "aliases":[
    {"name":"León-O", "desc_es":"Señor de los ThunderCats"},
    {"name":"Starlion", "desc_fr":"Le jeune seigneur des Cosmocats"},
  ]
}
```

The extractor will flatten the above objects into a list of field/value pairs. Nested objects will be seperated with a dot (`.`) and arrays will simply repeat the fields. The above object will be extracted to the following list of Solr document fields.

```
name=Lion-o
age=30
leader=true
aliases.name=León-O
aliases.desc_es=Señor de los ThunderCats
aliases.name=Starlion
aliases.desc_fr=Le jeune seigneur des Cosmocats
```

This means that our schema should handle `name`, `age`, `leader`, `aliases.name` (a `dot` is a valid field character), and `aliases.desc_*` which is a description in the given language of the suffix (Spanish and French).

### Required Schema Fields

Solr schemas can be very complex with many types and analyzers. Refer to the [Solr 4.7 reference guide](http://archive.apache.org/dist/lucene/solr/ref-guide/apache-solr-ref-guide-4.7.pdf) for a complete list. But Riak Search requires a few fields in order to properly distribute an object across a cluster. These fields are all prefixed with `_yz`, which stands for *Yokozuna*, the project name that makes Riak Search function.

Here is a bare minimum skeleton Solr Schema. It won't do much for you other than allow Riak Search to properly manage your stored objects.

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<schema name="schedule" version="1.5">
 <fields>

   <!-- All of these fields are required by Riak Search -->
   <field name="_yz_id"   type="_yz_str" indexed="true" stored="true"  multiValued="false" required="true"/>
   <field name="_yz_ed"   type="_yz_str" indexed="true" stored="false" multiValued="false"/>
   <field name="_yz_pn"   type="_yz_str" indexed="true" stored="false" multiValued="false"/>
   <field name="_yz_fpn"  type="_yz_str" indexed="true" stored="false" multiValued="false"/>
   <field name="_yz_vtag" type="_yz_str" indexed="true" stored="false" multiValued="false"/>
   <field name="_yz_rk"   type="_yz_str" indexed="true" stored="true"  multiValued="false"/>
   <field name="_yz_rt"   type="_yz_str" indexed="true" stored="true"  multiValued="false"/>
   <field name="_yz_rb"   type="_yz_str" indexed="true" stored="true"  multiValued="false"/>
   <field name="_yz_err"  type="_yz_str" indexed="true" stored="false" multiValued="false"/>
 </fields>

 <uniqueKey>_yz_id</uniqueKey>

 <types>
    <!-- YZ String: Used for non-analyzed fields -->
    <fieldType name="_yz_str" class="solr.StrField" sortMissingLast="true" />
 </types>
</schema>
```

If you're missing any of the above fields, Riak Search will reject your
custom schema. The value for `<uniqueKey>` *must* be `_yz_id`.

In the table below, you'll find a description of the various required
fields. You'll rarely need to use any fields other than `_yz_rt` (bucket
type), `_yz_rb` (bucket) and `_yz_rk` (Riak key). On occasion `_yz_err`
can be helpful if you suspect that your extractors are failing.
Malformed JSON or XML will cause Riak Search to index a key and set
`_yz_err` to 1, allowing you to reindex with proper values later.

Field   | Name |Description
--------|------|-----
`_yz_id`  | ID   | Unique identifier of this Solr document
`_yz_ed`  | Entropy Data | Data related to anti-entropy
`_yz_pn`  | Partition Number | Used as a filter query param to remove duplicate replicas across nodes
`_yz_fpn` | First Partition Number | The first partition in this doc's preflist, used for further filtering on overlapping partitions
`_yz_vtag`| VTag | If there is a sibling, use vtag to differentiate them
`_yz_rk`  | Riak Key | The key of the Riak object this doc corresponds to
`_yz_rt`  | Riak Bucket Type | The bucket type of the Riak object this doc corresponds to
`_yz_rb`  | Riak Bucket | The bucket of the Riak object this doc corresponds to
`_yz_err` | Error Flag | indicating if this doc is the product of a failed object extraction

### Defining Fields

With your required fields known, and the skeleton schema elements in place, it's time to add your own fields. Since you know your object structure, you need to map the name and type of each field (a string, integer, boolean, etc).

When creating fields you can either create specific fields via the `field` element, or an asterisk (`*`) wildcard field via `dynamicField`. Any field that matches a specific field name will win, and if not, it will attempt to match a dynamic field pattern.

Besides a field `type`, you also must decide if a value is to be `indexed` (usually `true`) and `stored`. When a value is `stored` that means that you can get the value back as a result of a query, but it also doubles the storage of the field (once in Riak, again in Solr). If a single Riak object can have more than one copy of the same matching field, you also must set `multiValued` to `true`.

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<schema name="schedule" version="1.0">
 <fields>
   <field name="name"   type="string"  indexed="true" stored="true" />
   <field name="age"    type="int"     indexed="true" stored="false" />
   <field name="leader" type="boolean" indexed="true" stored="false" />
   <field name="aliases.name" type="string" indexed="true" stored="true" multiValued="true" />
   <dynamicField name="*_es" type="text_es" indexed="true" stored="true" multiValued="true" />
   <dynamicField name="*_fr" type="text_fr" indexed="true" stored="true" multiValued="true" />

   <!-- All of these fields are required by Riak Search -->
   <field name="_yz_id"   type="_yz_str" indexed="true" stored="true"  multiValued="false" required="true"/>
   <field name="_yz_ed"   type="_yz_str" indexed="true" stored="false" multiValued="false"/>
   <field name="_yz_pn"   type="_yz_str" indexed="true" stored="false" multiValued="false"/>
   <field name="_yz_fpn"  type="_yz_str" indexed="true" stored="false" multiValued="false"/>
   <field name="_yz_vtag" type="_yz_str" indexed="true" stored="false" multiValued="false"/>
   <field name="_yz_rk"   type="_yz_str" indexed="true" stored="true"  multiValued="false"/>
   <field name="_yz_rt"   type="_yz_str" indexed="true" stored="true"  multiValued="false"/>
   <field name="_yz_rb"   type="_yz_str" indexed="true" stored="true"  multiValued="false"/>
   <field name="_yz_err"  type="_yz_str" indexed="true" stored="false" multiValued="false"/>
 </fields>

 <uniqueKey>_yz_id</uniqueKey>
```

Next take note of the types you used in the fields, and ensure that each of the field types are defined as a `fieldType` under the `types` element. Basic types such as `string`, `boolean`, `int` have matching Solr classes. There are dozens more types, including many kinds of number (`float`, `tdouble`, `random`), `date` fields, and even geolocation types.

Besides simple field types, you can also customize analyzers for different languages. In our example, we mapped any field that ends with `*_es` to Spanish, and `*_de` to German.

```xml
 <types>
   <!-- YZ String: Used for non-analyzed fields -->
   <fieldType name="_yz_str" class="solr.StrField" sortMissingLast="true" />

   <fieldType name="string" class="solr.StrField" sortMissingLast="true" />
   <fieldType name="boolean" class="solr.BoolField" sortMissingLast="true"/>
   <fieldType name="int" class="solr.TrieIntField" precisionStep="0" positionIncrementGap="0"/>

   <!-- Spanish -->
   <fieldType name="text_es" class="solr.TextField" positionIncrementGap="100">
     <analyzer>
       <tokenizer class="solr.StandardTokenizerFactory"/>
       <filter class="solr.LowerCaseFilterFactory"/>
       <filter class="solr.StopFilterFactory" ignoreCase="true" words="lang/stopwords_es.txt" format="snowball" />
       <filter class="solr.SpanishLightStemFilterFactory"/>
       <!-- more aggressive: <filter class="solr.SnowballPorterFilterFactory" language="Spanish"/> -->
     </analyzer>
   </fieldType>

   <!-- German -->
   <fieldType name="text_de" class="solr.TextField" positionIncrementGap="100">
     <analyzer>
       <tokenizer class="solr.StandardTokenizerFactory"/>
       <filter class="solr.LowerCaseFilterFactory"/>
       <filter class="solr.StopFilterFactory" ignoreCase="true" words="lang/stopwords_de.txt" format="snowball" />
       <filter class="solr.GermanNormalizationFilterFactory"/>
       <filter class="solr.GermanLightStemFilterFactory"/>
       <!-- less aggressive: <filter class="solr.GermanMinimalStemFilterFactory"/> -->
       <!-- more aggressive: <filter class="solr.SnowballPorterFilterFactory" language="German2"/> -->
     </analyzer>
   </fieldType>
 </types>
</schema>
```

### "Catch-All" Field

Without a catch-all field, an exception will be thrown if data is
provided to index without a corresponding `<field>` element. The
following is the catch-all field from the default Yokozuna schema and
can be used in a custom schema as well.

```xml
<dynamicField name="*" type="ignored"  />
```

The following is required to be a child of the `types` element in the schema:

```xml
<fieldtype name="ignored" stored="false" indexed="false" multiValued="true" class="solr.StrField" />
```

### Dates

The format of strings that represents a date/time is important as [Solr
only understands ISO8601 UTC date/time
values](http://lucene.apache.org/solr/4_6_1/solr-core/org/apache/solr/schema/DateField.html).
An example of a correctly formatted date/time string is
`1995-12-31T23:59:59Z`. If you provide an incorrectly formatted
date/time value, an exception similar to this will be logged to `solr.log`:

```log
2014-02-27 21:30:00,372 [ERROR] <qtp1481681868-421>@SolrException.java:108 org.apache.solr.common.SolrException: Invalid Date String:'Thu Feb 27 21:29:59 +0000 2014'
        at org.apache.solr.schema.DateField.parseMath(DateField.java:182)
        at org.apache.solr.schema.TrieField.createField(TrieField.java:611)
        at org.apache.solr.schema.TrieField.createFields(TrieField.java:650)
        at org.apache.solr.schema.TrieDateField.createFields(TrieDateField.java:157)
        at org.apache.solr.update.DocumentBuilder.addField(DocumentBuilder.java:47)
        ...
        ...
        ...
```

## Field Properties By Use-Case

Sometimes it can be tricky to decide whether a value should be `stored`, or whether `multiValued` is allowed. This handy table from the [Solr documentation](https://cwiki.apache.org/confluence/display/solr/Field+Properties+by+Use+Case) may help you pick field properties.

An entry of `true` or `false` in the table indicates that the option must be set to the given value for the use case to function correctly. If no entry is provided, the setting of that attribute has no impact on the case.

<table class=schemausecase>
<thead>
<tr>
<th>Use Case</th>
<th><code>indexed</code></th>
<th><code>stored</code></th>
<th><code>multiValued</code></th>
<th><code>omitNorms</code></th>
<th><code>termVectors</code></th>
<th><code>termPositions</code></th>
</tr>
</thead>
<tbody>
<tr>
<td>search within field</td>
<td><code>true</code></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>retrieve contents</td>
<td></td>
<td><code>true</code></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>use as unique key</td>
<td><code>true</code></td>
<td></td>
<td><code>false</code></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>sort on field</td>
<td><code>true</code></td>
<td></td>
<td><code>false</code></td>
<td><code>true</code>[[1|Search Schema#notes]]</td>
<td></td>
<td></td>
</tr>
<tr>
<td>use field boosts[[5|Search Schema#notes]]</a></td>
<td></td>
<td></td>
<td></td>
<td><code>false</code></td>
<td></td>
<td></td>
</tr>
<tr>
<td>document boosts affect searches within field</td>
<td></td>
<td></td>
<td></td>
<td><code>false</code></td>
<td></td>
<td></td>
</tr>
<tr>
<td>highlighting</td>
<td><code>true</code>[[4|Search Schema#notes]]</td>
<td><code>true</code></td>
<td></td>
<td></td>
<td>[[2|Search Schema#notes]]</td>
<td><code>true</code>[[3|Search Schema#notes]]</td>
</tr>
<tr>
<td>faceting[[5|Search Schema#notes]]</td>
<td><code>true</code></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>add multiple values, maintaining order</td>
<td></td>
<td></td>
<td><code>true</code></td>
<td></td>
<td></td>
<td></td>
</tr>
<tr>
<td>field length affects doc score</td>
<td></td>
<td></td>
<td></td>
<td><code>false</code></td>
<td></td>
<td></td>
</tr>
<tr>
<td>MoreLikeThis[[5|Search Schema#notes]]</td>
<td></td>
<td></td>
<td></td>
<td></td>
<td><code>true</code>[[6|Search Schema#notes]]</td>
<td></td>
</tr>
</tbody></table>

### <a name="notes"></a>Notes

1. <a name="s1"></a>Recommended but not necessary.
2. <a name="s2"></a>Will be used if present, but not necessary.
3. <a name="s3"></a>(if `termVectors`=`true`)
4. <a name="s4"></a>A tokenizer must be defined for the field, but it doesn't need to be indexed.
5. <a name="s5"></a>Described in Understanding Analyzers, Tokenizers, and Filters.
6. <a name="s6"></a>Term vectors are not mandatory here. If not true, then a stored field is analyzed. So term vectors are recommended, but only required if stored=false.


## The Default Schema

Riak Search comes bundled with a default schema named `_yz_default`. It defaults to many dynamic field types, where the suffix defines its type. This is an easy path to start development, but we recommend in production that you define your own schema. Take special note of `dynamicField name="*"`, which is a catchall index for any value. Sufficiently sized objects can potentially take up tremendous disk space.

You can find the [yokozuna default solr schema](https://raw.github.com/basho/yokozuna/develop/priv/default_schema.xml) on github.
