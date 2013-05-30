---
title: Indexing and Querying KV Data
project: riak
version: 0.14.0+
document: cookbook
toc: true
audience: intermediate
keywords: [search, 2i]
---

Riak Search now supports indexing and querying of data stored in Riak KV.  Out of the box, simple indexing of plain text, XML, and JSON data can be enabled in an instant.

<div class="info">
<div class="title">Riak Search and MapReduce</div>
Riak Search isn't just able to index data from Riak KV, it can also be used to [[feed data into Riak's MapReduce|Riak-Search---Querying#Querying Integrated with Map/Reduce]].
</div>

## Setting up Indexing

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

## Datatypes

Riak Search is able to handle several standard data encodings with zero configuration.  Simply set the Content-Type metadata on your objects to the appropriate mime-type.  Out of the box, XML, JSON, and plain-text encodings are supported.

## JSON Encoded Data

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

## XML Encoded Data

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

## Erlang Data

If your object contains Erlang terms, you can set your Content-Type to "application/x-erlang". This expects either an Erlang term that is a proplist or a nested proplist. In the case of a proplist, the key is used as the field name, and the value as the field value. When the object's value is a nested proplist, field names are constructed by concatenating the nested keys together with underscores in between.

## Plain-text Data

If your data is plain text, set your Content-Type to "text/plain". The plain-text decoder is also used if no Content-Type is found.

Specifying that your data is in plain-text format will cause Riak Search to index all of the text in the object's value under a single field, named "value".  Queries can be explicit about searching this field, as in "value:seven AND value:score", or omit the default field name, as in "seven AND score".

## Other Data Encodings

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

## Field Types

If you read the "Other Data Encodings" section about writing your own encoder, you may have been surprised to find that all fields should be extracted as strings.  The reason for this is that it's the schema's job to say what the types of the fields are.

If you do not specify a schema, the default will be used.  The default schema indexes all fields as string values, unless they end in "_num" or "_dt", or any of the other dynamic fields defined in the Schema documentation.

You may define your own schema for your KV indexes, in the same manner as you would define a schema for non-KV indexes.  Just make sure the field names match those produced by the extractor in use.
