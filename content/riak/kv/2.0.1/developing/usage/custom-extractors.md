---
title: "Custom Extractors"
description: ""
project: "riak_kv"
project_version: "2.0.1"
menu:
  riak_kv-2.0.1:
    name: "Custom Extractors"
    identifier: "usage_custom_extractors"
    weight: 113
    parent: "developing_usage"
toc: true
aliases:
  - /riak/2.0.1/dev/search/custom-extractors
  - /riak/kv/2.0.1/dev/search/custom-extractors
---

Solr, and by extension Riak Search, has default extractors for a wide
variety of data types, including JSON, XML, and plaintext. Riak Search
ships with the following extractors:

Content Type | Erlang Module
:------------|:-------------
`application/json` | `yz_json_extractor`
`application/xml` | `yz_xml_extractor`
`text/plain` | `yz_text_extractor`
`text/xml` | `yz_xml_extractor`
No specified type | `yz_noop_extractor`

There are also built-in extractors for [Riak Data Types]({{<baseurl>}}riak/kv/2.0.1/developing/usage/searching-data-types).

If you're working with a data format that does not have a default Solr
extractor, you can create your own and register it with Riak Search.
We'll show you how to do so by way of example.

## The Extractor Interface

Creating a custom extract involves creating an Erlang interface that
implements two functions:

* `extract/1` --- Takes the contents of the object and calls `extract/2` 
    with the same contents and an empty list
* `extract/2` --- Takes the contents of the object and returns an Erlang
    [proplist](http://www.erlang.org/doc/man/proplists.html) with a
    single field name and a single value associated with that name

The following extractor shows how a pure text extractor implements those
two functions:

```erlang
-module(search_test_extractor).
-include("yokozuna.hrl").
-compile(export_all).

extract(Value) ->
    extract(Value, []).

extract(Value, Opts) ->
    FieldName = field_name(Opts),
    [{FieldName, Value}].

-spec field_name(proplist()) -> any().
field_name(Opts) ->
    proplists:get_value(field_name, Opts, text).
```

This extractor takes the contents of a `Value` and returns a proplist
with a single field name (in this case `text`) and the single value.
This function can be run in the Erlang shell. Let's run it providing the
text `hello`:

```erlang
> c(search_test_extractor).
%% {ok, search_test_extractor}

> search_test_extractor:extract("hello").

%% Console output:
[{text, "hello"}]
```

Upon running this command, the value `hello` would be indexed in Solr
under the fieldname `text`. If you wanted to find all objects with a
`text` field that begins with `Fourscore`, you could use the
Solr query `text:Fourscore*`, to give just one example.

## An Example Custom Extractor

Let's say that we're storing HTTP header packet data in Riak. Here's an
example of such a packet:

```
GET http://www.google.com HTTP/1.1
```

We want to register the following information in Solr:

Field name | Value | Extracted value in this example
:----------|:------|:-------------------------------
`method` | The HTTP method | `GET`
`host` | The URL's host | `www.google.com`
`uri` | The URI, i.e. what comes after the host | `/`

The example extractor below would provide the three desired
fields/values. It relies on the
[`decode_packet`](http://www.erlang.org/doc/man/erlang.html#decode_packet-3)
function from Erlang's standard library.

```erlang
-module(yz_httpheader_extractor).
-compile(export_all).

extract(Value) ->
    extract(Value, []).

%% In this example, we can ignore the Opts variable from the example
%% above, hence the underscore:
extract(Value, _Opts) ->
    {ok,
        {http_request,
         Method,
         {absoluteURI, http, Host, undefined, Uri},
         _Version},
        _Rest} = erlang:decode_packet(http, Value, []),
    [{method, Method}, {host, list_to_binary(Host)}, {uri, list_to_binary(Uri)}].
```

This file will be stored in a `yz_httpheader_extractor.erl` file (as
Erlang filenames must match the module name). Now that our extractor has
been written, it must be compiled and registered in Riak before it can
be used.

## Registering Custom Extractors

In order to use a custom extractor, you must create a compiled `.beam`
file out of your `.erl` extractor file and then tell Riak where that
file is located. Let's say that we have created a
`search_test_extractor.erl` file in the directory `/opt/beams`. First,
we need to compile that file:

```bash
erlc search_test_extractor.erl
```

To instruct Riak where to find the resulting
`search_test_extractor.beam` file, we'll need to add a line to an
`advanced.config` file in the node's `/etc` directory (more information
can be found in our documentation on [advanced]({{<baseurl>}}riak/kv/2.0.1/configuring/reference/#advanced-configuration)). Here's an
example:

```advancedconfig
[
  %% Other configs
  {vm_args, [
    {"-pa /opt/beams", ""}
  ]},
  %% Other configs
]
```

This will instruct the Erlang VM on which Riak runs to look for compiled
`.beam` files in the proper directory. You should re-start the node at
this point. Once the node has been re-started, you can use the node's
Erlang shell to register the `yz_httpheader_extractor`. First, attach to
the shell:

```bash
riak attach
```

At this point, we need to choose a MIME type for our extractor. Let's
call it `application/httpheader`. Once you're in the shell:

```erlang
> yz_extractor:register("application/httpheader", yz_httpheader_extractor).
```

If successful, this command will return a list of currently registered
extractors. It should look like this:

```erlang
[{default,yz_noop_extractor},
 {"application/httpheader",yz_httpheader_extractor},
 {"application/json",yz_json_extractor},
 {"application/riak_counter",yz_dt_extractor},
 {"application/riak_map",yz_dt_extractor},
 {"application/riak_set",yz_dt_extractor},
 {"application/xml",yz_xml_extractor},
 {"text/plain",yz_text_extractor},
 {"text/xml",yz_xml_extractor}]
```

If the `application/httpheader` extractor is part of that list, then the
extractor has been successfully registered.

## Verifying Our Custom Extractor

Now that Riak Search knows how to decode and extract HTTP header packet
data, let's store some in Riak and then query it. We'll put the example
packet data from above in a `google_packet.bin` file. Then, we'll `PUT`
that binary to Riak's `/search/extract` endpoint:

```curl
curl -XPUT $RIAK_HOST/search/extract \
     -H 'Content-Type: application/httpheader' \ # Note that we used our custom MIME type
     --data-binary @google_packet.bin
```

That should return the following JSON:

```json
{
  "method": "GET",
  "host": "www.google.com",
  "uri": "/"
}
```

We can also verify this in the Erlang shell (whether in a Riak node's
Erlang shell or otherwise):

```erlang
yz_extractor:run(<<"GET http://www.google.com HTTP/1.1\n">>, yz_httpheader_extractor).

%% Console output:
[{method,'GET'},{host,<<"www.google.com">>},{uri,<<"/">>}]
```

## Indexing and Searching HTTP Header Packet Data

Now that Solr knows how to extract HTTP header packet data, we need to
create a schema that extends the [default schema]({{<baseurl>}}riak/kv/2.0.1/developing/usage/search-schemas/#creating-a-custom-schema). The following fields should be added
to `<fields>` in the schema, which we'll name `http_header_schema` and
store in a `http_header_schema.xml` file:

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<schema name="http_header_schema" version="1.5">
<fields>
  <!-- other required fields here -->

  <field name="method" type="string" indexed="true" stored="true" multiValued="false"/>
  <field name="host" type="string" indexed="true" stored="true" multiValued="false"/>
  <field name="uri" type="string" indexed="true" stored="true" multiValued="false"/>
</fields>
```

Now, we can store the schema:

```java
import org.apache.commons.io.FileUtils

File xml = new File("http_header_schema.xml");
String xmlString = FileUtils.readFileToString(xml);
YokozunaSchema schema = new YokozunaSchema("http_header_schema", xmlString);
StoreSchema storeSchemaOp = new StoreSchema.Builder(schema).build();
client.execute(storeSchemaOp);
```

```ruby
schema_xml = File.read('http_header_schema.xml')
client.create_search_schema('http_header_schema', schema_xml)
```

```php
$schema_string = file_get_contents('http_header_schema.xml');
(new \Basho\Riak\Command\Builder\StoreSchema($riak))
  ->withName('http_header_schema')
  ->withSchemaString($schema_string)
  ->build()
  ->execute();
```

```python
import io

schema_xml = open('http_header_schema.xml').read()
client.create_search_schema('http_header_schema', schema_xml)
```

```curl
curl -XPUT $RIAK_HOST/search/schema/http_header_schema \
     -H 'Content-Type: application/xml' \
     --data-binary @http_header_schema.xml
```

Riak now has our schema stored and ready for use. Let's create a search
index called `header_data` that's associated with our new schema:

```java
YokozunaIndex headerDataIndex = new YokozunaIndex("header_data", "http_header_schema");
StoreSearchIndex storeIndex = new StoreSearchIndex.Builder(headerDataIndex)
        .build();
client.execute(storeIndex);
```

```ruby
client.create_search_index('header_data', 'http_header_schema')
```

```php
(new \Basho\Riak\Command\Builder\StoreIndex($riak))
  ->withName('header_data')
  ->usingSchema('http_header_schema')
  ->build()
  ->execute();
```

```python
client.create_search_index('header_data', 'http_header_schema')
```

```curl
curl -XPUT $RIAK_HOST/search/index/header_data \
     -H 'Content-Type: application/json' \
     -d '{"schema":"http_header_schema"}'
```

Now, we can create and activate a [bucket type]({{<baseurl>}}riak/kv/2.0.1/developing/usage/bucket-types)
for all of the HTTP header data that we plan to store. Any bucket that
bears this type will be associated with our `header_data` search index.
We'll call our bucket type `http_data_store`.

```bash
riak-admin bucket-type create http_data_store '{"props":{"search_index":"header_data"}}'
riak-admin bucket-type activate http_data_store
```

Let's use the same `google_packet.bin` file that we used previously and
store it in a bucket with the `http_data_store` bucket type, making sure
to use our custom `application/httpheader` MIME type:

```java
Location key = new Location(new Namespace("http_data_store", "packets"), "google");
File packetData = new File("google_packet.bin");
byte[] packetBinary = FileUtils.readFileToByteArray(packetData);

RiakObject packetObject = new RiakObject()
        .setContentType("application/httpheader")
        .setValue(BinaryValue.create(packetBinary));

StoreValue storeOp = new StoreValue.Builder(packetObject)
        .setLocation(key)
        .build();
client.execute(storeOp);
```

```ruby
packet_data = File.read('google_packet.bin')
bucket = client.bucket_type('http_data_store').bucket('packets')
obj = Riak::Robject.new(bucket, 'google')
obj.content_type = 'application/httpheader'
obj.raw_data = packetData
obj.store
```

```php
$object = new Object(file_get_contents("google_packet.bin"), ['Content-Type' => 'application/httpheader']);

(new \Basho\Riak\Command\Builder\StoreObject($riak))
  ->buildLocation('google', 'packets', 'http_data_store')
  ->withObject($object)
  ->build()
  ->execute();
```

```python
packet_data = open('google_packet.bin').read()
bucket = client.bucket_type('http_data_store').bucket('packets')
obj = RiakObject(client, bucket, 'google')
obj.content_type = 'application/httpheader'
obj.data = packet_data
obj.store()
```

```curl
curl -XPUT $RIAK_HOST/types/http_data_store/buckets/packets/keys/google \
     -H 'Content-Type: application/httpheader' \
     --data-binary @google_packet.bin
```

Now that we have some header packet data stored, we can query our
`header_data` index on whatever basis we'd like. First, let's verify
that we'll get one result if we query for objects that have the HTTP
method `GET`:

```java
// Using the same method from above:
String query = "method:GET";

// Again using the same method from above:
int numberFound = results.numResults(); // 1
```

```ruby
results = client.search('http_header_schema', 'method:GET')
results['num_found'] # 1
```

```php
$response = (\Basho\Riak\Command\Search\FetchObjects($riak))
  ->withQuery('method:GET')
  ->withIndexName('header_data')
  ->build()
  ->execute();

$response->getNumFound();
```

```python
results = client.fulltext_search('http_header_schema', 'method:GET')
results['num_found'] # 1
```

```curl
curl "$RIAK_HOST/search/query/header_data?wt=json&q=method:GET"

# This should return a fairly large JSON object with a "num_found" field
# The value of that field should be 1
```
