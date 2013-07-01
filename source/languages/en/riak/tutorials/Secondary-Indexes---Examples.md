---
title: Secondary Index Examples
project: riak
version: 1.2.0+
document: tutorials
toc: true
audience: advanced
keywords: [operator, 2i]
---

To run the following examples, ensure that Riak is running on localhost with the HTTP endpoint listing on port 8098, and configured to use an index-capable storage backend. `curl` is required.

## Indexing Objects

The following example indexes four different objects. Notice that we're storing both integer and string (aka binary) fields, field names are automatically lowercased, some fields have multiple values, and duplicate fields are automatically de-duplicated:

```bash
curl -v -XPUT \
-d 'data1' \
-H "x-riak-index-field1_bin: val1" \
-H "x-riak-index-field2_int: 1001" \
http://127.0.0.1:8098/riak/mybucket/mykey1

curl -v -XPUT \
-d 'data2' \
-H "x-riak-index-Field1_bin: val2" \
-H "x-riak-index-Field2_int: 1002" \
http://127.0.0.1:8098/riak/mybucket/mykey2

curl -v -XPUT \
-d 'data3' \
-H "X-RIAK-INDEX-FIELD1_BIN: val3" \
-H "X-RIAK-INDEX-FIELD2_INT: 1003" \
http://127.0.0.1:8098/riak/mybucket/mykey3

curl -v -XPUT \
-d 'data4' \
-H "x-riak-index-field1_bin: val4, val4, val4a, val4b" \
-H "x-riak-index-field2_int: 1004, 1004, 1005, 1006" \
-H "x-riak-index-field2_int: 1004" \
-H "x-riak-index-field2_int: 1004" \
-H "x-riak-index-field2_int: 1004" \
-H "x-riak-index-field2_int: 1007" \
http://127.0.0.1:8098/riak/mybucket/mykey4
```

The following examples demonstrate what happens when an index field is specified with an invalid field name or type. The system responds with `400 Bad Request` and a description of the error.


Invalid field name:

```
curl -XPUT \
-d 'data1' \
-H "x-riak-index-field2_foo: 1001" \
http://127.0.0.1:8098/riak/mybucket/mykey

# Response
Unknown field type for field: 'field2_foo'.
```

Incorrect data type:

```
curl -XPUT \
-d 'data1' \
-H "x-riak-index-field2_int: bar" \
http://127.0.0.1:8098/riak/mybucket/mykey

# Response
Could not parse field 'field2_int', value 'bar'.
```

## Exact Match Query

The following examples use the HTTP interface to perform an exact match index query:

```bash
# Query a binary index...
curl http://localhost:8098/buckets/mybucket/index/field1_bin/val1

# Query an integer index...
curl http://localhost:8098/buckets/mybucket/index/field2_int/1001
```

The following example performs an exact match query and pipes the results into a MapReduce job:

```bash
curl -X POST \
-H "content-type: application/json" \
-d @- \
http://localhost:8098/mapred \
<<EOF
{
   "inputs":{
       "bucket":"mybucket",
       "index":"field1_bin",
       "key":"val3"
   },
   "query":[
      {
         "reduce":{
            "language":"erlang",
            "module":"riak_kv_mapreduce",
            "function":"reduce_identity",
            "keep":true
         }
      }
   ]
}
EOF
```

## Range Query

The following examples use the HTTP interface to perform a range query:

```bash
# Query a binary index...
curl http://localhost:8098/buckets/mybucket/index/field1_bin/val2/val4

# Query an integer index...
curl http://localhost:8098/buckets/mybucket/index/field2_int/1002/1004
```

The following example performs a range query and pipes the results into a MapReduce job:

```bash
curl -X POST \
-H "content-type: application/json" \
-d @- \
http://localhost:8098/mapred \
<<EOF
{
   "inputs":{
       "bucket":"mybucket",
       "index":"field1_bin",
       "start":"val2",
       "end":"val4"
   },
   "query":[
      {
         "reduce":{
            "language":"erlang",
            "module":"riak_kv_mapreduce",
            "function":"reduce_identity",
            "keep":true
         }
      }
   ]
}
EOF
```

{{#1.4.0+}}
#### Range with terms

When performing a range query, it is possible to retrieve the matched index values alongside the Riak keys using `return_terms=true`. An example from a small sampling of Twitter data with indexed hash tags:

```bash
curl 'http://localhost:10018/buckets/tweets/index/hashtags_bin/rock/rocl?return_terms=true'
{"results":[{"rock":"349224101224787968"},{"rocks":"349223639880699905"}]}
```
{{/1.4.0+}}

{{#1.4.0+}}
### Pagination

When asking for large result sets, it is often desirable to ask the servers to return chunks of results instead of a firehose. As of Riak 1.4, you can do so using `max_results=<n>`, where `n` is the number of results you'd like to receive.

Assuming more keys are available, a `continuation` value will be included in the results to allow the client to request the next page.

Here is an example of a range query with both `return_terms` and pagination against the same Twitter data set.  Results are fed into python for easier reading.

```bash
curl 'http://localhost:10018/buckets/tweets/index/hashtags_bin/ri/ru?max_results=5&return_terms=true' | python -mjson.tool
{
    "continuation": "g2gCbQAAAAdyaXBqYWtlbQAAABIzNDkyMjA2ODcwNTcxMjk0NzM=",
    "results": [
        {
            "rice": "349222574510710785"
        },
        {
            "rickross": "349222868095217664"
        },
        {
            "ridelife": "349221819552763905"
        },
        {
            "ripjake": "349220649341952001"
        },
        {
            "ripjake": "349220687057129473"
        }
    ]
}

# Take the continuation value from the previous result set and feed it back into the query
curl 'http://localhost:10018/buckets/tweets/index/hashtags_bin/ri/ru?continuation=g2gCbQAAAAdyaXBqYWtlbQAAABIzNDkyMjA2ODcwNTcxMjk0NzM=&max_results=5&return_terms=true' | python -mjson.tool
{
    "continuation": "g2gCbQAAAAlyb2Jhc2VyaWFtAAAAEjM0OTIyMzcwMjc2NTkxMjA2NQ==",
    "results": [
        {
            "ripjake": "349221198774808579"
        },
        {
            "ripped": "349224017347100672"
        },
        {
            "roadtrip": "349221207155032066"
        },
        {
            "roastietime": "349221370724491265"
        },
        {
            "robaseria": "349223702765912065"
        }
    ]
}
```
{{/1.4.0+}}

{{#1.4.0+}}
### Streaming
It is possible to stream results using `stream=true`. This can be combined with pagination and `return_terms`.

{{/1.4.0+}}

{{#1.4.0+}}
### Sorting
As of Riak 1.4, the result set is sorted on index values (when executing range queries) and object keys.  See the pagination example above: hash tags (2i keys) are returned in ascending order, and the object keys (Twitter IDs) for the messages which contain the "ripjake" hash tag are also returned in ascending order.

{{/1.4.0+}}

## Retrieve all object keys in a bucket based on the $bucket index

The following example uses the HTTP interface to retrieve the keys for all objects stored in the bucket 'mybucket' using an exact match on the special $bucket index.

```bash
curl http://localhost:8098/buckets/mybucket/index/\$bucket/_
```

## Count objects in a bucket based on the $bucket index

The following example performs a secondary index lookup on the $bucket index like in the previous example and pipes this into a MapReduce that counts the number of records in the 'mybucket' bucket. In order to improve efficiency, the batch size has been increased from the default size of 20.

```bash
curl -XPOST http://localhost:8098/mapred
  -H 'Content-Type: application/json'
  -d '{"inputs":{
           "bucket":"mybucket",
           "index":"$bucket",
           "key":"mybucket"
       },
       "query":[{"reduce":{"language":"erlang",
                           "module":"riak_kv_mapreduce",
                           "function":"reduce_count_inputs",
                           "arg":{"reduce_phase_batch_size":1000}
                          }
               }]
       }'
EOF
```
