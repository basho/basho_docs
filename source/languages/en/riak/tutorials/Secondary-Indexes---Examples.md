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

## Retrieve all object keys in a bucket based on the $bucket index

The following example uses the HTTP interface to retrieve the keys for all objects stored in the bucket 'mybucket' using an exact match on the special $bucket index. 

```bash
curl http://localhost:8098/buckets/mybucket/index/\$bucket/mybucket
```

## Count objects in a bucket based on the $bucket index

The following example performs a secondary index lookup on the $bucket index like in the previous examle and pipes this into a MapReduce that counts the number of records in the 'mybucket' bucket. The 'do_prereduce' option is enabled in order to reduce the amount of data being sent between nodes.

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
                           "arg":{"do_prereduce":true}
                          }
               }]
       }'
EOF
```