---
title: Using Datatypes
project: Riak
version: 2.0.0+
document: tutorials
toc: true
audience: intermediate
keywords: [developers, data-types]
---

https://gist.github.com/russelldb/5da7d895cebc77dd38b8

There are three options:

`map`
`set`
`counter`

You can _not_ create a bucket 

## Using Bucket Types with CRDTs

Setting up a bucket to store a specific data type, e.g. maps:

```bash
riak-admin bucket-type create map_bucket '{"props":{"datatype":"map"}}''
```

Via HTTP:

```curl
curl -XPUT \
-H "Content-Type: application/json" \
-d '{"props":{"datatype":"map"}}' \
http://localhost:8098/buckets/map_bucket/props
```

And then check:

```curl
curl http://localhost:8098/buckets/map_bucket/props | python -mjson.tool
```

PBC example?