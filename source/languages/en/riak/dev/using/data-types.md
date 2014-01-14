---
title: Using Datatypes
project: Riak
version: 2.0.0+
document: tutorials
toc: true
audience: intermediate
keywords: [developers, data-types]
---

In versions of Riak prior to 2.0, Riak was essentially agnostic toward data types (with the exception of counters, introduced in version 1.4)

Sets, Maps, Registers, Flags, Counters

Enables developers to delegate certain responsibilities back to Riak

Shopping cart example => previously, devs would have to resolve conflicts on the application side; CRDTs enable them to allow Riak to handle conflicts in type-specific ways

Monotonic => change is in a single direction

Setting up a bucket to store a specific data type, e.g. maps:

```bash
riak-admin bucket-type create map_bucket '{"props":{"datatype":"map"}}''
```

