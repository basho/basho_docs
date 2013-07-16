---
title: "Multi Data Center Replication: Per Bucket"
project: riakee
version: 1.1.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, bucket]
---

To enable or disable replication per bucket, you can use the `repl` bucket property.

Some changes have occurred between 1.1 and 1.2.

These `repl` values are available in Riak EE version 1.1 and above:

  * `true`: enable replication (realtime + fullsync)
  * `false`: disable replication (realtime + fullsync)

These option values are only available in Riak EE version 1.2 and above:

  * `realtime`: replication only occurs in realtime for this bucket
  * `fullsync`: replication only occurs during a full-synchronization
  * `both`: replication occurs in realtime and during full-synchronization

### Example of Disabling

```
curl -v -XPUT http://127.0.0.1:8091/riak/my_bucket \
  -H "Content-Type: application/json" \
  -d '{"props":{"repl":false}}'
```

### Example of Enabling

```
curl -v -XPUT http://127.0.0.1:8091/riak/my_bucket \
  -H "Content-Type: application/json" \
  -d '{"props":{"repl":true}}'
```
