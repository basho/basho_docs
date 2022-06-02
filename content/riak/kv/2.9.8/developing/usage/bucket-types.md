---
title: "Bucket Types"
description: ""
project: "riak_kv"
project_version: 2.9.8
menu:
  riak_kv-2.9.8:
    name: "Bucket Types"
    identifier: "usage_bucket_types"
    weight: 108
    parent: "developing_usage"
toc: true
aliases:
  - /riak/2.9.8/dev/advanced/bucket-types
  - /riak/kv/2.9.8/dev/advanced/bucket-types
---

If you ever need to turn off indexing for a bucket, set the
`search_index` property to the `_dont_index_` sentinel value.

## Bucket Properties

Although we recommend that you use all new buckets under a bucket type,
if you have existing data with a type-free bucket (i.e. under the
`default` bucket type) you can set the `search_index` property for a
specific bucket.

```java
Namespace catsBucket = new Namespace("cats");
StoreBucketPropsOperation storePropsOp = new StoreBucketPropsOperation.Builder(catsBucket)
        .withSearchIndex("famous")
        .build();
client.execute(storePropsOp);
```

```ruby
bucket = client.bucket('cats')
bucket.properties = {'search_index' => 'famous'}
```

```php
(new \Basho\Riak\Command\Builder\Search\AssociateIndex($riak))
    ->withName('famous')
    ->buildBucket('cats')
    ->build()
    ->execute();
```

```python
bucket = client.bucket('cats')
bucket.set_properties({'search_index': 'famous'})
```

```csharp
var properties = new RiakBucketProperties();
properties.SetSearchIndex("famous");
var rslt = client.SetBucketProperties("cats", properties);
```

```javascript
var bucketProps_cb = function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
    // success
};

var store = new Riak.Commands.KV.StoreBucketProps.Builder()
    .withBucket("cats")
    .withSearchIndex("famous")
    .withCallback(bucketProps_cb)
    .build();

client.execute(store);
```

```erlang
riakc_pb_socket:set_search_index(Pid, <<"cats">>, <<"famous">>).
```

```golang
cmd, err := riak.NewStoreBucketPropsCommandBuilder().
    WithBucketType("animals").
    WithBucket("cats").
    WithSearchIndex("famous").
    Build()
if err != nil {
    return err
}

err = cluster.Execute(cmd)
```

```curl
curl -XPUT $RIAK_HOST/buckets/cats/props \
     -H'content-type:application/json' \
     -d'{"props":{"search_index":"famous"}}'
```




