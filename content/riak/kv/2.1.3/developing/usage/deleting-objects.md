---
title: "Deleting Objects"
description: ""
project: "riak_kv"
project_version: "2.1.3"
menu:
  riak_kv-2.1.3:
    name: "Deleting Objects"
    identifier: "usage_deleting_objects"
    weight: 103
    parent: "developing_usage"
toc: true
---

**TODO: Revise body-copy**

The delete command follows a predictable pattern and looks like this:

```
DELETE /types/TYPE/buckets/BUCKET/keys/KEY
```

The normal HTTP response codes for `DELETE` operations are `204 No
Content` and `404 Not Found`. 404 responses are *normal*, in the sense
that `DELETE` operations are idempotent and not finding the resource has
the same effect as deleting it.

Let's try to delete our `genius` key from the `oscar_wilde` bucket
(which bears the type `quotes`) from above.

```java
Location geniusQuote = new Location(new Namespace("quotes", "oscar_wilde"), "genius");
DeleteValue delete = new DeleteValue.Builder(geniusQuote).build();
client.execute(delete);
```

```ruby
bucket = client.bucket_type('quotes').bucket('oscar_wilde')
bucket.delete('genius')
```

```php
(new \Basho\Riak\Command\Builder\DeleteObject($riak))
  ->buildBucket('oscar_wilde', 'quotes')
  ->build()
  ->execute();
```

```python
bucket = client.bucket_type('quotes').bucket('oscar_wilde')
bucket.delete('genius')
```

```csharp
var id = new RiakObjectId("users", "random_user_keys", null);
var obj = new RiakObject(id, @"{'user':'data'}",
    RiakConstants.ContentTypes.ApplicationJson);
var rslt = client.Put(obj);
string key = rslt.Value.Key;
id = new RiakObjectId("users", "random_user_keys", key);
var del_rslt = client.Delete(id);
```

```javascript
// continuing from above example
options = {
    bucketType: 'users', bucket: 'random_user_keys',
    key: generatedKey
};
client.deleteValue(options, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
});
```

```erlang
riakc_pb_socket:delete(Pid, {<<"quotes">>, <<"oscar_wilde">>}, <<"genius">>)
```

```golang
// Continuing from above example
cmd, err = riak.NewDeleteValueCommandBuilder().
    WithBucketType("users").
    WithBucket("random_user_keys").
    WithKey(rsp.GeneratedKey).
    Build()

if err != nil {
    fmt.Println(err.Error())
    return
}

if err := cluster.Execute(cmd); err != nil {
    fmt.Println(err.Error())
    return
}
```

```curl
curl -XDELETE http://localhost:8098/types/quotes/buckets/oscar_wilde/keys/genius
```
