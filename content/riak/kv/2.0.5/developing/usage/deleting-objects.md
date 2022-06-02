---
title: "Deleting Objects"
description: ""
project: "riak_kv"
project_version: "2.0.5"
menu:
  riak_kv-2.0.5:
    name: "Deleting Objects"
    identifier: "usage_deleting_objects"
    weight: 103
    parent: "developing_usage"
toc: true
---

The delete command follows a predictable pattern and looks like this:

```
DELETE /types/TYPE/buckets/BUCKET/keys/KEY
```

The normal HTTP response codes for `DELETE` operations are `204 No
Content` and `404 Not Found`. 404 responses are *normal*, in the sense
that `DELETE` operations are idempotent and not finding the resource has
the same effect as deleting it.

Let's try to delete the `genius` key from the `oscar_wilde` bucket
(which bears the type `quotes`):

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

## Client Library Examples

If you are updating an object that has been deleted---or if an update 
might target a deleted object---we recommend that
you first fetch the [causal context]({{<baseurl>}}riak/kv/2.0.5/learn/concepts/causal-context) of the object prior to updating.
This can be done by setting the `deletedvclock` parameter to `true` as
part of the [fetch operation]({{<baseurl>}}riak/kv/2.0.5/developing/api/protocol-buffers/fetch-object). This can also be done
with the official Riak clients for Ruby, Java, and Erlang, as in the
example below:


```ruby
object.delete
deleted_object = bucket.get('bucket', 'key', deletedvclock: true)
deleted_object.vclock
```

```python
# It is not currently possible to fetch the causal context for a deleted
# key in the Python client.
```

```java
Location loc = new Location("<bucket>")
    .setBucketType("<bucket_type>")
    .setKey("<key>");
FetchValue fetch = new FetchValue.Builder(loc)
    .withOption(Option.DELETED_VCLOCK, true)
    .build();
FetchValue.Response response = client.execute(fetch);
System.out.println(response.getVclock().asString());
```

```erlang
{ok, Obj} = riakc_pb_socket:get(Pid,
                              {<<"bucket_type">>, <<"bucket">>},
                              <<"key">>,
                              [{deleted_vclock}]).

%% In the Erlang client, the vector clock is accessible using the Obj
%% object obtained above.
```

```php
$response = (new \Basho\Riak\Command\Builder\FetchObject($riak))
  ->buildLocation('deleted_key', 'in_some_bucket', 'of_a_certain_type')
  ->build()
  ->execute();

echo $response->getVclock(); // a85hYGBgzGDKBVI8m9WOeb835ZRhYCg1zGBKZM5jZdhnceAcXxYA
```
