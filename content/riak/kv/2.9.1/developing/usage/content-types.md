---
title: "Content Types"
description: ""
project: "riak_kv"
project_version: 2.9.1
menu:
  riak_kv-2.9.1:
    name: "Content Types"
    identifier: "usage_content_types"
    weight: 104
    parent: "developing_usage"
toc: true
---

Riak KV is a fundamentally content-agnostic database. You can use it to
store anything you want, from JSON to XML to HTML to binaries to images
and beyond. It's important to note that _all_ objects stored in
Riak need a specified content type. If you don't specify a
content type, the reaction will vary based on your client library:

```java
// In the Java client, the response when storing an object without
// specifying a content type will depend on what is being stored. If you
// store a Java Map, for example, the client will automatically specify
// that the object is "application/json"; if you store a String, the
// client will specify "application/x-www-form-urlencoded"; POJOs are
// stored as JSON by default, and so on.
```

```ruby
# In the Ruby client, you must always specify a content type. If you
# you don't, you'll see the following error:
ArgumentError: content_type is not defined!
```

```php
# PHP will default to cURLs default content-type for POST & PUT requests:
#   application/x-www-form-urlencoded

# If you use the StoreObject::buildJsonObject() method when building your command, 
# it will store the item with application/json as the content-type
```

```python
# In the Python client, the default content type is "application/json".
# Because of this, you should always make sure to specify the content
# type when storing other types of data.
```

```csharp
// Using the Riak .NET Client, the response when storing an object without
// specifying a content type will depend on what is being stored.
// If you store a Dictionary, for example, the client will
// automatically specify that the object is "application/json";
// POCOs are stored as JSON by default, and so on.
```

```javascript
// In the Node.js client, the default content type is "application/json".
// Because of this, you should always make sure to specify the content
// type when storing other types of data.
```

```erlang
%% In the Erlang client, the response when storing an object without
%% specify8ing a content type will depend on what is being stored. If
%% you store a simple binary, for example, the client will automatically
%% specify that the object is "application/octet-stream"; if you store a
%% string, the client will specify "application/x-erlang-binary"; and so
%% on.
```

```golang
// In the Go client, you must always specify a content type.
```

Because content type negotiation varies so widely from client to client,
we recommend consulting the documentation for your preferred client for
more information.

## Specifying Content Type

For all writes to Riak, you will need to specify a content type, for
example `text/plain` or `application/json`.

```java
Location wildeGeniusQuote = new Location(new Namespace("quotes", "oscar_wilde"), "genius");
BinaryValue text = BinaryValue.create("I have nothing to declare but my genius");
RiakObject obj = new RiakObject()
        .setContentType("text/plain")
        .setValue(text);
StoreValue store = new StoreValue.Builder(myKey, obj)
        .build();
client.execute(store);
```

```ruby
bucket = client.bucket_type('quotes').bucket('oscar_wilde')
obj = Riak::RObject.new(bucket, 'genius')
obj.content_type = 'text/plain'
obj.raw_data = 'I have nothing to declare but my genius'
obj.store
```

```php
$response = (new \Basho\Riak\Command\Builder\StoreObject($riak))
  ->buildLocation('genius', 'oscar_wilde', 'quotes')
  ->buildObject('I have nothing to declare but my genius!', 'text/plain')
  ->build()
  ->execute();
```

```python
bucket = client.bucket_type('quotes').bucket('oscar_wilde')
obj = RiakObject(client, bucket, 'genius')
obj.content_type = 'text/plain'
obj.data = 'I have nothing to declare but my genius'
obj.store()
```

```csharp
var id = new RiakObjectId("quotes", "oscar_wilde", "genius");
var obj = new RiakObject(id, "I have nothing to declare but my genius",
    RiakConstants.ContentTypes.TextPlain);
var rslt = client.Put(obj);
```

```javascript
var riakObj = new Riak.Commands.KV.RiakObject();
riakObj.setContentType('text/plain');
riakObj.setValue('I have nothing to declare but my genius');
client.storeValue({
    bucketType: 'quotes', bucket: 'oscar_wilde', key: 'genius',
    value: riakObj
}, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
});
```

```erlang
Object = riakc_obj:new({<<"quotes">>, <<"oscar_wilde">>},
                       <<"genius">>,
                       <<"I have nothing to declare but my genius">>,
                       <<"text/plain">>).
riakc_pb_socket:put(Pid, Object).
```

```golang
obj := &riak.Object{
    ContentType:     "text/plain",
    Charset:         "utf-8",
    ContentEncoding: "utf-8",
    Value:           []byte("I have nothing to declare but my genius"),
}

cmd, err := riak.NewStoreValueCommandBuilder().
    WithBucketType("quotes").
    WithBucket("oscar_wilde").
    WithKey("genius").
    WithContent(obj).
    Build()

if err != nil {
    fmt.Println(err.Error())
    return
}

if err := cluster.Execute(cmd); err != nil {
    fmt.Println(err.Error())
    return
}

svc := cmd.(*riak.StoreValueCommand)
rsp := svc.Response
```

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "I have nothing to declare but my genius" \
  http://localhost:8098/types/quotes/buckets/oscar_wilde/keys/genius

# Please note that POST is also a valid method for writes, for the sake
# of compatibility
```
