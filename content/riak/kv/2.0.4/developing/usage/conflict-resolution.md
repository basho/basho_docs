---
title: "Conflict Resolution"
description: ""
project: "riak_kv"
project_version: "2.0.4"
menu:
  riak_kv-2.0.4:
    name: "Conflict Resolution"
    identifier: "usage_conflict_resolution"
    weight: 116
    parent: "developing_usage"
toc: true
aliases:
  - /riak/2.0.4/dev/using/conflict-resolution
  - /riak/kv/2.0.4/dev/using/conflict-resolution
---

[usage bucket types]: {{<baseurl>}}riak/kv/2.0.4/developing/usage/bucket-types
[use ref strong consistency]: {{<baseurl>}}riak/kv/2.0.4/using/reference/strong-consistency

One of Riak's [central goals](../../../learn/why-riak-kv) is high availability. It was built as a [clustered]({{<baseurl>}}riak/kv/2.0.4/learn/concepts/clusters) system in which any [node]({{<baseurl>}}riak/kv/2.0.4/learn/glossary/#node) is capable of receiving requests without requiring that
every node participate in each request.

If you are using Riak in an [eventually consistent]({{<baseurl>}}riak/kv/2.0.4/learn/concepts/eventual-consistency) way, conflicts between object values on different nodes is
unavoidable. Often, Riak can resolve these conflicts on its own
internally if you use causal context, i.e. [vector clocks]({{<baseurl>}}riak/kv/2.0.4/learn/concepts/causal-context#vector-clocks) or [dotted version vectors]({{<baseurl>}}riak/kv/2.0.4/learn/concepts/causal-context#dotted-version-vectors), when updating objects. Instructions on this can be found in the section [below](#siblings).

{{% note title="Important note on terminology" %}}
In versions of Riak prior to 2.0, vector clocks were the only causal context
mechanism available in Riak, which changed with the introduction of dotted
version vectors in 2.0. Please note that you may frequent find terminology in
client library APIs, internal Basho documentation, and more that uses the term
"vector clock" interchangeably with causal context in general. Riak's HTTP API
still uses a `X-Riak-Vclock` header, for example, even if you are using dotted
version vectors.
{{% /note %}}

But even when you use causal context, Riak cannot always decide which
value is most causally recent, especially in cases involving concurrent
updates to an object. So how does Riak behave when it can't decide on a
single most-up-to-date value? **That is your choice**. A full listing of
available options can be found in the [section below](#client-and-server-side-conflict-resolution). For now,
though, please bear in mind that we strongly recommend one of the
following two options:

1. If your data can be modeled as one of the currently available [Riak
   Data Types]({{<baseurl>}}riak/kv/2.0.4/developing/data-types), we recommend using one of these types,
   because all of them have conflict resolution _built in_, completely
   relieving applications of the need to engage in conflict resolution.
2. If your data cannot be modeled as one of the available Data Types,
   we recommend allowing Riak to generate [siblings](#siblings) and to design your application to resolve
   conflicts in a way that fits your use case. Developing your own
   **conflict resolution strategy** can be tricky, but it has clear
   advantages over other approaches.

Because Riak allows for a mixed approach when storing and managing data,
you can apply multiple conflict resolution strategies within a cluster.

> **Note on strong consistency**
>
> In versions of Riak 2.0 and later, you have the option of using Riak in
a strongly consistent fashion. This document pertains to usage of Riak
as an _eventually_ consistent system. If you'd like to use Riak's
strong consistency feature, please refer to the following documents:
>
> * [Using Strong Consistency]({{<baseurl>}}riak/kv/2.0.4/developing/app-guide/strong-consistency) --- A guide for developers
> * [Managing Strong Consistency]({{<baseurl>}}riak/kv/2.0.4/configuring/strong-consistency) --- A guide for operators
> * [strong consistency][use ref strong consistency] --- A more theoretical explication of strong
  consistency

## Client- and Server-side Conflict Resolution

Riak's eventual consistency model is powerful because Riak is
fundamentally non-opinionated about how data resolution takes place.
While Riak _does_ have a set of [defaults]({{<baseurl>}}riak/kv/2.0.4/developing/app-guide/replication-properties#available-parameters), there are a variety of general
approaches to conflict resolution that are available. In Riak, you can
mix and match conflict resolution strategies at the bucket level,
[using bucket types][usage bucket types]. The most important [bucket properties]({{<baseurl>}}riak/kv/2.0.4/learn/concepts/buckets)
to consider when reasoning about conflict resolution are the
`allow_mult` and `last_write_wins` properties.

These properties provide you with the following basic options:

### Timestamp-based Resolution

If the `[allow_mult](#siblings)` parameter is set to
`false`, Riak resolves all object replica conflicts internally and does
not return siblings to the client. How Riak resolves those conflicts
depends on the value that you set for a different bucket property,
`[last_write_wins]({{<baseurl>}}riak/kv/2.0.4/learn/concepts/buckets)`. If `last_write_wins` is set to `false`,
Riak will resolve all conflicts on the basis of
[timestamps](http://en.wikipedia.org/wiki/Timestamp), which are
attached to all Riak objects as metadata.

The problem with timestamps is that they are not a reliable resolution
mechanism in distributed systems, and they always bear the risk of data
loss. A better yet still-problematic option is to adopt a
last-write-wins strategy, described directly below.

### Last-write-wins

Another way to manage conflicts is to set `allow_mult` to `false`, as
with timestamp-based resolution, while also setting the
`last_write_wins` parameter to
`true`. This produces a so-called last-write-wins (LWW) strategy whereby
Riak foregoes the use of all internal conflict resolution strategies
when making writes, effectively disregarding all previous writes.

The problem with LWW is that it will necessarily drop some writes in the
case of concurrent updates in the name of preventing sibling creation.
If your use case requires that your application be able to reason about
differing values produced in the case of concurrent updates, then we
advise against LWW as a general conflict resolution strategy.

However, LWW can be useful---and safe---if you are certain that there
will be no concurrent updates. If you are storing immutable data in
which each object is guaranteed to have its own key or engaging in
operations related to bulk loading, you should consider LWW.

{{% note title="Undefined behavior warning" %}}
Setting both `allow_mult` and `last_write_wins` to `true` necessarily leads to
unpredictable behavior and should always be avoided.
{{% /note %}}

### Resolve Conflicts on the Application Side

While setting `allow_mult` to `false` unburdens applications from having
to reason about siblings, delegating that responsibility to Riak itself,
it bears all of the drawbacks explained above. On the other hand,
setting `allow_mult` to `true` has the following benefits:

* Riak will retain writes even in the case of concurrent updates to a
  key, which enables you to capture the benefits of high availability
  with a far lower risk of data loss
* If your application encounters siblings, it can apply its own
  use-case-specific conflict resolution logic

Conflict resolution in Riak can be a complex business, but the presence
of this variety of options means that requests to Riak can always be
made in accordance with your data model(s), business needs, and use
cases. For examples of client-side sibling resolution, see the following
client-library-specific docs:

* [Java]({{<baseurl>}}riak/kv/2.0.4/developing/usage/conflict-resolution/java)
* [Ruby]({{<baseurl>}}riak/kv/2.0.4/developing/usage/conflict-resolution/ruby)
* [Python]({{<baseurl>}}riak/kv/2.0.4/developing/usage/conflict-resolution/python)
* [C#]({{<baseurl>}}riak/kv/2.0.4/developing/usage/conflict-resolution/csharp)
* [Node.js]({{<baseurl>}}riak/kv/2.0.4/developing/usage/conflict-resolution/nodejs)

In Riak versions 2.0 and later, `allow_mult` is set to `true` by default
for any [bucket types]({{<baseurl>}}riak/kv/2.0.4/developing/usage/bucket-types) that you create. This means
that if you wish to avoid client-side sibling resolution, you have a few
options:

* Explicitly create and activate [bucket types]({{<baseurl>}}riak/kv/2.0.4/developing/usage/bucket-types)
  that set `allow_mult` to `false`
* Use Riak's [Configuration Files]({{<baseurl>}}riak/kv/2.0.4/configuring/reference) to change the [default bucket properties]({{<baseurl>}}riak/kv/2.0.4/configuring/reference#default-bucket-properties) for your
  cluster. If you set the `buckets.default.allow_mult` parameter to
  `false`, all bucket types that you create will have `allow_mult` set
  to `false` by default.

## Causal Context

When a value is stored in Riak, it is tagged with a piece of metadata
called a **causal context** which establishes the object's initial
version. Causal context comes in one of two possible forms, depending
on what value you set for `dvv_enabled`. If set to `true`, [dotted version vectors]({{<baseurl>}}riak/kv/2.0.4/learn/concepts/causal-context#dotted-version-vectors) will be used; if set to `false` (the default), [vector clocks]({{<baseurl>}}riak/kv/2.0.4/learn/concepts/causal-context#vector-clocks) will be used.

Causal context essentially enables Riak to compare the different values
of objects stored in Riak and to determine a number of important things
about those values:

 * Whether one value is a direct descendant of the other
 * Whether the values are direct descendants of a common parent
 * Whether the values are unrelated in recent heritage

Using the information provided by causal context, Riak is frequently,
though not always, able to resolve conflicts between values without
producing siblings.

Both vector clocks and dotted version vectors are non human readable and
look something like this:

```
a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkzGNlsP/VfYYvCwA=
```

If `allow_mult` is set to `true`, you should _always_ use causal context
when updating objects, _unless you are certain that no object exists
under that key_. Failing to use causal context with mutable data,
especially for objects that are frequently updated, can lead to
[sibling explosion]({{<baseurl>}}riak/kv/2.0.4/using/performance/latency-reduction#siblings), which can
produce a variety of problems in your cluster. Fortunately, much of the
work involved with using causal context is handled automatically by
Basho's official [client libraries]({{<baseurl>}}riak/kv/2.0.4/developing/client-libraries). Examples can be found for each
client library in the [Object Updates]({{<baseurl>}}riak/kv/2.0.4/developing/usage/updating-objects) document.

## Siblings

A **sibling** is created when Riak is unable to resolve the canonical
version of an object being stored, i.e. when Riak is presented with
multiple possible values for an object and can't figure out which one is
most causally recent. The following scenarios can create sibling values
inside of a single object:

1. **Concurrent writes** --- If two writes occur simultaneously from
clients, Riak may not be able to choose a single value to store, in
which case the object will be given a sibling. These writes could happen
on the same node or on different nodes.
2. **Stale causal context** --- Writes from any client using a stale
[causal context]({{<baseurl>}}riak/kv/2.0.4/learn/concepts/causal-context). This is a less likely scenario if a client updates
the object by reading the object first, fetching the causal context
currently attached to the object, and then returning that causal context
to Riak when performing the update (fortunately, our client libraries
handle much of this automatically). However, even if a client follows
this protocol when performing updates, a situation may occur in which an
update happens from a different client while the read/write cycle is
taking place. This may cause the first client to issue the write with an
old causal context value and for a sibling to be created. A client is
"misbehaved" if it habitually updates objects with a stale or no context
object.
3. **Missing causal context** --- If an object is updated with no causal
context attached, siblings are very likely to be created. This is an
unlikely scenario if you're using a Basho client library, but it _can_
happen if you are manipulating objects using a client like `curl` and
forgetting to set the `X-Riak-Vclock` header.

## Siblings in Action

Let's have a more concrete look at how siblings work in Riak. First,
we'll create a bucket type called `siblings_allowed` with `allow_mult`
set to `true`:

```bash
riak-admin bucket-type create siblings_allowed '{"props":{"allow_mult":true}}'
riak-admin bucket-type activate siblings_allowed
riak-admin bucket-type status siblings_allowed
```

If the type has been activated, running the `status` command should
return `siblings_allowed is active`. Now, we'll create two objects and
write both of them to the same key without first fetching the object
(which obtains the causal context):

```java
Location bestCharacterKey =
  new Location(new Namespace("siblings_allowed", "nickolodeon"), "best_character");

RiakObject obj1 = new RiakObject()
        .withContentType("text/plain")
        .withValue(BinaryValue.create("Ren"));
RiakObject obj2 = new RiakObject()
        .withContentType("text/plain")
        .withValue(BinaryValue.create("Stimpy"));
StoreValue store1 = new StoreValue.Builder(obj1)
        .withLocation(bestCharacterKey)
        .build();
StoreValue store2 = new StoreValue.Builder(obj2)
        .withLocation(bestCharacterKey)
        .build();
client.execute(store1);
client.execute(store2);
```

```ruby
bucket = client.bucket_type('siblings_allowed').bucket('nickolodeon')
obj1 = Riak::RObject.new(bucket, 'best_character')
obj1.content_type = 'text/plain'
obj1.raw_data = 'Ren'
obj1.store

obj2 = Riak::RObject.new(bucket, 'best_character')
obj2.content_type = 'text/plain'
obj2.raw_data = 'Stimpy'
obj2.store
```

```python
bucket = client.bucket_type('siblings_allowed').bucket('nickolodeon')
obj1 = RiakObject(client, bucket, 'best_character')
obj1.content_type = 'text/plain'
obj1.data = 'Ren'
obj1.store()

obj2 = RiakObject(client, bucket, 'best_character')
obj2.content_type = 'text/plain'
obj2.data = 'Stimpy'
obj2.store()
```

```csharp
var id = new RiakObjectId("siblings_allowed", "nickolodeon", "best_character");

var renObj = new RiakObject(id, "Ren", RiakConstants.ContentTypes.TextPlain);
var stimpyObj = new RiakObject(id, "Stimpy", RiakConstants.ContentTypes.TextPlain);

var renResult = client.Put(renObj);
var stimpyResult = client.Put(stimpyObj);
```

```javascript
var obj1 = new Riak.Commands.KV.RiakObject();
obj1.setContentType('text/plain');
obj1.setBucketType('siblings_allowed');
obj1.setBucket('nickolodeon');
obj1.setKey('best_character');
obj1.setValue('Ren');

var obj2 = new Riak.Commands.KV.RiakObject();
obj2.setContentType('text/plain');
obj2.setBucketType('siblings_allowed');
obj2.setBucket('nickolodeon');
obj2.setKey('best_character');
obj2.setValue('Ren');

var storeFuncs = [];
[obj1, obj2].forEach(function (obj) {
    storeFuncs.push(
        function (async_cb) {
            client.storeValue({ value: obj }, function (err, rslt) {
                async_cb(err, rslt);
            });
        }
    );
});

async.parallel(storeFuncs, function (err, rslts) {
    if (err) {
        throw new Error(err);
    }
});
```

```erlang
Obj1 = riakc_obj:new({<<"siblings_allowed">>, <<"nickolodeon">>},
                     <<"best_character">>,
                     <<"Ren">>,
                     <<"text/plain">>),
Obj2 = riakc_obj:new({<<"siblings_allowed">>, <<"nickolodeon">>},
                     <<"best_character">>,
                     <<"Stimpy">>,
                     <<"text/plain">>),
riakc_pb_socket:put(Pid, Obj1),
riakc_pb_socket:put(Pid, Obj2).
```

```curl
curl -XPUT http://localhost:8098/types/siblings_allowed/nickolodeon/whatever/keys/best_character \
  -H "Content-Type: text/plain" \
  -d "Ren"

curl -XPUT http://localhost:8098/types/siblings_allowed/nickolodeon/whatever/keys/best_character \
  -H "Content-Type: text/plain" \
  -d "Stimpy"
```

> **Getting started with Riak KV clients**
>
> If you are connecting to Riak using one of Basho's official
[client libraries]({{<baseurl>}}riak/kv/2.0.4/developing/client-libraries), you can find more information about getting started with your client in [Developing with Riak KV: Getting Started]({{<baseurl>}}riak/kv/2.0.4/developing/getting-started) section.

At this point, multiple objects have been stored in the same key without
passing any causal context to Riak. Let's see what happens if we try to
read contents of the object:

```java
Location bestCharacterKey =
  new Location(new Namespace("siblings_allowed", "nickolodeon"), "best_character");

FetchValue fetch = new FetchValue.Builder(bestCharacterKey).build();
FetchValue.Response response = client.execute(fetch);
RiakObject obj = response.getValue(RiakObject.class);
System.out.println(obj.getValue().toString());
```

```ruby
bucket = client.bucket_type('siblings_allowed').bucket('nickolodeon')
obj = bucket.get('best_character')
obj
```

```python
bucket = client.bucket_type('siblings_allowed').bucket('nickolodeon')
obj = bucket.get('best_character')
obj.siblings
```

```csharp
var id = new RiakObjectId("siblings_allowed", "nickolodeon", "best_character");
var getResult = client.Get(id);
RiakObject obj = getResult.Value;
Debug.WriteLine(format: "Sibling count: {0}", args: obj.Siblings.Count);
foreach (var sibling in obj.Siblings)
{
    Debug.WriteLine(
        format: "    VTag: {0}",
        args: sibling.VTag);
}
```

```javascript
client.fetchValue({
    bucketType: 'siblings_allowed', bucket:
        'nickolodeon', key: 'best_character'
}, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
    logger.info("nickolodeon/best_character has '%d' siblings",
        rslt.values.length);
});
```

```curl
curl http://localhost:8098/types/siblings_allowed/buckets/nickolodeon/keys/best_character
```

Uh-oh! Siblings have been found. We should get this response:

```java
com.basho.riak.client.cap.UnresolvedConflictException: Siblings found
```

```ruby
<Riak::RObject {nickolodeon,best_character} [#<Riak::RContent [text/plain]:"Ren">, #<Riak::RContent [text/plain]:"Stimpy">]>
```

```python
[<riak.content.RiakContent object at 0x10a00eb90>, <riak.content.RiakContent object at 0x10a00ebd0>]
```

```csharp
Sibling count: 2
    VTag: 1DSVo7VED8AC6llS8IcDE6
    VTag: 7EiwrlFAJI5VMLK87vU4tE
```

```javascript
info: nickolodeon/best_character has '2' siblings
```

```curl
Siblings:
175xDv0I3UFCfGRC7K7U9z
6zY2mUCFPEoL834vYCDmPe
```

As you can see, reading an object with sibling values will result in
some form of "multiple choices" response (e.g. `300 Multiple Choices` in
HTTP). If you're using the HTTP interface and want to view all sibling
values, you can attach an `Accept: multipart/mixed` header to your
request:

```curl
curl -H "Accept: multipart/mixed" \
  http://localhost:8098/types/siblings_allowed/buckets/nickolodeon/keys/best_character
```

Response (without headers):

```
ren
--WUnzXITIPJFwucNwfdaofMkEG7H

stimpy
--WUnzXITIPJFwucNwfdaofMkEG7H--
```

If you select the first of the two siblings and retrieve its value, you
should see `Ren` and not `Stimpy`.

### Using Causal Context

Once you are presented with multiple options for a single value, you
must determine the correct value. In an application, this can be done
either in an automatic fashion, using a use case-specific resolver, or
by presenting the conflicting objects to the end user. For more
information on application-side conflict resolution, see our
client-library-specific documentation for the following languages:

* [Java]({{<baseurl>}}riak/kv/2.0.4/developing/usage/conflict-resolution/java)
* [Ruby]({{<baseurl>}}riak/kv/2.0.4/developing/usage/conflict-resolution/ruby)
* [Python]({{<baseurl>}}riak/kv/2.0.4/developing/usage/conflict-resolution/python)
* [C#]({{<baseurl>}}riak/kv/2.0.4/developing/usage/conflict-resolution/csharp)
* [Node.js]({{<baseurl>}}riak/kv/2.0.4/developing/usage/conflict-resolution/nodejs)

We won't deal with conflict resolution in this section. Instead, we'll
focus on how to use causal context.

After having written several objects to Riak in the section above, we
have values in our object: `Ren` and `Stimpy`. But let's say that we
decide that `Stimpy` is the correct value based on our application's use
case. In order to resolve the conflict, we need to do three things:

1. Fetch the current object (which will return both siblings)
2. Modify the value of the object, i.e. make the value `Stimpy`
3. Write the object back to the `best_character` key

What happens when we fetch the object first, prior to the update, is
that the object handled by the client has a causal context attached. At
that point, we can modify the object's value, and when we write the
object back to Riak, _the causal context will automatically be attached
to it_. Let's see what that looks like in practice:

```java
// First, we fetch the object
Location bestCharacterKey =
  new Location(new Namespace("siblings_allowed", "nickolodeon"), "best_character");
FetchValue fetch = new FetchValue.Builder(bestCharacterKey).build();
FetchValue.Response res = client.execute(fetch);
RiakObject obj = res.getValue(RiakObject.class);


// Then we modify the object's value
obj.setValue(BinaryValue.create("Stimpy"));

// Then we store the object, which has the vector clock already attached
StoreValue store = new StoreValue.Builder(obj)
        .withLocation(bestCharacterKey);
client.execute(store);
```

```ruby
# First, we fetch the object
bucket = client.bucket('nickolodeon')
obj = bucket.get('best_character', type: 'siblings_allowed')

# Then we modify the object's value
obj.raw_data = 'Stimpy'

# Then we store the object, which has the vector clock already attached
obj.store
```

```python
# First, we fetch the object
bucket = client.bucket_type('siblings_allowed').bucket('nickolodeon')
obj = bucket.get('best_character')

# Then pick one of the siblings
resolved_sibling = obj.siblings[3]

# Then replace the siblings data structure with the single sibling we want to keep
obj.siblings = [resolved_sibling]
obj.store()
```

```csharp
// First, fetch the object
var getResult = client.Get(id);

// Then, modify the object's value
RiakObject obj = getResult.Value;
obj.SetObject<string>("Stimpy", RiakConstants.ContentTypes.TextPlain);

// Then, store the object which has vector clock attached
var putRslt = client.Put(obj);
CheckResult(putRslt);

obj = putRslt.Value;
// Voila, no more siblings!
Debug.Assert(obj.Siblings.Count == 0);
```

```javascript
client.fetchValue({
        bucketType: 'siblings_allowed',
        bucket: 'nickolodeon',
        key: 'best_character'
    }, function (err, rslt) {
        if (err) {
            throw new Error(err);
        }

        var riakObj = rslt.values.shift();
        riakObj.setValue('Stimpy');
        client.storeValue({ value: riakObj, returnBody: true },
            function (err, rslt) {
                if (err) {
                    throw new Error(err);
                }

                assert(rslt.values.length === 1);
            }
        );
    }
);
```

```curl
curl -i http://localhost:8098/types/siblings_allowed/buckets/nickolodeon/keys/best_character

# In the HTTP interface, the causal context can be found in the
# "X-Riak-Vclock" header. That will look something like this:

X-Riak-Vclock: a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkzGNlsP/VfYYvCwA=

# When performing a write to the same key, that same header needs to
# accompany the write for Riak to be able to use the vector clock
```

{{% note title="Concurrent conflict resolution" %}}
It should be noted that it is possible to have two clients that are
simultaneously engaging in conflict resolution. To avoid a pathological
divergence, you should be sure to limit the number of reconciliations and fail
once that limit has been exceeded.
{{% /note %}}

### Sibling Explosion

Sibling explosion occurs when an object rapidly collects siblings
without being reconciled. This can lead to myriad issues. Having an
enormous object in your node can cause reads of that object to crash
the entire node. Other issues include [increased cluster latency]({{<baseurl>}}riak/kv/2.0.4/using/performance/latency-reduction) as the object is replicated and out-of-memory errors.

### Vector Clock Explosion

Besides sibling explosion, the vector clock itself can grow extremely
large when a significant volume of updates are performed on a single
object in a small period of time. While updating a single object
_extremely_ frequently is not recommended, you can tune Riak's vector
clock pruning to prevent vector clocks from growing too large too
quickly. More on pruning in the [section below](#vector-clock-pruning).

### How does `last_write_wins` affect resolution?

On the surface, it seems like setting `allow_mult` to `false`
(the default) and `last_write_wins` to `true` would result in the same
behavior, but there is a subtle distinction.

Even though both settings return only one value to the client, setting
`allow_mult` to `false` still uses vector clocks for resolution, whereas
if `last_write_wins` is `true`, Riak reads the timestamp to determine
the latest version. Deeper in the system, if `allow_mult` is `false`,
Riak will still allow siblings to exist when they are created (via
concurrent writes or network partitions), whereas setting
`last_write_wins` to `true` means that Riak will overwrite the value
with the one that has the later timestamp.

When you don't care about sibling creation, setting `allow_mult` to
`false` has the least surprising behavior: you get the latest value,
but network partitions are handled gracefully. However, for cases in
which keys are rewritten often (and quickly) and the new value isn't
necessarily dependent on the old value, `last_write_wins` will provide
better performance. Some use cases where you might want to use
`last_write_wins` include caching, session storage, and insert-only
(no updates).

{{% note title="Note on combining `allow_mult` and `last_write_wins`" %}}
The combination of setting both the `allow_mult` and `last_write_wins`
properties to `true` leads to undefined behavior and should not be used.
{{% /note %}}

## Vector Clock Pruning

Riak regularly prunes vector clocks to prevent overgrowth based on four
parameters which can be set for any bucket type that you create:

Parameter | Default value | Description
:---------|:--------------|:-----------
`small_vclock` | `50` | If the length of the vector clock list is smaller than this value, the list's entries will not be pruned
`big_vclock` | `50` | If the length of the vector clock list is larger than this value, the list will be pruned
`young_vclock` | `20` | If a vector clock entry is younger than this value (in milliseconds), it will not be pruned
`old_vclock` | `86400` (one day) | If a vector clock entry is older than this value (in milliseconds), it will be pruned

This diagram shows how the values of these parameters dictate the vector
clock pruning process:

![Vclock Pruning]({{<baseurl>}}images/vclock-pruning.png)

## More Information

Additional background information on vector clocks:

* [Vector Clocks on Wikipedia](http://en.wikipedia.org/wiki/Vector_clock)
* [Why Vector Clocks are Easy](http://basho.com/why-vector-clocks-are-easy/)
* [Why Vector Clocks are Hard](http://basho.com/why-vector-clocks-are-hard/)
* The vector clocks used in Riak are based on the [work of Leslie Lamport](http://portal.acm.org/citation.cfm?id=359563)
