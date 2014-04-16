---
title: Conflict Resolution
project: riak
version: 2.0.0+
document: tutorials
audience: intermediate
keywords: [developers, conflict-resolution, vclocks, vector-clocks]
---

One of Riak's central goals is high availability. It was built as a multi-node system in which any node is capable of receiving requests without requiring that each node participate in each request. In distributed systems like this, it's important to be able to keep track of which version of a value is the most current. This is where vector clocks come in.

## Client- and Server-side Conflict Resolution

One of the things that makes Riak's eventual consistency model powerful is Riak is very non-opinionated about how data resolution takes place. While Riak does have a set of [[defaults|Replication Properties#available-parameters]], there is a wide variety of ways that data inconsistency can be resolved. The following basic options are available:

* **Allowing Riak to resolve all conflicts**. If the `[[allow_mult|Conflict Resolution#siblings]]` parameter is set to `false`, conflict between data replicas can still take place, but Riak resolves those conflicts behind the scenes on the basis of [[vector clocks]]. This unburdens Riak clients from engaging in conflict resolution, delegating that responsibility to Riak itself. While this can ease the development process, it has the important drawback that applications cannot form their own deterministic merge logic.

  Another way to prevent conflicts is to set the `[[last_write_wins|Conflict Resolution#last-write-wins]]` parameter to `true` instead of `allow_mult`. The last-write-wins strategy means that conflicts will be resolved on the basis of which object has the most recent timestamp. While this can also ease the development process by guaranteeing that clients don't have to deal with siblings, using clock time as a resolution mechanism in a distributed system can lead to unpredictable results.

  <div class="note">
  <div class="title">Undefined behavior warning</div>
  Setting both <tt>allow_mult</tt> and <tt>last_write_wins</tt> to <tt>true</tt> necessarily leads to unpredictable behavior and should always be avoided.
  </div>

* **Allow Riak to form siblings and resolve conflicts on the application side**. If `allow_mult` is set to `true`, Riak will form [[siblings|Conflict Resolution#siblings]] if you write multiple objects to a key without providing Riak with a [[vector clock]] that would enable it to judge which object should be considered most up to date. When sibling objects are formed, applications need to decide which of the objects is "correct" (the definition of which depends on the application itself).

  An application can resolve sibling conflicts by [[passing a vector clock|Conflict Resolution#vector-clocks]] to Riak that shows which replica (or version) of an object that particular client has seen. Or, it can provide some other conflict resolution logic. You could specify, for example, that some objects cannot be "correct" because they're too large, their timestamps show them to be too old, the number of items in the [[shopping cart|Dynamo]] has too items, etc. In effect, the sky's the limit when applications are in control of conflict resolution.

Conflict resolution in Riak can be a complex business, but the presence of this variety of options means that all requests to Riak can always be made in accordance with your data model(s), business needs, and use cases.

## Vector Clocks and Relationships Between Objects

Vector clocks enable Riak to compare two objects stored in a specific location, as defined by the object's [[bucket|Buckets]] and [[key|Keys and Objects]], as well as the [[bucket type|Using Bucket Types]] defining the bucket's properties, and determine the relationship between the two objects. A number of important aspects of that relationship can be determined using vector clocks:

 * Whether one object is a direct descendant of the other
 * Whether the objects are direct descendants of a common parent
 * Whether the objects are unrelated in recent heritage

Using this knowledge, Riak can auto-repair out-of-sync data when feasible or at least provide a client with an opportunity to reconcile divergent changesets in an application-specific manner.

Vector clocks are non-human-readable and look something like this:

```
a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkzGNlsP/VfYYvCwA=
```

## Vector Clock Tagging

When a value is stored in Riak, it is tagged with a vector clock, establishing its initial version. Using this knowledge, Riak can auto-repair out-of-sync data when feasible or at least provide a client with an opportunity to reconcile divergent changesets in an application-specific manner.

## Siblings

A **sibling** is created when Riak is unable to resolve the canonical version of an object being stored. These scenarios can create siblings inside of a single object, usually under one of the following conditions:

1. **Concurrent writes** --- If two writes occur simultaneously from clients with the same vector clock value, Riak will not be able to determine the correct object to store, and the object will be given two siblings.  These writes could happen to the same node or to different nodes.

2. **Stale Vector Clock** --- Writes from any client using a stale vector clock
value. This is a less likely scenario from a well-behaved client that performs a read (to get the current vector clock) before a write. A situation
may occur, however, in which a write happens from a different client while the read/write cycle is taking place. This would cause the first client to issue the write with an old vector clock value and a sibling to be created. A misbehaving client could continually create siblings if it habitually issued writes with a stale vector clock.

3. **Missing Vector Clock** --- Writes to an existing object without a vector
clock. While the least likely scenario, it can happen when manipulating an
object using a client like `curl` and forgetting to set the `X-Riak-Vclock`
header or using a [[Riak client library|Client Libraries]] and failing to take advantage of vector clock-related functionality.

Riak uses siblings because it is impossible to order events with respect to
time in a distributed system, which means that they must be ordered causally. If `allow_mult` is set to `false` in the [[bucket type|Using Bucket Types]] that you are using, siblings and vector clocks don't need to be dealt with on the application side because Riak will never return siblings upon read.

If, however, `allow_mult` is set to `true`, Riak will not resolve conflicts for you, and the responsibility for conflict resolution will be delegated to the application, which will have to either select one of the siblings as being more correct or to delete or replace the object.

### Siblings in Action

Let's have a more concrete look at how siblings work in Riak. First, we'll create a bucket type called `siblings_allowed` with `allow_mult` set to `true`:

```bash
riak-admin bucket-type create siblings_allowed '{"props":{"allow_mult":true}}'
riak-admin bucket-type activate siblings_allowed
riak-admin bucket-type status siblings_allowed
```

If the type has been properly activated, the `status` command should return `siblings_allowed is active`. Now, we'll create two objects and write both of them to the same key without providing a vector clock:

```ruby
bucket = client.bucket('nickolodeon')
obj1 = Riak::RObject.new(bucket, 'best_character')
obj1.content_type = 'text/plain'
obj1.raw_data = 'Ren'
obj1.store(type: 'siblings_allowed')

obj2 = Riak::RObject.new(bucket, 'best_character')
obj2.content_type = 'text/plain'
obj2.raw_data = 'Stimpy'
obj2.store(type: 'siblings_allowed')
```

```java
Location bestCharacterKey = new Location("nickolodeon")
        .setBucketType("siblings_allowed")
        .setKey("best_character");
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

```python
bucket = client.bucket('nickolodeon', bucket_type='siblings_allowed')
obj1 = RiakObject(client, bucket, 'best_character')
obj1.content_type = 'text/plain'
obj1.data = 'Ren'
obj1.store()

obj2 = RiakObject(client, bucket, 'best_character')
obj2.content_type = 'text/plain'
obj2.data = 'Stimpy'
obj2.store()
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
curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "Ren" \
  http://localhost:8098/types/siblings_allowed/nickolodeon/whatever/keys/best_character

curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "Stimpy" \
  http://localhost:8098/types/siblings_allowed/nickolodeon/whatever/keys/best_character
```

### V-tags

At this point, multiple objects are stored in the same key. Let's see what happens if you try to read contents of the object:

```ruby
bucket = client.bucket('nickolodeon')
obj = bucket.get('best_character', type: 'siblings_allowed')
obj
```

```java
Location bestCharacterKey = new Location("nickolodeon")
        .setBucketType("siblings_allowed")
        .setKey("best_character");
FetchValue fetch = new FetchValue.Builder(bestCharacterKey).build();
FetchValue.Response response = client.execute(fetch);
RiakObject obj = response.getValue(RiakObject.class);
System.out.println(obj.getValue().toString());
```

```python
bucket = client.bucket('nickolodeon', bucket_type='siblings_allowed')
obj = bucket.get('best_character')
obj.siblings
```

```curl
curl http://localhost:8098/types/siblings_allowed/buckets/nickolodeon/keys/best_character
```

You should get the response:

```ruby
<Riak::RObject {nickolodeon,best_character} [#<Riak::RContent [text/plain]:"Ren">, #<Riak::RContent [text/plain]:"Stimpy">]>
```

```java
com.basho.riak.client.cap.UnresolvedConflictException: Siblings found
```

```python
[<riak.content.RiakContent object at 0x10a00eb90>, <riak.content.RiakContent object at 0x10a00ebd0>]
```

```curl
Siblings:
175xDv0I3UFCfGRC7K7U9z
6zY2mUCFPEoL834vYCDmPe
```

As you can see, reading an object with multiple values will result in some form of "multiple choices" response (e.g. `300 Multiple Choices` in HTTP).

If you want to view all objects using the HTTP interface, you can attach an `Accept: multipart/mixed` header to your request:

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

If you select the first of the two siblings and retrieve its value, you should see `Ren` and not `Stimpy`.

### Conflict Resolution Using Vector Clocks

Once you are presented with multiple options for a single value, you must
determine the correct value. In an application, this can be done either in an
automatic fashion, using a use case-specific resolver, or by presenting the conflicting objects to the end user. 

To update Riak with the appropriate value you will need the current vector
clock. Right now, there are replicas with two different values: `Ren` and `Stimpy`. Now, let's say that we decide that `Stimpy` is the correct value on the basis of our application's use case. In order to resolve the conflict, we need to fetch the object's vector clock and then write the correct value to the key _while passing the fetched vector clock to Riak_:

```ruby
bucket = client.bucket('nickolodeon')
obj = bucket.get('best_character', type: 'siblings_allowed')
vclock = obj.vclock
new_obj = Riak::RObject.new(bucket, 'best_character')
new_obj.content_type = 'text/plain'
new_obj.raw_data = 'Stimpy'
new_obj.store(type: 'siblings_allowed', vclock: vclock)
```

```java
Location bestCharacterKey = new Location("nickolodeon")
        .setBucketType("siblings_allowed")
        .setKey("best_character");
FetchValue fetch = new FetchValue.Builder(bestCharacterKey).build();
FetchValue.Response res = client.execute(fetch);
VClock vClock = res.getVClock();
RiakObject newObj = new RiakObject()
        .withValue(BinaryValue.create("Stimpy"));
StoreValue store = new StoreValue.Builder(newObj)
        .withLocation(bestCharacterKey)
        .withVectorClock(vClock);
client.execute(store);
```

```python
bucket = client.bucket('nickolodeon', bucket_type='siblings_allowed')
obj = bucket.get('best_character')
vclock = obj.vclock
new_obj = Riak::RObject.new(bucket, 'best_character')
new_obj.content_type = 'text/plain'
new_obj.data = 'Stimpy'
new_obj.store(vclock=vclock)
```

```curl
curl -i http://localhost:8098/types/siblings_allowed/buckets/nickolodeon/keys/best_character

# In the HTTP interface, the vector clock can be found in the "X-Riak-Vclock" header. That will look something like this:

X-Riak-Vclock: a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkzGNlsP/VfYYvCwA=
```

In Riak clients, you can access the vector clock as part of the object.

Using the vector clock, you can then write the correct value to the `character` key, passing the vector clock to Riak as a header:

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -H "X-Riak-Vclock: a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkzGNlsP/VfYYvCwA=" \
  -d "stimpy" \
  http://localhost:8098/types/siblings_allowed/buckets/nickolodeon/keys/best_character
```

<div class="note">
<div class="title">Concurrent conflict resolution</div>
It should be noted that if you are trying to resolve conflicts automatically,
you can end up in a condition in which two clients are simultaneously
resolving and creating new conflicts. To avoid a pathological divergence, you
should be sure to limit the number of reconciliations and fail once that limit
has been exceeded.
</div>

### Sibling Explosion

Sibling explosion occurs when an object rapidly collects siblings without being reconciled. This can lead to myriad issues. Having an enormous object in your node can cause reads of that object to crash the entire node. Other issues include increased cluster latency as the object is replicated and out-of-memory errors.

### Vector Clock Explosion

Besides sibling explosion, the vector clock can grow extremely large when a
significant volume of updates are performed on a single object in a small
period of time. While updating a single object _extremely_ frequently is not
recommended, you can tune Riak's vector clock pruning to prevent vector clocks
from growing too large too quickly.

### How does `last_write_wins` affect resolution?

On the surface, it seems like setting `allow_mult` to `false` (the default)
and `last_write_wins` to `true` would result in the same behavior, but
there is a subtle distinction.

Even though both settings return only one value to the client, setting `allow_mult` to `false` still uses vector clocks for resolution, whereas if `last_write_wins` is `true`, Riak reads the timestamp to determine the latest version. Deeper in the system, if `allow_mult` is `false`, Riak will still allow siblings to exist when they are created (via concurrent writes or network partitions), whereas setting `last_write_wins` to `true` means that Riak will overwrite the value with the one that has the later timestamp.

When you don't care about sibling creation, setting `allow_mult` to `false` has the least surprising behavior: you get the latest value, but network partitions are handled gracefully. However, for cases in which keys are rewritten often (and quickly) and the new value isn't necessarily dependent on the old value, `last_write_wins` will provide better performance. Some use cases where you might want to use `last_write_wins` include caching, session storage, and insert-only (no updates).

<div class="note">
The combination of setting both the <tt>allow_mult</tt> and <tt>last_write_wins</tt> properties to <tt>true</tt> leads to undefined behavior and should not be used.
</div>

## Vector Clock Pruning

Riak regularly prunes vector clocks to prevent overgrowth based on four parameters which can be set in any bucket type that you create:

Parameter | Default value | Description
:---------|:--------------|:-----------
`small_vclock` | `50` | If the length of the vector clock list is smaller than this value, the list will not be pruned
`big_vclock` | `50` | If the length of the vector clock list is larger than this value, the list will be pruned
`young_vclock` | `20` | If a vector clock is younger than this value (in milliseconds), it will not be pruned.
`old_vclock` | `86400` (one day) | If a vector clock is older than this value (in milliseconds), it will be pruned

This diagram shows how the values of these parameters dictate the vector clock pruning process:

![Vclock Pruning](/images/vclock-pruning.png)

## More Information

Additional background information on vector clocks:

* [[Vector Clocks on Wikipedia|http://en.wikipedia.org/wiki/Vector_clock]]
* [[Why Vector Clocks are Easy|http://blog.basho.com/2010/01/29/why-vector-clocks-are-easy/]]
* [[Why Vector Clocks are Hard|http://blog.basho.com/2010/04/05/why-vector-clocks-are-hard/]]
* The vector clocks used in Riak are based on the [[work of Leslie Lamport|http://portal.acm.org/citation.cfm?id=359563]].
