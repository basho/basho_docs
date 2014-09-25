---
title: Conflict Resolution
project: riak
version: 2.0.0+
document: tutorials
audience: intermediate
keywords: [developers, conflict-resolution, vclocks, vector-clocks]
---

One of Riak's central goals is high availability. It was built as a
multi-node system in which any [[node|Riak Glossary#node]] is capable of
receiving requests without requiring that each node participate in each
request.

If you are using Riak in an [[eventually consistent|Eventual
Consistency]] way, conflicts between object values on different nodes is
unavoidable. Often, Riak can resolve these conflicts on its own
internally, especially if you use [[vector clocks]] when updating
objects. Instructions on using vector clocks can be found in the section
[[below|Conflict Resolution#Siblings]].

But even when you use [[vector clocks]] \(or their close equivalent,
[[dotted version vectors]]), Riak cannot always decide which value is
most causally recent, especially in cases involving concurrent updates.
How Riak behaves in that case is your choice. A list of options is
available in the section on client- and server-side resolution
[[below|Conflict Resolution#Client-and-Server-side-Resolution]].

In general, we recommend one of the following options:

1. If your data can be modeled as one of the currently available [[Riak
   Data Types|Data Types]], we recommend using one of types, because all
   of them have conflict resolution _built in_, completely relieving
   applications of the need to engage in conflict resolution.
2. If your data cannot be modeled as one of the available Data Types,
   we recommend allowing Riak to generate [[siblings|Conflict
   Resolution#Siblings]] and to resolve conflicts on the application
   side. Developing your own **conflict resolution strategy** can be
   tricky, but it has clear advantages over other approaches. While it's
   difficult to generalize about resolution strategies, we offer an
   example [[later in this document|Conflict
   Resolution#Sibling-Resolution-Example]].

<div class="note">
<div class="title">Note on strong consistency</div>
In versions of Riak 2.0 and later, you have the option of using Riak in
a strongly consistent fashion. This document pertains to usage of Riak
as an <em>eventually</em> consistent system. If you'd like to use Riak's
strong consistency feature, please refer to the following documents:

* [[Using Strong Consistency]] --- A guide for developers<br />
* [[Managing Strong Consistency]] --- A guide for operators<br />
* [[Strong Consistency]] --- A theoretical treatment of strong consistency
</div>

## Client- and Server-side Conflict Resolution

Riak's eventual consistency model is powerful because Riak is
fundamentally non-opinionated about how data resolution takes place.
While Riak _does_ have a set of [[defaults|Replication
Properties#available-parameters]], there is a variety of general
approaches to conflict resolution that are available. In Riak, you can
mix and match conflict resolution strategies at the bucket level,
[[using bucket types]]. The most important [[bucket properties|Buckets]]
to consider when reasoning about conflict resolution are the
`allow_mult` and `last_write_wins` properties.

These properties provide you with the following basic options:

### Timestamp-based Resolution

If the `[[allow_mult|Conflict Resolution#siblings]]` parameter is set to
`false`, Riak resolves all object replica conflicts internally and does
not return siblings to the client. How Riak resolves those conflicts
depends on the value that you set for a different bucket property,
`[[last_write_wins|Buckets]]`. If `last_write_wins` is set to `false`,
Riak will resolve all conflicts on the basis of
[timestamps](http://en.wikipedia.org/wiki/Timestamp), which are
attached to all Riak objects as metadata.

The problem with timestamps is that they are not a reliable resolution
mechanism in distributed systems, and they always bear the risk of data
loss. A better yet still-problematic option is to adopt a
last-write-wins strategy, described directly below.

### Last-write-wins

Another way to manage conflicts is to set `allow_mult` to `false`, as
with timestamp-based resolution, and the `[[last_write_wins|Conflict
Resolution#last-write-wins]]` parameter to `true`. This produces a
so-called last-write-wins (LWW) strategy whereby all conflicts are
resolved internally in Riak on the basis of [[vector clocks]]. While
this strategy is preferred to timestamp-based strategies because it
takes object update causality into account instead of timestamps, it
still bears the risk that writes will be lost.

The problem is that LWW will necessarily drop some writes in the case of
concurrent updates in the name of preventing sibling creation. If your
use case requires that your application be able to reason about
differing values produced in the case of concurrent updates, then we
advise against LWW as a conflict resolution strategy.

<div class="note">
<div class="title">Undefined behavior warning</div>
Setting both <code>allow_mult</code> and <code>last_write_wins</code> to
<code>true</code> necessarily leads to unpredictable behavior and should
always be avoided.
</div>

### Resolve Conflicts on the Application Side

While setting `allow_mult` to `false` unburdens applications from having
to reason about siblings, delegating that responsibility to Riak itself,
it bears all of the drawbacks explained above. On the other hand,
setting `allow_mult` to `true` has the following benefits:

* Riak will retain writes even in the case of concurrent updates to a
  key, which enables you to capture the benefits of high availability
  with a lower risk of data loss
* If your application encounters siblings, it can apply its own
  use case-specific conflict resolution logic

Conflict resolution in Riak can be a complex business, but the presence
of this variety of options means that all requests to Riak can always be
made in accordance with your data model(s), business needs, and use
cases. For an example of client-side sibling resolution, see the
[[section below|Conflict Resolution#Sibling-Resolution-Example]].

In Riak versions 2.0 and later, `allow_mult` is set to `true` by default
for any [[bucket types|Using Bucket Types]] that you create. If you wish
to avoid client-side sibling resolution, you have a few options:

* Explicitly create and activate [[bucket types|Using Bucket Types]]
  that set `allow_mult` to `false`.
* Use Riak's [[configuration files]] to change the [[default bucket
  properties|Configuration Files#Default-Bucket-Properties]] for your
  cluster. If you set the `buckets.default.allow_mult` parameter to
  `false`, all bucket types that you create will have `allow_mult` set
  to `false` by default.

## Vector Clocks and Relationships Between Objects

When a value is stored in Riak, it is tagged with a vector clock,
establishing its initial version. That vector clock changes value over
time if the object is updated.

Vector clocks enable Riak to compare objects stored in a key and
determine a number of important aspects of the relationship between the
objects:

 * Whether one object is a direct descendant of the other
 * Whether the objects are direct descendants of a common parent
 * Whether the objects are unrelated in recent heritage

Using this knowledge, Riak is frequently, though not always, able to
resolve conflicts without producing siblings.

Vector clocks are non-human-readable and look something like this:

```
a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkzGNlsP/VfYYvCwA=
```

If `allow_mult` is set to `true`, you should _always_ [[use vector
clocks|Conflict Resolution#Siblings]] when updating objects, _unless
you are certain that no object exists under that key_. Failing to use
vector clocks can lead to [[sibling explosion|Latency
Reduction#Siblings]], which can produce a variety of problems in your
cluster.

## Siblings

A **sibling** is created when Riak is unable to resolve the canonical
version of an object being stored. The following scenarios can create
siblings inside of a single object:

1. **Concurrent writes** --- If two writes occur simultaneously from
   clients with the same vector clock value, Riak will not be able to
   choose a single value to store, in which case the object will be
   given a sibling. These writes could happen on the same node or on
   different nodes.

2. **Stale Vector Clock** --- Writes from any client using a stale
   vector clock value. This is a less likely scenario from a
   well-behaved client that performs a read (to fetch the current vector
   clock) before performing a write. A situation may occur, however, in
   which a write happens from a different client while the read/write
   cycle is taking place. This would cause the first client to issue the
   write with an old vector clock value and a sibling to be created. A
   misbehaving client could continually create siblings if it habitually
   issued writes with a stale vector clock.

3. **Missing Vector Clock** --- Writes to an existing object without a
   vector clock. While the least likely scenario, it can happen when
   manipulating an object using a client like `curl` and forgetting to
   set the `X-Riak-Vclock` header or using a [[Riak client
   library|Client Libraries]] and failing to take advantage of vector
   clock-related functionality.

### Siblings in Action

Let's have a more concrete look at how siblings work in Riak. First,
we'll create a bucket type called `siblings_allowed` with `allow_mult`
set to `true`:

```bash
riak-admin bucket-type create siblings_allowed '{"props":{"allow_mult":true}}'
riak-admin bucket-type activate siblings_allowed
riak-admin bucket-type status siblings_allowed
```

If the type has been properly activated, the `status` command should
return `siblings_allowed is active`. Now, we'll create two objects and
write both of them to the same key without first fetching the object
(which obtains the vector clock):

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

<div class="note">
<div class="title">Getting started with Riak clients</div>
If you are connecting to Riak using one of Basho's official
[[client libraries]], you can find more information about getting
started with your client in our [[quickstart
guide|Five-Minute Install#setting-up-your-riak-client]].
</div>

### V-tags

At this point, multiple objects are stored in the same key. Let's see
what happens if we try to read contents of the object:

```java
Location bestCharacterKey =
  new Location(new Namespace("siblings_allowed", "nickolodeon"), "best_character");

FetchValue fetch = new FetchValue.Builder(bestCharacterKey).build();
FetchValue.Response response = client.execute(fetch);
RiakObject obj = response.getValue(RiakObject.class);
System.out.println(obj.getValue().toString());
```

```ruby
bucket = client.bucket('nickolodeon')
obj = bucket.get('best_character', type: 'siblings_allowed')
obj
```

```python
bucket = client.bucket_type('siblings_allowed').bucket('nickolodeon')
obj = bucket.get('best_character')
obj.siblings
```

```curl
curl http://localhost:8098/types/siblings_allowed/buckets/nickolodeon/keys/best_character
```

We should get this response:

```java
com.basho.riak.client.cap.UnresolvedConflictException: Siblings found
```

```ruby
<Riak::RObject {nickolodeon,best_character} [#<Riak::RContent [text/plain]:"Ren">, #<Riak::RContent [text/plain]:"Stimpy">]>
```

```python
[<riak.content.RiakContent object at 0x10a00eb90>, <riak.content.RiakContent object at 0x10a00ebd0>]
```

```curl
Siblings:
175xDv0I3UFCfGRC7K7U9z
6zY2mUCFPEoL834vYCDmPe
```

As you can see, reading an object with multiple values will result in
some form of "multiple choices" response (e.g. `300 Multiple Choices` in
HTTP).

If you want to view all objects using the HTTP interface, you can attach
an `Accept: multipart/mixed` header to your request:

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

### Conflict Resolution Using Vector Clocks

Once you are presented with multiple options for a single value, you
must determine the correct value. In an application, this can be done
either in an automatic fashion, using a use case-specific resolver, or
by presenting the conflicting objects to the end user.

At the moment, we have two replicas with two different values, one with
`Ren`, the other with `Stimpy`. But let's say that we decide that
`Stimpy` is the correct value based on our application's use case. In
order to resolve the conflict, we need to do three things:

1. Fetch the current object (which will return both siblings)
2. Modify the value of the object, i.e. make the value `Stimpy`
3. Write the object back to the `best_character` key

What happens when we fetch the object first, prior to the update, is
that the object handled by the client has a vector clock attached. At
that point, we can modify the object's value, and when we write the
object back to Riak, _the vector clock will automatically be attached
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

# Then we modify the object's value
new_obj.data = 'Stimpy'

# Then we store the object, which has the vector clock already attached
new_obj.store(vclock=vclock)
```

```curl
curl -i http://localhost:8098/types/siblings_allowed/buckets/nickolodeon/keys/best_character

# In the HTTP interface, the vector clock can be found in the "X-Riak-Vclock" header. That will look something like this:

X-Riak-Vclock: a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkzGNlsP/VfYYvCwA=

# When performing a write to the same key, that same header needs to
# accompany the write for Riak to be able to use the vector clock
```

<div class="note">
<div class="title">Concurrent conflict resolution</div>
It should be noted that if you are trying to resolve conflicts
automatically, you can end up in a condition in which two clients are
simultaneously resolving and creating new conflicts. To avoid a
pathological divergence, you should be sure to limit the number of
reconciliations and fail once that limit has been exceeded.
</div>

### Sibling Explosion

Sibling explosion occurs when an object rapidly collects siblings
without being reconciled. This can lead to myriad issues. Having an
enormous object in your node can cause reads of that object to crash
the entire node. Other issues include [[increased cluster
latency|Latency Reduction Checklist]] as the object is replicated and
out-of-memory errors.

### Vector Clock Explosion

Besides sibling explosion, the vector clock itself can grow extremely
large when a significant volume of updates are performed on a single
object in a small period of time. While updating a single object
_extremely_ frequently is not recommended, you can tune Riak's vector
clock pruning to prevent vector clocks from growing too large too
quickly. More on pruning in the [[section below|Conflict
Resolution#vector-clock-pruning]].

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

<div class="note">
The combination of setting both the <code>allow_mult</code> and
<code>last_write_wins</code> properties to <code>true</code> leads to
undefined behavior and should not be used.
</div>

## Vector Clock Pruning

Riak regularly prunes vector clocks to prevent overgrowth based on four
parameters which can be set for any bucket type that you create:

Parameter | Default value | Description
:---------|:--------------|:-----------
`small_vclock` | `50` | If the length of the vector clock list is smaller than this value, the list's entries will not be pruned.
`big_vclock` | `50` | If the length of the vector clock list is larger than this value, the list will be pruned
`young_vclock` | `20` | If a vector clock entry is younger than this value (in milliseconds), it will not be pruned.
`old_vclock` | `86400` (one day) | If a vector clock entry is older than this value (in milliseconds), it will be pruned

This diagram shows how the values of these parameters dictate the vector
clock pruning process:

![Vclock Pruning](/images/vclock-pruning.png)

## Sibling Resolution Example

For reasons explained in the sections above, we strongly recommend
adopting a conflict resolution strategy that requires applications to
resolve siblings according to use-case-specific criteria. Here, we'll
provide an example using code samples in a variety of languages. The
Java client handles conflict resolution a little bit differently, so
Java examples will be in a separate section below the other language
samples. Ruby and Python examples can be found immediately below.

Let's say that we're building a social network application and storing
lists of usernames representing each user's "friends." All of the data
for our application will be stored in buckets that bear the [[bucket
type|Using Bucket Types]] `siblings`. In this bucket type, `allow_mult`
is set to `true`, which means that Riak will generate siblings in
certain cases, siblings that are application will need to be set up to
resolve when they arise.

As explained above, our application is storing objects that consist of
lists of usernames. The question we now need to ask ourselves is this:
if an object has siblings, which of the lists should be deemed correct?
Let's keep it simple here for now and say that the following criterion
will hold: if two lists are compared, the longer list will be considered
"correct." While this might not make sense in real-world applications,
it's a good jumping-off point.

First, let's create a `User` type for storing our data. Each `User`
object will house a `friends` property that lists the usernames of that
user's friends. We'll also create a method to turn `User` objects into
JSON (since we'll be storing them all as JSON).

```ruby
class User
  def initialize(friends)
    @friends = friends
  end

  def to_json
    return {
      :friends => @friends
    }.to_json
end
```

```python
class User:
    def __init__(self, friends):
        self.friends = friends

    def to_json(self):
        return vars(self)
```

Now, we can create `User` objects and see what they look like as JSON:

```ruby
bashobunny = User.new(['captheorem238', 'siblingsrule572'])
bashobunny.to_json

# {:friends=>["captheorem238", "siblingsrule572"]}
```

```python
bashobunny = User(['captheorem238', 'siblingsrule572'])

bashobunny.to_json()
# {'friends': ['captheorem238', 'siblingsrule572']}
```

Let's say that we've stored a bunch of `User` objects in Riak, and that
a few concurrent writes have led to siblings. How is our application
going to deal with that? Let's say that there's a `User` object stored
in the bucket `users` (which is of the bucket type `siblings`, as noted
above) under the key `bashobunny`. First, we can fetch the object that
is stored there and see if it has siblings:

```ruby
obj = client.bucket('users').get('bashobunny')

if obj.siblings.length > 1
  # perform resolution operation
end
```

```python
obj = client.bucket('users').get('bashobunny')

if len(obj.siblings) > 1:
    # perform resolution operation
```

```python
obj = client.bucket_type('siblings').bucket('users').get('bashobunny')

if len(obj.siblings) > 1:

```

### Java Example

The official [Riak Java
client](https://github.com/basho/riak-java-client) works somewhat
differently from the others because it provides a `ConflictResolver`
interface that requires you to implement a `resolve` method that will
return a single correct value of the type of object that you are using.
In this example, we'll create a standard POJO class `User`. Each `User`
object will have one property: a list of strings in which each string is
the username of one of that user's friends.

```java
public class User {
    public List<String> friends;

    public User(List<String> friends) {
        this.friends = friends;
    }
}

// An example of instantiating a new User object
List<String> friends = new LinkedList<String>();
friends.add("fred");
User bashobunny = new User(friends);
```

So what happens if there are siblings and the user `bashobunny` has
different friend lists in different object replicas? The Java client
allows you to implement a `ConflictResolver` interface that enables you
to create your own resolution logic. The `resolve` method takes a Java
`List` of objects and must return a single object of that type, in this
case `User`. The example resolver below will return `null` if there are
no object replicas available, will return the lone value if only one
replica is present, and if there is more than one replica present (i.e.
if there are siblings), it will iterate through the existing siblings to
determine which `User` object has the longest `friends` list.

```java
import com.basho.riak.client.api.cap.ConflictResolver;

public class UserResolver implements ConflictResolver<User> {
    @Override
    public User resolve(List<User> siblings) {
        if (siblings.size == 0) {
            return null;
        } else if (siblings.size == 1) {
            return siblings.get(0);
        } else {
            int longestList = 0;
            User userWithLongestList;
            for (User user : siblings) {
                if (user.friends.size > longestList) {
                    userWithLongestList = user;
                    longestList = user.friends.size;
                } else {
                    return siblings.get(0);
                }
            }
            return userWithLongestList;
        }
    }
}
```

To use a conflict resolver, you must register it:

```java
ConflictResolverFactory factory = ConflictResolverFactory.getInstance();
ConflictResolver<User> userResolver = factory.getConflictResolver(User.class);
```

With the resolver registered, the resolution logic you have chosen will
resolve siblings automatically upon read. Resolver registration can
occur 

## More Information

Additional background information on vector clocks:

* [Vector Clocks on Wikipedia](http://en.wikipedia.org/wiki/Vector_clock)
* [Why Vector Clocks are Easy](http://blog.basho.com/2010/01/29/why-vector-clocks-are-easy/)
* [Why Vector Clocks are Hard](http://blog.basho.com/2010/04/05/why-vector-clocks-are-hard/)
* The vector clocks used in Riak are based on the [work of Leslie Lamport](http://portal.acm.org/citation.cfm?id=359563)
