---
title: Vector Clocks
project: riak
version: 0.10.0+
document: appendix
toc: true
audience: intermediate
keywords: [appendix, concepts]
moved: {
  '1.4.0-': '/references/appendices/concepts/Vector-Clocks'
}
---

## Overview

One of Riak's central goals is high availability. It was built as a multi-node system in which any node is capable of receiving requests without requiring that each node participate in each request. In a system like this, it's important to be able to keep track of which version of a value is the most current. This is where vector clocks come in.

When a value is stored in Riak, it is tagged with a vector clock, establishing its initial version. They are non-human-readable and look something like this:

```
a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkzGNlsP/VfYYvCwA=
```

For each update, the vector clock is extended in such a way that Riak can later compare two versioned replicas of the object and determine the following:

 * Whether one object is a direct descendant of the other
 * Whether the objects are direct descendants of a common parent
 * Whether the objects are unrelated in recent heritage

Using this knowledge, Riak can auto-repair out-of-sync data when feasible or at least provide a client with an opportunity to reconcile divergent changesets in an application-specific manner.

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
time in a distributed system, which means that they must be ordered causally. If `allow_mult` is set to `false` {{#2.0.0-}}on a bucket{{/2.0.0-}}{{#2.0.0+}}in the [[bucket type|Using Bucket Types]] that you are using{{/2.0.0+}}, siblings and vector clocks don't need to be dealt with on the application side because Riak will never return siblings upon read.

If, however, `allow_mult` is set to `true`, Riak will not resolve conflicts for you, and the responsibility for conflict resolution will be delegated to the application, which will have to either select one of the siblings as being more correct or to delete or replace the object.

### Siblings in Action

Let's have a more concrete look at how siblings work in Riak. First, we'll create a {{#2.0.0-}}bucket called `siblings_bucket`{{/2.0.0-}}{{#2.0.0+}}bucket type called `siblings_allowed`{{/2.0.0+}} with `allow_mult` set to `true`:

{{#2.0.0-}}

```curl
curl -XPUT \
  -H "Content-Type: application/json" \
  -d '{"props":{"allow_mult":true}}' \
  http://localhost:8098/buckets/siblings_bucket/props
```
{{/2.0.0-}}
{{#2.0.0+}}

```bash
riak-admin bucket-type create siblings_allowed '{"props":{"allow_mult":true}}'
riak-admin bucket-type activate siblings_allowed
riak-admin bucket-type status siblings_allowed

# If the type has been properly activated, the 'status' command should
# return 'siblings_allowed is active'
```
{{/2.0.0+}}

Now, we'll create two objects and write both of them to the same key without providing a vector clock:

{{#2.0.0-}}

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "ren" \
  http://localhost:8098/buckets/siblings_bucket/keys/character

curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "stimpy" \
  http://localhost:8098/buckets/siblings_bucket/keys/character
```
{{/2.0.0-}}
{{#2.0.0+}}

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "ren" \
  http://localhost:8098/types/siblings_allowed/buckets/whatever/keys/character

curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "stimpy" \
  http://localhost:8098/types/siblings_allowed/buckets/whatever/keys/character
```
{{/2.0.0+}}

### V-Tags

At this point, multiple objects are stored in the same key. Let's see what happens if you try to read contents of the object:

{{#2.0.0-}}

```curl
curl http://127.0.0.1:8098/buckets/siblings_bucket/keys/character
```
{{/2.0.0-}}
{{#2.0.0+}}

```curl
curl http://localhost:8098/types/siblings_allowed/buckets/whatever/keys/character
```
{{/2.0.0+}}

You should get the response:

```curl
Siblings:
175xDv0I3UFCfGRC7K7U9z
6zY2mUCFPEoL834vYCDmPe
```

As you can see, reading an object with multiple values will result in some form of "multiple choices" response (e.g. `300 Multiple Choices` in HTTP).

You also have the option of viewing all objects currently stored under the `character` key at once:

{{#2.0.0-}}

```curl
curl -H "Accept: multipart/mixed" \
  http://localhost:8098/buckets/siblings_bucket/keys/character
```
{{/2.0.0-}}
{{#2.0.0+}}

```curl
curl -H "Accept: multipart/mixed" \
  http://localhost:8098/types/siblings_allowed/buckets/whatever/keys/character
```
{{/2.0.0+}}

Response (without headers):

```
ren
--WUnzXITIPJFwucNwfdaofMkEG7H

stimpy
--WUnzXITIPJFwucNwfdaofMkEG7H--
```

If you select the first of the two siblings and retrieve its value, you should see `ren` and not `stimpy`.

### Conflict Resolution

Once you are presented with multiple options for a single value, you must
determine the correct value. In an application, this can be done either in an
automatic fashion, using a use case-specific resolver, or by presenting the conflicting objects to the end user. 

To update Riak with the appropriate value you will need the current vector
clock. Right now, there are replicas with two different values: `ren` and `stimpy`. Let's say that we decide that `stimpy` is the correct value on the basis of our application's use case. In order to resolve the conflict, you need to fetch the object's vector clock and then write the correct value to the key _while passing the fetched vector clock to Riak_:

{{#2.0.0-}}

```curl
curl -i http://localhost:8098/buckets/siblings_bucket/keys/character
```
{{/2.0.0-}}
{{#2.0.0+}}

```curl
curl -i http://localhost:8098/types/siblings_allowed/buckets/siblings_bucket/keys/character
```
{{/2.0.0+}}

The vector clock can be found in the `X-Riak-Vclock` header. That will look something like this:

```
X-Riak-Vclock: a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkzGNlsP/VfYYvCwA=
```

Using the vector clock, you can then write the correct value to the `character` key, passing the vector clock to Riak as a header:

{{#2.0.0-}}

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -H "X-Riak-Vclock: a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkzGNlsP/VfYYvCwA=" \
  -d "stimpy" \
  http://localhost:8098/buckets/siblings_bucket/keys/character
```
{{/2.0.0-}}
{{#2.0.0+}}

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -H "X-Riak-Vclock: a85hYGBgzGDKBVIcR4M2cgczH7HPYEpkzGNlsP/VfYYvCwA=" \
  -d "stimpy" \
  http://localhost:8098/types/siblings_allowed/buckets/siblings_bucket/keys/character
```
{{/2.0.0+}}

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

Even though both settings return only one value to the client, setting `allow_mult` to `false` still uses vector clocks for resolution, whereas if `last_write_wins` is `true`, Riak reads the timestamp to determine the latest version. Deeper in the system, if `allow_mult` is `false`, Riak will still allow siblings to exist when they are created (via concurrent writes or network partitions), whereas setting `last_write_wins` to `true` means that Riak will simply overwrite the value with the one that has the later timestamp.

When you don't care about sibling creation, setting `allow_mult` to `false` has the least surprising behavior: you get the latest value, but network partitions are handled gracefully. However, for cases in which keys are rewritten often (and quickly) and the new value isn't necessarily dependent on the old value, `last_write_wins` will provide better performance. Some use cases where you might want to use `last_write_wins` include caching, session storage, and insert-only (no updates).

<div class="note">
The combination of setting both the <tt>allow_mult</tt> and <tt>last_write_wins</tt> properties to <tt>true</tt> leads to undefined behavior and should not be used.
</div>

## Vector Clock Pruning

Riak regularly prunes vector clocks to prevent overgrowth based on four parameters which can be set per bucket:

 * `small_vclock`
 * `big_vclock`
 * `young_vclock`
 * `old_vclock`

The `small_vclock` and `big_vclock` parameters refer to the length of
the vector clock list. If the length of the list is smaller than
`small_vclock` it will not be pruned. If the length is greater than
`big_vclock` it will be pruned.

![Vclock Pruning](/images/vclock-pruning.png)

The `young_vclock` and `old_vclock` parameters refer to a timestamp
stored with each vclock entry. If the list length is between
`small_vclock` and `big_vclock` the age of each entry is checked. If
the entry is younger than `young_vclock` it is not pruned. If the
entry is older than `old_vclock` than it is pruned.

## More Information

Additional background information on vector clocks:

* [[Vector Clocks on Wikipedia|http://en.wikipedia.org/wiki/Vector_clock]]
* [[Why Vector Clocks are Easy|http://blog.basho.com/2010/01/29/why-vector-clocks-are-easy/]]
* [[Why Vector Clocks are Hard|http://blog.basho.com/2010/04/05/why-vector-clocks-are-hard/]]
* The vector clocks used in Riak are based on the [[work of Leslie Lamport|http://portal.acm.org/citation.cfm?id=359563]].
