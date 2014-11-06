---
title: Using Strong Consistency
project: riak
version: 2.0.0+
document: guide
toc: true
audience: advanced
keywords: [developers, strong-consistency]
---

<div class="note">
<div class="title">Note on commercial support</div>
Riak's strong consistency feature is currently an open-source-only
feature and is not yet commercially supported.
</div>

In versions 2.0 and later, Riak allows you to create buckets that
provide [[strong consistency]] guarantees for the data stored within
them, enabling you to use Riak as a CP system (consistent plus partition
tolerant) for all of the data in that bucket. You can store just some of
your data in strongly consistent buckets or all of your data, depending
on your use case. Strong consistency was added to complement Riak's
standard [[eventually consistent|Eventual Consistency]], high
availability mode.

## Tradeoffs

When data is stored in a bucket with strong consistency guarantees, a
value is guaranteed readable by any client _immediately_ after a
successful write has occurred to a given key. In this sense, single-key
strongly consistent operations are atomic, and operations on a given key
are [linearizable](http://en.wikipedia.org/wiki/Linearizability). This
behavior comes at the expense of availability because a [[quorum|Strong
Consistency#Trade-offs]] of primary [[vnodes|Riak Glossary#vnode]]
responsible for the key must be online and reachable or the request will
fail.

This trade-off is unavoidable for strongly consistent data, but the
[choice is now yours](http://en.wikipedia.org/wiki/CAP_theorem) to make.

## Enabling Strong Consistency

Complete instructions on enabling strong consistency can be found in
our documentation on [[strong consistency for operators|Managing Strong
Consistency#Enabling-Strong-Consistency]].

## Creating Consistent Bucket Types

[[Strong consistency]] requirements in Riak are applied on a
bucket-by-bucket basis, meaning that you can use some buckets in an
eventually consistent fashion and others in a strongly consistent
fashion, depending on your use case.

To apply strong consistency to a bucket, you must create a [[bucket
type|Using Bucket Types]] that sets the `consistent` bucket property to
`true`, activate that type, and then apply that type to specific
bucket/key pairs.

To give an example, we'll create a bucket type called
`strongly_consistent` with the `consistent` bucket property set to
`true`:

```bash
riak-admin bucket-type create strongly_consistent \
    '{"props":{"consistent":true}}'
```

<div class="note">
<div class="title">Note on bucket type names</div>
You can name [[bucket types|Using Bucket Types]] whatever you wish, with
the exception of `default`, which is a reserved term (a full listing of
the properties associated with the `default` bucket type can be found in
the documentation on [[bucket properties and operations|The
Basics#Bucket-Properties-and-Operations]]).
</div>

Once the `strongly_consistent` bucket type has been created, we can
check the status of the type to ensure that it has propagated through
all nodes and is thus ready to be activated:

```bash
riak-admin bucket-type status strongly_consistent
```

If the console outputs `strongly_consistent has been created and may be
activated` and the properties listing shows that `consistent` has been
set to `true`, then you may proceed with activation:

```bash
riak-admin bucket-type activate strongly_consistent
```

When activation is successful, the console will return the following:

```bash
strongly_consistent has been activated
```

Now, any bucket that bears the type `strongly_consistent`---or whatever
you wish to name it---will provide strong consistency guarantees.

Elsewhere in the Riak docs, you can find more information on [[using
bucket types]], on the concept of [[strong consistency]], and on strong
consistency [[for operators|Managing Strong Consistency]].

## Replication Properties

Strongly consistent operations in Riak function much differently from
their [[eventually consistent|Eventual Consistency]] counterparts.
Whereas eventually consistent operations enable you to set values for a
variety of [[replication properties]] either on each request or at the
bucket level, [[using bucket types]], these settings are quietly ignored
for strongly consistent operations. These settings include `r`, `pr`,
`w`, `rw`, and others. Two replication properties that _can_ be set,
however, are `n_val` and `return_body`.

The `n_val` property is extremely important for two reasons:

1. It dictates how fault tolerant a strongly consistent bucket is. More
   information can be found in [[our recommendations for
   operators|Managing Strong Consistency#Fault-Tolerance]].
2. Once the `n_val` property is set for a given bucket type, it cannot
   be changed. If you wish to change the `n_val` for one or more
   strongly consistent buckets [[using bucket types]], you will need to
   create a new bucket type with the desired `n_val`.

We also recommend setting the `n_val` on strongly consistent buckets to
at least 5. More on why we make this recommendation can be found in
[[Fault Tolerance|Managing Strong Consistency#Fault-Tolerance]].

## Object Context

Riak uses [[causal context]] to determine the causal history of objects.
In versions of Riak prior to 2.0, [[vector clocks|Causal
Context#Vector-Clocks]] were used to provide objects with causal context
metadata.  In Riak versions 2.0 and later, there is an option to use
[[dotted version vectors]], which function much like vector clocks from
the standpoint of clients, but with important advantages over vector
clocks.

While we strongly recommend attaching context to objects for all
updates---whether traditional vector clocks or the newer dotted version
vectors---they are purely [[optional|Conflict Resolution]] for all
eventually consistent operations in Riak. This is not the case for
strongly consistent operations. **When modifying strongly consistent
objects in Riak, you _must_ attach a context object**.

If you attempt to modify a strongly consistent object without attaching
a context to the request, the request will always fail. And while it is
possible to make writes to non-existing keys without attaching context,
we recommend doing this only if you are certain that the key does not
yet exist.

Instructions on using context objects can be found in our documentation
on [[object updates]].

## Strongly Consistent Writes

Writing to strongly consistent keys involves some of the same best
practices that we advise when writing to eventually consistent keys. We
recommend bearing the following in mind:

1. If you _know_ that a key does not yet exist, you can write to that
   key without supplying a [[context with the object|Using Strong
   Consistency#Object-Context]]. If you are unsure, then you should
   default to supplying a context object.
2. If an object already exists under a key, strong consistency demands
   that you supply an [[object context|Using Strong
   Consistency#Object-Context]]. If you do not supply one, the update
   will necessarily fail.
3. Because strongly consistent writes must occasionally
   [[sacrifice availability|Strong
   Consistency#Strong-vs.-Eventual-Consistency]] for the sake of
   consistency, **strongly consistent updates can fail even under normal
   conditions**, particularly in the event of concurrent updates.

## Error Messages

For the most part, performing reads, writes, and deletes on data in
strongly consistent buckets works much like it does in
non-strongly-consistent-buckets. One important exception to this is how
writes are performed. Strongly consistent buckets cannot allow siblings
by definition, and so all writes to existing keys must include a context
with the object.

If you attempt a write to a non-empty key without including causal
context, you will receive the following error:

```ruby
Riak::Conflict: The object is in conflict (has siblings) and cannot be treated singly or saved:
```

```java
java.lang.IllegalArgumentException: VClock cannot be null.
```

```python
riak.RiakError: 'failed'
```

```erlang
{error,<<"failed">>}
```

```curl
<html><head><title>412 Precondition Failed</title></head><body><h1>Precondition Failed</h1>Precondition Failed<p><hr><address>mochiweb+webmachine web server</address></body></html>
```

<div class="note">
<div class="title">Getting started with Riak clients</div>
If you are connecting to Riak using one of Basho's official
[[client libraries]], you can find more information about getting
started with your client in our [[quickstart guide|Five-Minute
Install#setting-up-your-riak-client]].
</div>

## Known Issue with Client Libraries

All of Basho's official [[client libraries]] currently convert errors
returned by Riak into generic exceptions, with a message derived from
the error message returned by Riak. In many cases this presents no
problems, since many error conditions are normal when using Riak.

When working with strong consistency, however, operations like
[[conditional puts|Strong Consistency#Implementation-Details]] commonly
produce errors that are difficult for clients to interpret. For example,
it is expected behavior for conditional puts to fail in the case of
concurrent updates to an object. At present, the official Riak clients
will convert this failure into an exception that is no different from
other error conditions, i.e. they will not indicate any
strong-consistency-specific errors.

The best solution to this problem at the moment is to catch these
exceptions on the application side and parse server-side error messages
to see if the error involved a conditional failure. If so, you should
set up your application to retry any updates, perhaps a specified number
of times or perhaps indefinitely, depending on the use case.

If you do set up a retry logic of this sort, however, it is necessary
to retry the entire read/modify/put cycle, meaning that you will need
to fetch the object, modify it, and then write. If you perform a simple
put over and over again, without reading the object, the update will
continue to fail.

A future version of Riak will address these issues by modifying the
server API to more accurately report errors specific to strongly
consistent operations.
