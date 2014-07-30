---
title: Using Strong Consistency
project: riak
version: 2.0.0+
document: guide
toc: true
audience: advanced
keywords: [developers, strong-consistency]
---

In versions 2.0 and later, Riak allows you to create buckets that
provide [[strong consistency]] guarantees for the data stored within
them, enabling you to use Riak as a CP system (consistent plus partition
tolerant) for at least some of your data. This option was added to
complement Riak's standard [[eventually consistent|Eventual 
Consistency]], high availability mode.

When data is stored in a bucket with strong consistency guarantees, a
value is guaranteed readable by any node *immediately* after a
successful write has occurred. The tradeoff is that unavailable nodes
will become temporarily unable to accept writes to that key. This
tradeoff is necessary, but the [choice is now
yours](http://en.wikipedia.org/wiki/CAP_theorem) to make.

## Enabling Strong Consistency

In order to use strong consistency, your Riak cluster must have **at
least three nodes**. If it does not, you will need to [[add nodes|Basic
Cluster Setup]].

The strong consistency subsystem in Riak is disabled by default. You
can turn it on by changing the configuration your Riak installation's
`[[riak.conf|Configuration Files]]` file. Simply un-comment the line
containing the `strong_consistency = on` setting or add the following
if that line is not present:

```riakconf
strong_consistency = on
```

**Note**: This will enable you to use strong consistency in Riak, but
this setting will _not_ apply to all of the data in your Riak cluster.
Instead, strong consistency is applied only at the bucket level, [[using
bucket types]] \(as shown directly below).

## Creating Consistent Bucket Types

[[Strong consistency]] requirements in Riak are applied on a
bucket-by-bucket basis, meaning that you can use some buckets in an
eventually consistent fashion and others in a strongly consistent
fashion, depending on your use case.

To apply strong consistency to a bucket, you must create a [[bucket type
|Using Bucket Types]] that sets the `consistent` bucket property to
`true`, activate the type, and then begin applying that type to
bucket/key pairs.

To give an example, we'll create a bucket type called
`strongly_consistent` with `consistent` set to `true`:

```bash
riak-admin bucket-type create strongly_consistent \
    '{"props":{"consistent":true}}'
```

**Note**: You can name [[bucket types|Using Bucket Types]] whatever you
wish, with the exception of `default`, which is a reserved term (a full
listing of the properties associated with the `default` bucket type can
be found in the documentation on [[bucket properties and operations|The
Basics#Bucket-Properties-and-Operations]]).

Once the `strongly_consistent` bucket type has been created, we can
check the status of the type to ensure that it has propagated through
all nodes and is thus ready to be used:


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
you named your bucket type---will provide strong consistency guarantees.

Elsewhere in the Riak docs, you can find more information on [[using
bucket types]] and on the concept of [[strong consistency]].

## Replication Properties

Strongly consistent operations in Riak function much differently from
their [[eventually consistent|Eventual Consistency]] counterparts.
Whereas eventually consistent operations enable you to set values for a
variety of [[replication properties]] either on each request or at the
bucket level, [[using bucket types]]. Most of these settings, including
`r`, `pr`, `w`, and `rw` are quietly ignored for strongly consistent
operations. The exceptions are `n_val` and `return_body`.

## Object Context

Riak uses context objects called [[vector clocks]] to determine the
causal history of objects. In Riak versions 2.0 and later, there is an
option to use [[dotted version vectors]], which function much like
vector clocks from the standpoint of clients. Here, we'll refer to both
as an object's **context**.

While we strongly recommend attaching context to objects for all
updates---whether traditional vector clocks or the newer dotted version
vectors---they are purely [[optional|Conflict Resolution]] for all
eventually consistent operations in Riak. This is not the case for
strongly consistent operations. **When modifying strongly consistent
objects in Riak, you _must_ attach a context object**. If you attempt to
modify a strongly consistent object without attaching a context to the
request, the request will always fail.

## Error Messages

For the most part, performing reads, writes, and deletes on data in
strongly consistent buckets works much like it does in non-strongly
consistent buckets. One important exception to this is how writes are
performed. Strongly consistent buckets cannot allow siblings by
definition, and so all writes to existing keys must include a context
with the object (as explained in the section above).

If you attempt a write to a non-empty key without including a s
clock, you will receive the following error:

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

### Known Issue with Client Libraries

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
