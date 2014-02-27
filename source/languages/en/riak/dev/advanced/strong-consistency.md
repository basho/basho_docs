---
title: Using Strong Consistency
project: riak
version: 2.0.0+
document: guide
toc: true
audience: advanced
keywords: [developers, strong-consistency]
---

In versions 2.0 and later, Riak allows you to create buckets that provide [[strong consistency]] guarantees for the data stored within them, enabling you to use Riak as a CP system (consistent plus partition tolerant) for at least some of your data. This option was added to complement Riak's standard [[eventually consistent|Eventual Consistency]], high availability mode.

When data is stored in a bucket with strong consistency guarantees, a value is guaranteed readable by any node *immediately* after a successful write has occurred. The tradeoff is that unavailable nodes will become temporarily unable to accept writes to that key. This tradeoff is necessary, but the [choice is now yours](http://en.wikipedia.org/wiki/CAP_theorem) to make.

## Enabling Strong Consistency

The strong consistency subsystem in Riak is disabled by default. You will need to turn it on by changing the configuration your Riak installation's `[[riak.conf|Configuration Files]]` file. To do so, you can simply un-comment the line containing the `strong_consistency = on` setting or add the following if that line is not present:

```riakconf
strong_consistency = on
```

**Note**: This will enable you to use strong consistency in Riak, but this setting will _not_ apply to all of the data in your Riak cluster. Instead, strong consistency is applied only at the bucket level, using bucket types (as show directly below).

## Creating a Strongly Consistent Bucket Type

[[Strong consistency]] requirements in Riak are applied on a bucket-by-bucket basis, meaning that you can use some buckets in an eventually consistent fashion and others in a strongly consistent fashion, depending on your use case.

To apply strong consistency to a bucket, you must create a [[bucket type|Using Bucket Types]] that sets the `consistent` bucket property to `true`, activate the type, and then begin applying that type to bucket/key pairs.

To give an example, we'll create a bucket type called `strongly_consistent` with `consistent` set to `true`:

```bash
riak-admin bucket-type create strongly_consistent '{"props":{"consistent":true}}'
```

**Note**: You can name [[bucket types|Using Bucket Types]] whatever you wish, with the exception of `default`, which is a reserved term (a full listing of the properties associated with the `default` bucket type can be found in the documentation on [[bucket properties and operations|The Basics#Bucket-Properties-and-Operations]]).

Once the `strongly_consistent` bucket type has been created, we can check the status of the type to ensure that it has propagated through all nodes and is thus ready to be used:


```bash
riak-admin bucket-type status strongly_consistent
```

If the console outputs `strongly_consistent has been created and may be activated` and the properties listing shows that `consistent` has been set to `true`, then you may proceed with activation:

```bash
riak-admin bucket-type activate strongly_consistent
```

When activation is successful, the console will return the following:

```bash
strongly_consistent has been activated
```

Now, any bucket that bears the type `strongly_consistent`---or whatever you named your bucket type---will provide strong consistency guarantees.

Elsewhere in the Riak docs, you can find more information on [[Using Bucket Types]] and on the concept of [[Strong Consistency]].
