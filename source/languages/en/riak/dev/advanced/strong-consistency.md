---
title: Using Strong Consistency
project: riak
version: 2.0.0+
document: guide
toc: true
audience: advanced
keywords: [developers, strong-consistency]
---

Riak allows you to create [[Strongly Consistent|Strong Consistency]] bucket types since version 2.0. This means that a value is guarenteed readable by any node immediately after a write has occurred, with the tradeoff that unavailable nodes will become unavailable to accept a write. This was added in version 2.0 to compliment Riak's standard [[Eventually Consistent|Eventual Consistency]], but high availability, mode.

This tradeoff is necessary, but the [choice is now yours](http://en.wikipedia.org/wiki/CAP_theorem) to make.

## Creating a Strongly Consistent Bucket Type

[[Strong consistency]] requirements in Riak are applied on a bucket-by-bucket basis, meaning that you can use some buckets in an eventually consistent fashion and others in a strongly consistent fashion, depending on your use case.

To apply strong consistency to a bucket, you must [[create a bucket type|Using Bucket Types]] that sets the `consistent` bucket property to `true`, activate the type, and then begin applying that type to bucket/key pairs.

Create a bucket type `strongly_consistent` (or something else) with `consistent` set to `true`:

```bash
riak-admin bucket-type create consistent_bucket '{"props":{"consistent":true}}'

# Or if your type involves setting other properties to non-default values as well:
riak-admin bucket-type create consistent_bucket '{"props":{"consistent":true, ... other properties ... }}'
```

Then check the status of the type to ensure that it has propagated through all nodes and is thus ready to be used:

```bash
riak-admin bucket-type status consistent_bucket
```

If the console outputs `consistent_bucket has been created and may be activated` and the properties listing shows that `consistent` has been set to `true`, then you may proceed with activation:

```bash
riak-admin bucket-type activate consistent_bucket
```

When activation is successful, the console will return the following:

```bash
consistent_bucket has been activated
```

Now, any bucket that bears the type `consistent_bucket`---or whatever you wish to name your bucket type---will provide strong consistency guarantees.

You can find more comprehensive information on [[Using Bucket Types]], and more information on [[Strong Consistency Concept]].