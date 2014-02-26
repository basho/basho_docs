---
title: Using Strong Consistency
project: riak
version: 2.0.0+
document: guide
toc: true
audience: advanced
keywords: [developers, strong-consistency]
---

Since version 2.0, Riak allows you to create [[strongly consistent|Strong Consistency]] bucket types. When a bucket's data is strongly consistent, a value is guaranteed to be readable by any node _immediately_ after a write has occurred, with the tradeoff that unavailable nodes will become unavailable to accept a write.

The option of using strongly consistent bucket types was added in 2.0 to compliment Riak's standard [[eventually consistent|Eventual Consistency]] mode, which favors high availability over consistency and remains the default.

The tradeoff between consistency and availability is unavoidable, but the [choice is now yours](http://en.wikipedia.org/wiki/CAP_theorem) to make.

## Creating a Strongly Consistent Bucket Type

[[Strong consistency]] requirements in Riak are applied on a bucket-by-bucket basis, meaning that you can use some buckets in an eventually consistent fashion and others in a strongly consistent fashion, depending on your use case.

To apply strong consistency to a bucket, you must [[create a bucket type|Using Bucket Types]] that sets the `consistent` bucket property to `true`, activate the type, and then begin applying that type to bucket/key pairs.

First, create a bucket type `strongly_consistent` with `consistent` set to `true`:

```bash
riak-admin bucket-type create consistent_bucket '{"props":{"consistent":true}}'
```

Here, the name `strongly_consistent` is used only for the sake of example. You may name any bucket type anything you wish, with the exception of `default`.

If your type involves setting properties other than `consistent` to non-default values, those properties may also be included:

```
riak-admin bucket-type create consistent_bucket '{"props":{"consistent":true, ... other properties ... }}'
```

Once the type has been created, check its status to ensure that it has propagated through all nodes and is thus ready to be used:

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

Elsewhere in the Riak documentatio, you can find more comprehensive information on [[Using Bucket Types]], and more information on [[Strong Consistency Concept]].