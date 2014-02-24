---
title: Using Strong Consistency
project: riak
version: 2.0.0+
document: guide
toc: true
audience: advanced
keywords: [developers, strong-consistency]
---

Riak allows you to create [[Strongly Consistent|Strong Consistency]] bucket types. This means that as soon as a value is successfully written to Riak, all clients will be able to see that value. This was added in version 2.0 to complement Riak's standard [[Eventually Consistent|Eventual Consistency]] model.

The catch? When nodes are unavailable it may be impossible for the cluster to accept a write. An unfortunate [but unavoidable](http://en.wikipedia.org/wiki/CAP_theorem) tradeoff.

## Creating a Strongly Consistent Bucket Type

[[Strong consistency]] requirements in Riak are applied on a bucket-by-bucket basis, meaning that you can use some buckets in an eventually consistent fashion and others in a strongly consistent fashion, depending on your use case.

To apply strong consistency to a bucket, you must [[create a bucket type|Using Bucket Types]] that sets the `consistent` bucket property to `true`, activate the type, and then begin applying that type to bucket/key pairs.

Create a bucket type `strongly_consistent` (the name is up to you) with `consistent` set to `true`:

```bash
riak-admin bucket-type create strongly_consistent '{"props":{"consistent":true}}'

# Or if your type involves setting other properties to non-default values as well:
riak-admin bucket-type create strongly_consistent '{"props":{"consistent":true, ... other properties ... }}'
```

Then check the status of the type to ensure that it has propagated through all nodes and is thus ready to be used:

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

You can find more comprehensive information on [[Using Bucket Types]], and more information on [[Strong Consistency]].
