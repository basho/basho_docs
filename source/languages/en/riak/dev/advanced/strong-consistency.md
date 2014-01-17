---
title: Strong Consistency
project: riak
version: 2.0.0+
document: guide
toc: true
audience: advanced
keywords: [developers, strong-consistency]
---

Riak was originally designed to guarantee _eventual_ data consistency. While this remains the default behavior of Riak, versions of Riak >= 2.0 allow you to use Riak in a way that guarantees _strong_ data consistency.

By strong consistency we mean ensuring that any `GET` operation performed on a key will return either nothing or a single value (and thus no siblings), and that `GET` operations will be successful only when all nodes agree on the key's value. If you successfully `PUT` a key, the next successful `GET` is guaranteed to show that write because Riak will ensure that the object didn't change since you last accessed it. The request will fail if a concurrent write occurred and changed the object.

The trade-off with this approach is that it improves data consistency (the "C" in [CAP](http://en.wikipedia.org/wiki/CAP_theorem)) by detracting from data availability (the "A" in CAP), which in many cases will mean higher latency and slightly slower performance.

We recommend using this feature only on those buckets that require it.

## Creating a Strongly Consistent Bucket Type

Strong consistency requirements in Riak are applied on a bucket-by-bucket basis, meaning that you can use some buckets in an eventually consistent fashion and others in a strongly consistent fashion, depending on your use case.

To apply strong consistency to a bucket, you must [[create a bucket type|Using Bucket Types]] that sets the `consistent` bucket property to `true`, activate the type, and then begin applying that type to bucket/key pairs.

Create a bucket type `strongly_consistent` with `consistent` set to `true`:

```bash
riak-admin bucket-type create consistent_bucket '{"props":{"consistent":true}}'

# Or if your type involves setting other properties to non-default values as well:
riak-admin bucket-type create consistent_bucket '{"props":{"consistent":true, ... other properties ... }}'
```

Then check the status of the type to ensure that it has propagated through all nodes and is thus ready to be used:

```bash
riak-admin bucket-type status consistent_bucket
```

If the console outputs `consistent_bucket has been created and may be activated` and also shows that `consistent` has been set to `true`, then you may proceed with activation:

```bash
riak-admin bucket-type activate consistent_bucket
```

When activation is successful, the console will return the following:

```bash
consistent_bucket has been activated
```

Now, any bucket that bears the type `consistent_bucket`---or whatever you wish to name your bucket type---will provide strong consistency guarantees. You can find more comprehensive information on using bucket types [[here|Using Bucket Types]], and more information on what strong consistency involves [[here|Strong Consistency Concept]].