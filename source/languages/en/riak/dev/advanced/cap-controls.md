---
title: Replication Properties
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [developers, cap, replication]
interest: [
"[[Installing and Upgrading]]",
"[[Concepts]]",
"[[Planning for a Riak System]]",
"[[Cluster Capacity Planning]]",
"[[Use Cases]]"
]
moved: {
  '1.4.0-': '/tutorials/fast-track/Tunable-CAP-Controls-in-Riak'
}
---

Here, we are going to talk about how Riak distributes your data around the cluster and lets you tune your levels of consistency and availability. This has immense value and implications for your applications and is one of the features that we feel truly differentiates Riak from other technologies.

At the bottom of this page, there is a final screencast that briefly touches on how to adjust your replication levels to match your application and business needs. Before you watch, however, have a quick read of the content below.

## A Primer on N, R, and W

Riak exposes replication controls to the developers in such a way that they can tune, down to the bucket level, how many copies of data they want to store, how many copes they wish to read from at a time, and how many copies must write to be considered a success. We do this using N, R, and W values.

Riak's guiding design principle is Dr. Eric Brewer's [CAP Theorem](http://en.wikipedia.org/wiki/CAP_theorem). The CAP theorem defines distributed systems in terms of three desired properties: consistency, availability, and partition (i.e. failure) tolerance. The theorem states that you can only rely on having two of the three properties at any time.

Riak chooses to focus on the A and P of CAP. This choice puts Riak in the eventually consistent camp. It should be stated, however, that the window for "eventually consistent" is usually in the neighborhood of milliseconds, which can be good enough for many applications.

### N Value and Replication

All data stored in Riak will be replicated to a number of nodes in the cluster according to the N value (`n_val`) property set on the bucket. By default, Riak chooses an `n_val` of 3 for you. This means that data stored in the bucket will be replicated to three different nodes, thus storing three copies. For this to be effective, you need at least three physical nodes in your cluster. The merits of this system can be demonstrated, however, using your local environment.

To change the `n_val` for a bucket, you must create and activate a [[bucket type|Using Bucket Types]] that uses the desired `n_val`, let's say 2:

```bash
riak-admin bucket-type create n_val_is_2 '{"props":{"n_val":2}}'
riak-admin bucket-type activate n_val_is_2
```

Now, all buckets that bear the type `n_val_is_5` will have `n_val` set to 2. Here's an example write:

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "the n_val on this write is 2" \
  http://localhost:8098/types/n_val_is_5/buckets/test_bucket/keys/test_key
```

<div class="note">
<div class="title">A Word on Setting the N Value</div>
<tt>n_val</tt> must be greater than 0 and less than or equal to the number of actual nodes in your cluster to get all the benefits of replication. We advise against modifying the <tt>n_val</tt> of a bucket after its initial creation as this may result in failed reads because the new value may not be replicated to all the appropriate partitions.
</div>

### R Value and Read Failure Tolerance

Above, we create a bucket type with `n_val` set to `2`.

Riak also allows clients to supply an *R value* on each direct fetch. The R value represents the number of Riak nodes that must return results for a read before the read is considered successful. This allows Riak to provide read availability even when nodes are down or laggy.

Let's create and activate a bucket type with `r` set to 1

```bash
riak-admin bucket-type create r_equals_1 '{"props":{"r":1}}'
riak-admin bucket-type activate r_equals_1
```

Now, for data that is in a bucket that bears our `r_equals_1` type, at least one copy is present in your cluster. Here's an example read request with `r` set to `1`:

```curl
curl http://localhost:8098/types/r_equals_1/buckets/test_bucket/keys/test_key
```

### W Value and Write Fault Tolerance

Riak also allows the client to supply a *W value* on each update. The W value represents the number of Riak nodes that must report success before an update is considered complete. This allows Riak to provide write availability even when nodes are down or laggy.

Let's create and activate a bucket type with `w` set to `3`:

```bash
riak-admin bucket-type create w_equals_3 '{"props":{"w":3}}'
riak-admin activate w_equals_3
```

Now, we can attempt a write in accordance with that W value:

```curl
curl -XPUT \
  -H "Content-type: text/plain" \
  --data-binary @story.txt \
  http://localhost:8098/types/w_equals_3/buckets/docs/keys/story.txt
```

### Symbolic Consistency Names

Riak 0.12 introduced "symbolic" consistency options for R and W that can be easier to use and understand. They are:

* `all` --- All replicas must reply. This is the same as setting R or W equal to N.
* `one` --- This is the same as sending 1 as the R or W value.
* `quorum` --- A majority of the replicas must respond, that is, half plus one. For the default N value of 3, this calculates to 2, an N value of 5 calculates to 3, and so on.
* `default` --- Uses whatever the per-bucket consistency property is for R or W, which may be any of the above values, or an integer.

Not submitting an R or W value is the same as sending `default`.

## N, R and W in Action

Here is a brief screencast that shows just how the N, R, and W values function in our running three-node Riak cluster:

<div style="display:none" class="iframe-video" id="http://player.vimeo.com/video/11172656"></div>

<p><a href="http://vimeo.com/11172656">Tuning CAP Controls in Riak</a> from <a href="http://vimeo.com/bashotech">Basho Technologies</a> on <a href="http://vimeo.com">Vimeo</a>.</p>
