---
title: Tunable CAP Controls in Riak
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, fast-track]
prev: "[[Links and Link Walking]]"
up:   "[[The Riak Fast Track]]"
versions: false
interest: [
"[[Installing and Upgrading]]",
"[[Concepts]]",
"[[Querying Riak]]",
"[[System Planning]]",
"[[Basic Cluster Setup]]",
"[[Use Cases]]"
]
---

So, we've come a long way. If you've done the Fast Track in order, that means you've had a short intro to Riak, set up a four node cluster on your local machine, worked a bit with the HTTP interface, and performed some MapReduce queries.

In the last section of the Fast Track, we are going to talk about how Riak distributes your data around the cluster and lets you tune your levels of consistency and availability. This has immense value and implications for your applications, and it's one of the Riak features that we feel truly differentiates us.

At the bottom of this page there is a final screencast that briefly touches on how to adjust your replication levels to match your application and business needs. Before you watch that, however, have a quick read of the content below.

## A Primer on N, R, and W

Riak exposes "CAP Controls" to the developers in such a way that they can, down to the Bucket level, tune how many copies of data we want to store. We do this using N, R, and W values.

Riak's guiding design principle is Dr. Eric Brewer's CAP Theorem. The CAP theorem defines distributed systems in terms of three desired properties: Consistency, Availability, and Partition (failure) tolerance. The theorem states you can only rely on having two of the three properties at any time.

Riak chooses to focus on the A and P of CAP. The choice puts Riak in the eventually consistent camp. However, the window for "eventually consistent" is in terms of milliseconds which can be good enough for many applications.

### N Value and Replication

All data stored in Riak will be replicated to a number of nodes in the cluster according to the N value (n_val) property set on the bucket. By default, Riak chooses an n_val of "3" for you. This means that data stored in the bucket will be replicated to three different nodes, thus storing three copies. For this to be effective, you need at least three physical nodes in your cluster. (We can, however, demonstrate its merits with local nodes.)

To change the N value for a bucket (to something different than the default) issue a PUT request to the bucket with the new N value. If you still have your three node Riak cluster running, try this:

```
$ curl -v -XPUT http://127.0.0.1:8091/riak/another_bucket \
  -H "Content-Type: application/json" \
  -d '{"props":{"n_val":2}}'
```

This will change the n_val of the bucket "another_bucket" to two, meaning that each piece of data in that bucket will be replicated to two partitions in the cluster.

<div class="note"><div class="title">A Word on Setting the N Value</div><code>n_val</code> must be greater than 0 and less than or equal to the number of actual nodes in your cluster to get all the benefits of replication. And, we advise against modifying the n_val of a bucket after its initial creation as this may result in failed reads because the new value may not be replicated to all the appropriate partitions.</div>

### R Value and Read Failure Tolerance

So we changed the Bucket n_val to 2 with that last command.

Riak allows the client to supply an "R value" on each direct fetch. The R value represents the number of Riak nodes which must return results for a read before the read is considered successful. This allows Riak to provide read availability even when nodes are down or laggy.

For example, in this HTTP request, the r value is set to 1:

```bash
http://127.0.0.1:8091/riak/images/1.png?r=1
```

This means that Riak will return a copy of that data if at least 1 copy is present in your cluster.

### W Value and Write Fault Tolerance

Riak also allows the client to supply a "W value" on each update. The W value represents the number of Riak nodes which must report success before an update is considered complete. This allows Riak to provide write availability even when nodes are down or laggy.

In this PUT operation, you can see the w value set to 3.

```
$ curl -v -XPUT http://127.0.0.1:8091/riak/docs/story.txt?w=3 \
  -H "Content-type: text/plain" \
  --data-binary @story.txt
```

### Symbolic Consistency Names

Riak 0.12 introduces "symbolic" consistency options for R and W that can be easier to use and understand. They are:

* *all* - All replicas must reply. This is the same as setting R or W equal to N.
* *one* - This is the same as sending 1 as the R or W value.
* *quorum* - A majority of the replicas must respond, that is, "half plus one". For the default N value of 3, this calculates to 2.
* *default* - Uses whatever the per-bucket consistency property is for R or W, which may be any of the above values, or an integer.

Not submitting an R or W value is the same as sending "default".

## N, R and W in Action

Here is brief screencast that will show you just how the N, R, and W values function in our running three node Riak cluster:

<div style="display:none" class="iframe-video" id="http://player.vimeo.com/video/11172656"></div>

<p><a href="http://vimeo.com/11172656">Tuning CAP Controls in Riak</a> from <a href="http://vimeo.com/bashotech">Basho Technologies</a> on <a href="http://vimeo.com">Vimeo</a>.</p>

### What's next?

Congratulations. If you're reading this and have done the Fast Track in order, it means that you've built a three node Riak cluster, inserted a small amount of data with the help of some scripts, performed basic API operations, queried that data with MapReduce, and have an introduction to the powers of tunable CAP controls. Needless to say, you've come a long way.

But, there is always more Riak learning to be done, so here is a list of next steps you may want to take if you are still interested in Riak. Whatever you do, if you have a moment, send an email to _docs@basho.com_ with your thoughts on what you thought we did well, and, more importantly, how we can make the Riak Fast Track better.

* If you'd like more general information about Riak, check out our collections of [[Publications]] and [[Slide Decks]].
* For more info on how to interact with Riak from your language of choice, start with [[Client Libraries]]
* To download a platform-specific package of Riak, try [[Installation]].
* If you're interested in knowing how Riak stacks up to a few other databases, have a look at [[Riak Comparisons]]
* If you want to have a look at the Riak Source you can do so on [GitHub](http://github.com/basho/riak).

And finally, a big [[Thank You]] to everyone who helped create, update and fix this tutorial.
