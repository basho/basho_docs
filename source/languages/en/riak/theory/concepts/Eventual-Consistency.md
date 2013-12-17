---
title: Eventual Consistency
project: riak
version: 0.10.0+
document: appendix
toc: true
audience: intermediate
keywords: [appendix, concepts]
moved: {
  '1.4.0-': '/references/appendices/concepts/Eventual-Consistency'
}
---

An introduction to eventual consistency and what it means in terms of handling data with Riak.

In a distributed and fault-tolerant environment like Riak, server and
network failures are expected. Riak is designed to respond to requests
even when servers are offline or the cluster is experiencing a network
partition.

This has two notable consequences:

* Requests can (and should) be tuned based on data model and business needs.
* Data can be inconsistent across a cluster.

<div class="note">
Data inconsistencies can best be mitigated by
immutability. Conflicting data is impossible in the absence of
updates.
</div>

## Replication properties and request tuning

There are a variety of configuration options which will influence
Riak's behavior when responding to write and read
requests.

The parameters that we're concerned with here:

<table>
<thead><tr><th>Parameter</th><th>Often referred to as</th><th>Default value</th><th>Summary</th></tr></thead>
<tbody>
<tr><th><pre>n_val</pre></th><td><pre>N</pre></td><td>3</td><td>Replication factor</td></tr>
<tr><th><pre>r</pre></th><td><pre>R</pre></td><td>quorum</td><td>Number of servers that must respond to a read request</td></tr>
<tr><th><pre>w</pre></th><td><pre>W</pre></td><td>quorum</td><td>Number of servers that must respond to a write request</td></tr>
<tr><th><pre>pr</pre></th><td><pre>PR</pre></td><td>0</td><td>Number of <b>primary</b> servers that must respond to a read request</td></tr>
<tr><th><pre>pw</pre></th><td><pre>PW</pre></td><td>0</td><td>Number of <b>primary</b> servers that must respond to a write request</td></tr>
</tbody>
</table>

A value of `quorum` indicates a majority of the `N` value (`N/2+1`, or
2 for the default `N` value of 3).

There are additional configuration items that are closely related to
the above which are not covered in this document: `notfound_ok`,
`basic_quorum` and `dw`. See the
[Understanding Riak's Configurable Behaviors blog series](http://basho.com/understanding-riaks-configurable-behaviors-part-1/)
for more on all of these parameters.

See also [[Vector Clocks]] for a discussion of key configuration
options that impact conflict resolution.


## A simple example of eventual consistency

*Unless specified otherwise, assume that all configuration values are
 left at their default.*

Let's assume for the moment that a server in a Riak cluster has
recently recovered from failure and has an old copy of the key
`manchester-manager`, with value "Alex Ferguson." The current value of
that key on the other servers in the cluster is "David Moyes."

Shortly after the server comes back online and other cluster members
recognize that it is available, a read request for
`manchester-manager` arrives.

Such a request would have an `R` value of 2, meaning that while the
request will be sent to all `N` (3) servers responsible for the data,
2 must reply with a value before the client is informed of its value.

Regardless of which order the responses arrive to the server that is
coordinating this request, "David Moyes" will be returned as the value
to the client, because "Alex Ferguson" is recognized as an older value.

Behind the scenes, after "David Moyes" is sent to the client, a read
repair mechanism will occur on the cluster to fix the older value on
the server that just came back online.

### R=1

If we keep all of the above scenario the same but tweak the request
slightly with `R=1`, perhaps to allow for a faster response to the
client, it **is** possible that the client will be fed "Alex Ferguson"
as the response, if the recently-recovered server is the first to
reply.

However, the read repair mechanism will kick in and fix the value, so
next time someone asks, "David Moyes" will indeed be the answer.

### R=1, sloppy quorum

Let's take the scenario back in time to the point where our unlucky
server originally failed. At that point, all 3 servers had "Alex
Ferguson" as the value for `manchester-manager`.

When a server fails, Riak's *sloppy quorum* feature kicks in, and
another server takes responsibility for serving its requests.

The first time we issue a read request after the failure, if `R` is
set to 1, we run a significant risk of receiving a *not found*
response from Riak. The server that has assumed responsibility for
that data won't have a copy of `manchester-manager` yet, and it's much
faster to verify a missing key than to pull a copy of the value from
disk, so that server will likely respond fastest.

If `R` is left to its default value of 2, there wouldn't be a problem,
because 1 of the servers that still had a copy of "Alex Ferguson"
would also respond before the client got its result. In either case,
read repair will step in after the request has been completed and make
certain that the value is propagated to all the servers that need it.

### PR, PW, sloppy quorum

Thus far, we've discussed settings that permit sloppy quorums, in the
interest of allowing Riak to maintain as high a level of availability
as possible in the presence of server or network failure.

It is possible to configure requests to ignore sloppy quorums in order
to limit the possibility of older data being returned to a client. The
tradeoff, of course, is that if failover servers are not permitted to
serve requests, there is an increased risk of request failures.

For example, in our scenario we've been discussing the possibility of
a server for the `manchester-manager` key having failed, but to be
more precise, we've been talking about a *primary* server, one that
when the cluster is perfectly healthy would bear responsibility for
that key.

When that server failed, using `R=2` as we've discussed or even `R=3`
for a read request would still work properly: a failover server
(sloppy quorum again) would be tasks to take responsibility for that
key, and when it receives a request for it, it would reply that it
doesn't have any such key, but the two surviving primary servers still
know who the `manchester-manager` is.

However, if the `PR` (primary read) value is specified, only the two
surviving primary servers are considered valid sources for that data.

So, setting `PR` to 2 works fine, because there are still 2 such
servers, but a read request with `PR=3` would fail because the 3rd
primary server is offline, and no failover server can take its place
*as a primary*.

The same is true of writes: `W=2` or `W=3` will work fine with the
primary server offline, as will `PW=2` (primary write), but `PW=3`
will result in an error.

<div class="note"><div class="title">Errors and failures</div>It is
important to understand the difference between an error and a failure.
<br/><br/>
The <code>PW=3</code> request in this scenario will result in an error, <b>but the
value will still be written to the two surviving primary servers</b>.
<br/><br/>
By specifying <code>PW=3</code> the client indicated that 3 primary servers must
respond for the operation to be considered successful, which it
wasn't, but there's no way to tell without performing another read
whether the operation truly failed.</div>


## Further Reading

* [Understanding Riak's Configurable Behaviors blog series](http://basho.com/understanding-riaks-configurable-behaviors-part-1/)
* Werner Vogels, et. al.: [Eventually Consistent - Revisited](http://www.allthingsdistributed.com/2008/12/eventually_consistent.html)
