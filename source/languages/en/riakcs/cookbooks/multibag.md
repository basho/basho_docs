---
title: Riak CS Multibag Support
project: riakcs
version: 1.5.0+
document: cookbook
header: riakee
audience: advanced
keywords: [cs, multibag, operator]
---

<div class="note">
<div class="title">Technical Preview</div>
Riak CS Multibag is currently in technical preview mode and is
available only to <a href="http://basho.com/riak-enterprise/">Riak
Enterprise</a> customers. It is not yet suitable for production use.
</div>

While Riak CS [Enterprise](http://basho.com/riak-enterprise) enables
you to distribute Riak CS object across multiple data centers in a
[[source/sink pattern|Multi Data Center Replication v3 Architecture]],
all linked clusters are treated the same. In Riak CS version 1.5.0,
however, Basho has added **multibag** support to Riak CS Enterprise.

With multibag support, you can store object manifests and blocks in
separate clusters, enhancing scalability and the overall storage
capabilities of a Riak CS installation.

## Bags

A bag is a set of clusters linked together via [[Multi-Datacenter
Replication|Multi Data Center Replication v3 Architecture]] \(MDC).
Without MDC support, a bag consists of a single cluster. With MDC
support, however, a bag can consist of several linked clusters. When
objects are stored in Riak CS with multibag support, you can target
specific bags by tagging objects with a bag ID.

## The Master Bag

In a Riak CS multibag setup, there is one special bag, known as the
master bag, that bears a set of special responsibilities, storing the
following objects:

* User information (for authentication and other purposes)
* Bucket-related information, e.g. the bag in which each bucket is
  stored
* Access statistics regarding Riak CS usage



## Multibag Configuration

In order to use Riak CS multibag, you need to modify multiple
configuration files. First, in each Riak CS node you need to alter the
node's `app.config` file to specify the host and port of each bag. For
example, if you wanted to set up bags on host 127.0.0.1 with three
different ports---10017,10027, and 10037-- you would add the following
section:

```appconfig
{riak_cs_multibag, [
    {bags,
     [
      {"bag-A", "127.0.0.1", 10017},
      {"bag-B", "127.0.0.1", 10027},
      {"bag-C", "127.0.0.1", 10037}
     ]},
     %% Other configs
]},
```

As with all configuration changes, each node must be restarted for the
changes to take effect. More information can be found in our
documentation 
