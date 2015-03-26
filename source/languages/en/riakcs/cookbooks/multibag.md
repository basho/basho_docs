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

While [Riak CS Enterprise](http://basho.com/riak-enterprise) enables
you to distribute Riak CS objects across multiple data centers in a
[[source/sink pattern|Multi Data Center Replication v3 Architecture]],
all linked clusters are treated the same. In Riak CS version 1.5.0,
however, Basho has added **multibag** support to Riak CS Enterprise.

With multibag support, you can store object manifests and blocks in
separate clusters or groups of clusters, a.k.a. **bags**, enhancing the
scalability and overall storage capabilities of a Riak CS installation.

## Bags

A bag is a set of clusters linked together via [[Multi-Datacenter
Replication|Multi Data Center Replication v3 Architecture]] \(MDC).
Without MDC support, a bag consists of a single cluster. With MDC
support, however, a bag can consist of several linked clusters. You can
assign bags **weights** that determine the likelihood that objects,
blocks, and manifests will be stored there. For example, if you expect
to use one bag more heavily than another you can increase the weight of
that bag using the interface described in [[Riak CS Command-line
Tools]].

## The Master Bag

In a Riak CS multibag setup, there is one special bag, known as the
**master bag**, that bears a set of special responsibilities. It stores
objects such as:

* User information (for authentication and other purposes)
* Bucket-related information, e.g. the bag in which each bucket is
  stored
* Access statistics regarding Riak CS usage

## Multibag Configuration

In order to use Riak CS multibag, you need to modify multiple configuration
files. First, in each Riak CS node you need to alter the node's
`advanced.config` or `app.config` file to specify the host and port of each bag.
For example, if you wanted to set up bags on host 127.0.0.1 with three different
ports -- 10017,10027, and 10037 -- you would add the following section:

```advancedconfig
{riak_cs_multibag, [
	%% Other configs
    {bags,
     [
      {"bagA", "127.0.0.1", 10017},
      {"bagB", "127.0.0.1", 10027},
      {"bagC", "127.0.0.1", 10037}
     ]},
     %% Other configs
]},
```
```appconfig
{riak_cs_multibag, [
	%% Other configs
    {bags,
     [
      {"bagA", "127.0.0.1", 10017},
      {"bagB", "127.0.0.1", 10027},
      {"bagC", "127.0.0.1", 10037}
     ]},
     %% Other configs
]},
```

As with all configuration changes, each node must be restarted for the
changes to take effect.

In addition to configuring Riak CS to use multibag support, you will need to
mirror the configuration changes shown above in Stanchion. In the
`advanced.config` or `app.config` file in each Stanchion node, the following
section would need to be inserted:

```advancedconfig
{stanchion, [
	%% Other configs
	{bags,
	 [
	  {"bagA", "127.0.0.1", 10017},
	  {"bagB", "127.0.0.1", 10027},
	  {"bagC", "127.0.0.1", 10037}
	 ]
	}
	%% Other configs
]},
```
```appconfig
{stanchion, [
	%% Other configs
	{bags,
	 [
	  {"bagA", "127.0.0.1", 10017},
	  {"bagB", "127.0.0.1", 10027},
	  {"bagC", "127.0.0.1", 10037}
	 ]
	}
	%% Other configs
]},
```

## Transitioning to Multibag Support

If you have an existing Riak CS installation without multibag support
and would like to add it, there is a series of basic steps to follow.

### Stanchion

Stanchion houses some of the basic functionality required for Riak CS
multibag support. The first step in transitioning to multibag support
is to upgrade Stanchion to a version that supports Riak CS multibag.
That involves performing the following steps on each node:

1. Stop the node
2. Upgrade Stanchion to a version that supports Riak CS multibag, i.e.
   Riak CS 1.5.0 and later
3. Set your desired Stanchion [[configuration|Configuring Stanchion]]
4. Start Stanchion on each node

### Add Clusters

To add clusters to a multibag installation, you must set up Riak CS and
Stanchion to communicate with those clusters. You can specify the
connection information as explained above in the [[Multibag
Configuration|Riak CS Multibag Support#Multibag-Configuration]] section.

### Set Weights

When a new bag is added, you must first set the weight of that bag to
zero using the [[`riak-cs-multibag`|Riak CS Command-Line Tools]]
command-line interface. The example below sets the weight of the
recently added bag `bagA` to zero:

```bash
riak-cs-multibag weight bagA 0
```

All weights are stored in the [[master bag|Riak CS Multibag
Support#The-Master-Bag]] and shared with all Riak CS nodes, which means
that you only have to set weights once for them to be valid throughout
your cluster.

All bags must begin their life with a weight of zero. However, you can
set non-zero weights once all Riak CS and Stanchion nodes are properly
set up to recognize one another in the cluster. Let's say that we've set
up three bags, `bagA`, `bagB`, and `bagC`. We want to assign them the
weights 40, 40, and 20, respectively. The following commands would
accomplish that:

```bash
riak-cs-multibag weight bagA 40
riak-cs-multibag weight bagB 40
riak-cs-multibag weight bagC 20
```

The weights don't need to add up to 100 or to any specific number. Each
weight will be calculated as a percentage of the total assigned weights.
Thus, if a fourth bag were added, you could assign it a weight of 30
without changing the other weights.

Congratulations! Your Riak CS installation is now ready to use the new
multibag feature.

## Command Line Interface

Complete documentation for the `riak-cs-multibag` interface can be found
in our documentation on [[Riak CS Command Line Tools|Riak CS Command
Line Tools#riak-cs-multibag]].

## Limitations

Riak CS multibag does not currently support [[proxy
gets|Multi Data Center Replication v3 Operations#Riak-CS-MDC-Gets]] from
sink clusters.
