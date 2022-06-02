---
title: "Riak CS Supercluster Support"
description: ""
menu:
  riak_cs-2.0.1:
    name: "Riak CS Supercluster Support"
    identifier: "advanced_supercluster_support"
    weight: 103
    parent: "run_advanced"
project: "riak_cs"
project_version: "2.0.1"
aliases:
  - /riakcs/2.0.1/cookbooks/supercluster/
---

{{% note title="Technical Preview" %}}
Riak CS Supercluster is currently in technical preview mode and is available
only to <a href="http://basho.com/riak-enterprise/">Riak Enterprise</a>
customers. It is not yet suitable for production use.
{{% /note %}}

While [Riak CS Enterprise](http://basho.com/riak-enterprise) enables
you to distribute Riak CS objects across multiple data centers in a
[source/sink pattern]({{<baseurl>}}riak/kv/2.1.3/using/reference/v3-multi-datacenter/architecture), all linked clusters are treated the same. In Riak CS version 1.5.0, however, Basho has added **supercluster** support to Riak CS Enterprise.

With supercluster support, you can store object manifests and blocks in
separate clusters or groups of clusters, a.k.a. **a set of supercluser members**, enhancing the scalability and overall storage capabilities of a Riak CS installation.

## Supercluster members

A supercluster member is a set of clusters linked together via [Multi-Datacenter Replication]({{<baseurl>}}riak/kv/2.1.3/using/reference/v3-multi-datacenter/architecture)\(MDC).
Without MDC support, a supercluster member consists of a single cluster. With MDC support, however, a supercluster member can consist of several linked clusters. You can assign members **weights** that determine the likelihood that objects, blocks, and manifests will be stored there. For example, if you expect to use one supercluster member more heavily than another you can increase the weight of that member using the interface described in [Riak CS Command-line Tools]({{<baseurl>}}riak/cs/2.0.1/cookbooks/command-line-tools).

## The Master Member

In a Riak CS supercluster setup, there is one special member, known as the
**master member**, that bears a set of special responsibilities. It stores
objects such as:

* User information (for authentication and other purposes)
* Bucket-related information, e.g. the supercluster member in which each bucket is
  stored
* Access statistics regarding Riak CS usage

## Supercluster Configuration

In order to use Riak CS supercluster, you need to modify multiple configuration
files. First, in each Riak CS node you need to alter the node's
`riak-cs.conf`, `advanced.config`, or `app.config` file to specify the host and port of each supercluster member.

For example, if you wanted to set up supercluster members on host `127.0.0.1` with three different ports -- `10017`,`10027`, and `10037` -- you would add the following section:

```riakcsconf
supercluster.member.Alpha = 127.0.0.1:10017
supercluster.member.Bravo = 127.0.0.1:10027
supercluster.member.Charlie = 127.0.0.1:10037
```
```advancedconfig
{riak_cs, [
  %% Other configs
    {supercluster_members,
     [
      {"Alpha", "127.0.0.1", 10017},
      {"Bravo", "127.0.0.1", 10027},
      {"Charlie", "127.0.0.1", 10037}
     ]},
     %% Other configs
]},
```
```appconfig
{riak_cs, [
  %% Other configs
    {supercluster_members,
     [
      {"Alpha", "127.0.0.1", 10017},
      {"Bravo", "127.0.0.1", 10027},
      {"Charlie", "127.0.0.1", 10037}
     ]},
     %% Other configs
]},
```

>As with all configuration changes, each node must be restarted for the
changes to take effect.

In addition to configuring Riak CS to use supercluster support, you will need to mirror the configuration changes shown above in Stanchion. In the
`stanchion.conf`, `advanced.config`, or `app.config` file in each Stanchion node, the following
section would need to be inserted:

```stanchionconf
supercluster.member.Alpha = 127.0.0.1:10017
supercluster.member.Bravo = 127.0.0.1:10027
supercluster.member.Charlie = 127.0.0.1:10037
```
```advancedconfig
{stanchion, [
  %% Other configs
  {supercluster_members,
   [
    {"Alpha", "127.0.0.1", 10017},
    {"Bravo", "127.0.0.1", 10027},
    {"Charlie", "127.0.0.1", 10037}
   ]
  }
  %% Other configs
]},
```
```appconfig
{stanchion, [
  %% Other configs
  {supercluster_members,
   [
    {"Alpha", "127.0.0.1", 10017},
    {"Bravo", "127.0.0.1", 10027},
    {"Charlie", "127.0.0.1", 10037}
   ]
  }
  %% Other configs
]},
```

## Transitioning to Supercluster Support

If you have an existing Riak CS installation without supercluster support
and would like to add it, there is a series of basic steps to follow.

### Stanchion

Stanchion houses some of the basic functionality required for Riak CS
supercluster support. The first step in transitioning to supercluster support
is to upgrade Stanchion to a version that supports Riak CS supercluster.
That involves performing the following steps on each node:

1. Stop the node
2. Upgrade Stanchion to a version that supports Riak CS supercluster, i.e.
   Riak CS 1.5.0 and later
3. Set your desired Stanchion [configuration]({{<baseurl>}}riak/cs/2.0.1/cookbooks/configuration/stanchion)
4. Start Stanchion on each node

### Add Clusters

To add clusters to a supercluster installation, you must set up Riak CS and
Stanchion to communicate with those clusters. You can specify the
connection information as explained above in the [supercluster Configuration](#supercluster-configuration) section.

### Set Weights

When a new supercluster member is added, you must first set the weight of that member to zero using the [`riak-cs-supercluster`]({{<baseurl>}}riak/cs/2.0.1/cookbooks/command-line-tools) command-line interface. 

The example below sets the weight of the recently added supercluster member `Alpha` to zero:

```bash
riak-cs-supercluster weight Alpha 0
```

All weights are stored in the [master member](#the-master-member) and shared with all Riak CS nodes, which means that you only have to set weights once for them to be valid throughout your cluster.

All supercluster members must begin their life with a weight of zero. However, you can set non-zero weights once all Riak CS and Stanchion nodes are properly
set up to recognize one another in the cluster. Let's say that we've set
up three members, `Alpha`, `Bravo`, and `Charlie`. We want to assign them the
weights 40, 40, and 20, respectively. The following commands would
accomplish that:

```bash
riak-cs-supercluster weight Alpha 40
riak-cs-supercluster weight Bravo 40
riak-cs-supercluster weight Charlie 20
```

The weights don't need to add up to 100 or to any specific number. Each
weight will be calculated as a percentage of the total assigned weights.
Thus, if a fourth supercluster member were added, you could assign it a weight of 30 without changing the other weights.

Congratulations! Your Riak CS installation is now ready to use the new
supercluster feature.

## Command Line Interface

Complete documentation for the `riak-cs-supercluster` interface can be found
in our documentation on [Riak CS Command Line Tools]({{<baseurl>}}riak/cs/2.0.1/cookbooks/command-line-tools/#riak-cs-supercluster).

## Limitations

Riak CS supercluster does not currently support [proxy gets]({{<baseurl>}}riak/kv/2.1.3/using/cluster-operations/v3-multi-datacenter/#riak-cs-mdc-gets) from
sink clusters.
