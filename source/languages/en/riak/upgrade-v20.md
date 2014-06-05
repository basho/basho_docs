---
title: Upgrading to 2.0
project: riak
version: 2.0.0+
document: guide
audience: intermediate
keywords: [2.0, developers]
---

If you are upgrading to Riak 2.0 from an earlier version, we strongly
recommend reading through each of the sections of this guide for
information on which concrete steps need to be undertaken to
successfully upgrade and which default Riak behaviors have changed.

If you are looking for an overview of the new features and functionality
included in version 2.0, we recommend checking out our [[intro to 2.0|Riak 2.0]]
guide.

## New Clients

If you are upgrading to Riak 2.0 or later, we recommend upgrading your
application to use a client that was built 

## Bucket Types

In versions of Riak prior to 2.0, the location of objects was determined
by objects' [[bucket|Buckets]] and [[key|Keys and Objects]] and all
bucket-level configurations were managed by setting
[[bucket properties|The Basics#bucket-properties-and-operations]]. In
Riak 2.0, [[bucket types|Using Bucket Types]] are both an additional
namespace for location objects and a new means of configuring bucket
properties.

More comprehensive details on usage can be found in the documentation on
[[using bucket types]]. Here, we'll list some of the things to be aware
of while upgrading.

#### Bucket types and object location

With the introduction of bucket types, the location of all Riak objects
is determined by bucket, key, _and_ bucket type, meaning that there
are three namespaces involved in object location instead of two. If your
application was written in conjunction with a version of Riak prior to
2.0, you should make sure that any endpoint in Riak targeted by

#### Features that rely on bucket types

#### Bucket types are not strictly necessary

Although we [[strongly recommend|Using Bucket Types#how-bucket-types-work]]
using bucket types, you do not have to use them after upgrading to 2.0.
If you do not, you can still manage bucket configurations using the
older, [[bucket properties|The Basics#bucket-properties-and-operations]]-based
system.

If you do decide to use bucket types, though, please bear in mind that
you cannot downgrade your cluster to a version of Riak prior to 2.0 if
you have (a) created and (b) activated a bucket type.

## New allow_mult Behavior

One of the biggest changes in version 2.0 involves default behavior
surrounding [[siblings|Vector Clocks#siblings]]. In versions of Riak
prior to 2.0, the `allow_mult` setting was set to `false` by default for
all buckets, which means that Riak's default behavior was to resolve
object replica [[conflicts|Conflict Resolution]] between nodes on its
own, and thus not to force connecting clients to resolve those conflicts.

In version 2.0, Riak's new default behavior is as follows:

* If you 

## When Downgrading is No Longer an Option

## Upgrading Your Configuration System

Riak 2.0 offers a replacement configuration system, based on the
[Cuttlefish](https://github.com/basho/cuttlefish) project, that both
simplifies configuration syntax and utilizes one file, `riak.conf`,
instead of two (`app.config` and `vm.args`). Full documentation of the
new system can be in the [[configuration files]] document.

If you're upgrading to Riak 2.0 from an earlier version, you have two
configuration options:

1. Manually port your configuration from the older system into the new system.
2. Keep your configuration files from the older system, which are still recognized in Riak 2.0.

If you choose the first option, make sure to consult the [[configuration files]]
documentation, as many configuration parameters have changed names,
some no longer exist, and others have been added.

If you choose the second option, Riak will automatically determine that
the older configuration system is being used. You should be aware,
however, that some settings must be set in an `advanced.config` file.
For a listing of those parameters, see our documentation on [[advanced configuration|Configuration Files#advanced-configuration]].

## Disk Usage Expectations

* Upgrade process could be resource intensive
* 

## Upgrading Search

#### Note on Riak client libraries

* Older clients using PBC can use the new Riak Search; clients using HTTP cannot

