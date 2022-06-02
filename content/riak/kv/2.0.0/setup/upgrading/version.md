---
title: "Upgrading to Riak KV 2.0"
description: ""
project: "riak_kv"
project_version: "2.0.0"
menu:
  riak_kv-2.0.0:
    name: "Upgrading to 2.0"
    identifier: "upgrading_version"
    weight: 101
    parent: "upgrading"
toc: true
aliases:
  - /riak/2.0.0/upgrade-v20/
---

When upgrading to Riak 2.0 from an earlier version, we strongly
recommend reading each section of the following guide. This guide
explains which default Riak behaviors have changed and specific steps 
to take for a successful upgrade.

For an overview of the new features and functionality
included in version 2.0, check out our guide to [Riak 2.0]({{<baseurl>}}riak/kv/2.0.0/introduction).

## New Clients

To take advantage of the new features available in Riak 2.0, we
recommend upgrading your application to an official Basho client that
was built with those features in mind. There are official
2.0-compatible clients in the following languages:

* [Java](https://github.com/basho/riak-java-client)
* [Ruby](https://github.com/basho/riak-ruby-client)
* [Python](https://github.com/basho/riak-python-client)
* [Erlang](https://github.com/basho/riak-erlang-client)

While we strongly recommend using the newest versions of these clients,
older versions will still work with Riak 2.0, with the drawback that
those older clients will not able to take advantage of [new features]({{<baseurl>}}riak/kv/2.0.0/introduction) like [data types]({{<baseurl>}}riak/kv/2.0.0/developing/data-types) or the new [Riak Search]({{<baseurl>}}riak/kv/2.0.0/using/reference/search).

## Bucket Types

In versions of Riak prior to 2.0, the location of objects was
determined by objects' [bucket]({{<baseurl>}}riak/kv/2.0.0/learn/concepts/buckets) and [key]({{<baseurl>}}riak/kv/2.0.0/learn/concepts/keys-and-objects), while all bucket-level configurations were managed by setting [bucket properties]({{<baseurl>}}riak/kv/2.0.0/developing/usage/bucket-types/).

In Riak 2.0, [bucket types]({{<baseurl>}}riak/kv/2.0.0/using/cluster-operations/bucket-types) are both an additional namespace for locating objects _and_ a new way of configuring bucket properties in a systematic fashion. More comprehensive details on usage can be found in the documentation on [using bucket types]({{<baseurl>}}riak/kv/2.0.0/using/reference/bucket-types).
Here, we'll list some of the things to be aware of when upgrading.

#### Bucket types and object location

With the introduction of bucket types, the location of all Riak objects
is determined by:

* bucket type
* bucket
* key

This means there are 3 namespaces involved in object location instead of 2.
A full tutorial can be found in [Using Bucket Types]({{<baseurl>}}riak/kv/2.0.0/using/reference/bucket-types).

If your application was written using a version of Riak
prior to 2.0, you should make sure that any endpoint in Riak targeting
a bucket/key pairing is changed to accommodate a bucket
type/bucket/key location.

If you're using a pre-2.0-specific client and targeting a location
specified only by bucket and key, Riak will use the default bucket
configurations. The following URLs are equivalent in Riak 2.0:

```
/buckets/<bucket>/keys/<key>
/types/default/buckets/<bucket>/keys/<key>
```

If you use object locations that don't specify a bucket type, you have
three options:

* Accept Riak's [default bucket configurations]({{<baseurl>}}riak/kv/2.0.0/using/reference/bucket-types/#buckets-as-namespaces)
* Change Riak's defaults using your [configuration files]({{<baseurl>}}riak/kv/2.0.0/configuring/reference/#default-bucket-properties)
* Manage multiple sets of bucket properties by specifying those
  properties for all operations (not recommended)

#### Features that rely on bucket types

One reason we recommend using bucket types for Riak 2.0
and later is because many newer Riak features were built with
bucket types as a precondition:

* [Strong consistency]({{<baseurl>}}riak/kv/2.0.0/using/reference/strong-consistency) --- Using Riak's strong consistency subsystem
  requires you to set the `consistent` parameter on a bucket type to
  `true`
* [Riak Data Types]({{<baseurl>}}riak/kv/2.0.0/developing/data-types) --- In order to use Riak Data
  Types, you must [create bucket types]({{<baseurl>}}riak/kv/2.0.0/developing/data-types/#setting-up-buckets-to-use-riak-data-types) specific to the
  Data Type you are using

#### Bucket types and downgrades

If you decide to use bucket types, please remember that you
cannot [downgrade]({{<baseurl>}}riak/kv/2.0.0/setup/downgrade) your cluster to a version of
Riak prior to 2.0 if you have both created and activated a
bucket type.

## New allow_mult Behavior

One of the biggest changes in version 2.0 regarding
application development involves Riak's default
[siblings]({{<baseurl>}}riak/kv/2.0.0/learn/concepts/causal-context/#siblings) behavior.

In versions prior to 2.0, the
`allow_mult` setting was set to `false` by default for all buckets.
So Riak's default behavior was to resolve
object replica [conflicts]({{<baseurl>}}riak/kv/2.0.0/developing/usage/conflict-resolution) between nodes on its
own; relieving connecting clients of the need to resolve those
conflicts.

**In 2.0, `allow_mult` is set to `true` for any bucket type that you
create and activate.** 

This means that the default when [using bucket types]({{<baseurl>}}riak/kv/2.0.0/using/reference/bucket-types/) is to handle [conflict resolution]({{<baseurl>}}riak/kv/2.0.0/developing/usage/conflict-resolution) on the client side using
either traditional [vector clocks]({{<baseurl>}}riak/kv/2.0.0/learn/concepts/causal-context/#vector-clocks) or the newer [dotted version vectors]({{<baseurl>}}riak/kv/2.0.0/learn/concepts/causal-context/#dotted-version-vector).

If you wish to set `allow_mult` to `false` in version 2.0, you have two
options:

* Set your bucket type's `allow_mult` property to `false`.
* Don't use bucket types.

More information on handling siblings can be found in our documentation
on [conflict resolution]({{<baseurl>}}riak/kv/2.0.0/developing/usage/conflict-resolution).

## Enabling Security

The [authentication and authorization]({{<baseurl>}}riak/kv/2.0.0/using/security/basics) mechanisms included with Riak 2.0 should only be turned
on after careful testing in a non-production environment. Security
changes the way all applications interact with Riak.

## When Downgrading is No Longer an Option

If you decide to upgrade to version 2.0, you can still downgrade your
cluster to an earlier version of Riak if you wish, _unless_ you perform
one of the following actions in your cluster:

* Index data to be used in conjunction with the new [Riak Search]({{<baseurl>}}riak/kv/2.0.0/using/reference/search).
* Create _and_ activate one or more [bucket types]({{<baseurl>}}riak/kv/2.0.0/using/reference/bucket-types/). By extension, you will not be able to downgrade your cluster if you have used the following features, both of which rely on bucket types:
    - [Strong consistency]({{<baseurl>}}riak/kv/2.0.0/using/reference/strong-consistency)
    - [Riak Data Types]({{<baseurl>}}riak/kv/2.0.0/developing/data-types)

If you use other new features, such as [Riak Security]({{<baseurl>}}riak/kv/2.0.0/using/security/basics) or the new [configuration files]({{<baseurl>}}riak/kv/2.0.0/configuring/reference/), you can still
downgrade your cluster, but you will no longer be able to use those
features after the downgrade.

## Upgrading Your Configuration System

Riak 2.0 offers a new configuration system that both simplifies
configuration syntax and uses one configuration file, `riak.conf`,
instead of the two files, `app.config` and `vm.args`, required by the
older system. Full documentation of the new system can be found in
[Configuration Files]({{<baseurl>}}riak/kv/2.0.0/configuring/reference/).

If you're upgrading to Riak 2.0 from an earlier version, you have two
configuration options:

1. Manually port your configuration from the older system into the new
   system.
2. Keep your configuration files from the older system, which are still
   recognized in Riak 2.0.

If you choose the first option, make sure to consult the
[configuration files]({{<baseurl>}}riak/kv/2.0.0/configuring/reference/) documentation, as many configuration parameters have changed names, some no longer exist, and others have been added that were not previously available.

If you choose the second option, Riak will automatically determine that
the older configuration system is being used. You should be aware,
however, that some settings must be set in an `advanced.config` file.
For a listing of those parameters, see our documentation on [advanced configuration]({{<baseurl>}}riak/kv/2.0.0/configuring/reference/#advanced-configuration).

If you choose to keep the existing `app.config` files, you _must_ add the
following additional settings in the `riak_core` section:

```appconfig
{riak_core,
     [{default_bucket_props,
          [{allow_mult,false}, %% or the same as an existing setting
           {dvv_enabled,false}]},
          %% other settings
     ]
},
```
This is to ensure backwards compatibility with 1.4 for these bucket properties.

## Upgrading LevelDB

If you are using LevelDB and upgrading to 2.0, no special steps need to
be taken, _unless_ you wish to use your old `app.config` file for
configuration. If so, make sure that you set the
`total_leveldb_mem_percent` parameter in the `eleveldb` section of the
file to 70.

```appconfig
{eleveldb, [
    %% ...
    {total_leveldb_mem_percent, 70},
    %% ...
]}
```

If you do not assign a value to `total_leveldb_mem_percent`, Riak will
default to a value of `15`, which can cause problems in some clusters.

## Upgrading Search

Information on upgrading Riak Search to 2.0 can be found in our
[Search upgrade guide]({{<baseurl>}}riak/kv/2.0.0/setup/upgrading/search).

## Migrating from Short Names

Although undocumented, versions of Riak prior to 2.0 did not prevent the
use of the Erlang VM's `-sname` configuration parameter. As of 2.0 this
is no longer permitted. Permitted in 2.0 are `nodename` in `riak.conf`
and `-name` in `vm.args`. If you are upgrading from a previous version
of Riak to 2.0 and are using `-sname` in your `vm.args`, the below steps
are required to migrate away from `-sname`.

1. Upgrade to Riak 1.4.12.
2. Back up the ring directory on each node, typically located in
`/var/lib/riak/ring`.
3. Stop all nodes in your cluster.
4. Run [`riak-admin reip <old_nodename> <new_nodename>`]({{<baseurl>}}riak/kv/2.0.0/using/admin/riak-admin/#reip) on each node in your cluster, for each node in your
cluster. For example, in a 5 node cluster this will be run 25 total
times, 5 times on each node. The `<old_nodename>` is the current
shortname, and the `<new_nodename>` is the new fully qualified hostname.
5. Change `riak.conf` or `vm.args`, depending on which configuration
system you're using, to use the new fully qualified hostname on each
node.
6. Start each node in your cluster.
