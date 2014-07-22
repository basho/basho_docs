---
title: Upgrading to 2.0
project: riak
version: 2.0.0+
document: guide
audience: intermediate
keywords: [developers, upgrading]
---

If you are upgrading to Riak 2.0 from an earlier version, we strongly
recommend reading through each of the sections of this guide for
information on which concrete steps need to be undertaken to
successfully upgrade and which default Riak behaviors have changed.

If you are looking for an overview of the new features and functionality
included in version 2.0, we recommend checking out our guide to
[[Riak 2.0]].

## New Clients

To take advantage of the new features available in Riak 2.0, we
recommend upgrading your application to an official Basho client that
was built with those features in mind. There exist official
2.0-compatible clients in the following languages:

* [Java](https://github.com/basho/riak-java-client)
* [Ruby](https://github.com/basho/riak-ruby-client)
* [Python](https://github.com/basho/riak-python-client)
* [Erlang](https://github.com/basho/riak-erlang-client)

While we strongly recommend using the newest versions of these clients,
older versions will still work with Riak 2.0, with the drawback that
those older clients will not able to take advantage of
[[new features|Riak 2.0]] like [[data types|Using Data Types]]
or the new [[Riak Search|Using Search]].

## Bucket Types

In versions of Riak prior to 2.0, the location of objects was determined
by objects' [[bucket|Buckets]] and [[key|Keys and Objects]], while all
bucket-level configurations were managed by setting
[[bucket properties|The Basics#bucket-properties-and-operations]].

In Riak 2.0, [[bucket types|Using Bucket Types]] are both an additional
namespace for locating objects _and_ a new means of configuring bucket
properties in a more systematic fashion. More comprehensive details on
usage can be found in the documentation on [[using bucket types]]. Here,
we'll list some of the things to be aware of when upgrading.

#### Bucket types and object location

With the introduction of bucket types, the location of all Riak objects
is determined by bucket type, bucket, and key, meaning that there
are three namespaces involved in object location instead of two. A full
tutorial can be found in [[Using Bucket Types]].

If your application was written in conjunction with a version of Riak 
prior to 2.0, you should make sure that any endpoint in Riak targeted in 
terms of a bucket/key pairing be changed to accommodate a bucket
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

* Accept Riak's [[default bucket configurations|Using Bucket Types#buckets-as-namespaces]]
* Change Riak's defaults using your [[configuration files|Configuration Files#default-bucket-properties]]
* Manage multiple sets of bucket properties by specifying those properties for all operations (not recommended)

#### Features that rely on bucket types

One of the reasons that we recommend using bucket types for Riak 2.0 and
later is because a variety of newer Riak features were built with bucket
types as a precondition:

* [[Strong consistency]] --- Using Riak's strong consistency subsystem requires you to set the `consistent` parameter on a bucket type to `true`
* [[Riak Data Types|Using Data Types]] --- In order to use Riak Data Types, you must [[create bucket types|Using Data Types#setting-up-buckets-to-use-riak-data-types]] specific to the Data Type you are using

#### Bucket types and downgrades

If you do decide to use bucket types, please bear in mind that you
cannot [[downgrade|Rolling Downgrades]] your cluster to a version of
Riak prior to 2.0 if you have both created and activated a
bucket type.

## New allow_mult Behavior

One of the biggest changes in version 2.0 from the standpoint of
application development involves Riak's default behavior regarding
[[siblings|Vector Clocks#siblings]]. In versions prior to 2.0, the
`allow_mult` setting was set to `false` by default for all buckets,
which means that Riak's default behavior was to resolve
object replica [[conflicts|Conflict Resolution]] between nodes on its
own, thus relieving connecting clients of the need to resolve those
conflicts.

**In 2.0, `allow_mult` is set to `true` for any bucket type that you
create and activate.** This means that the default when [[using bucket
types]] is to handle [[conflict resolution]] on the client side using
either traditional [[vector clocks]] or the newer [[dotted version
vectors]].

If you wish to set `allow_mult` to `false` in version 2.0, you have two
options:

* Set your bucket type's `allow_mult` property to `false`
* Don't use bucket types

More information on handling siblings can be found in our documentation
on [[conflict resolution]].

## Enabling Security

The
[[authentication and authorization|Authentication and Authorization]]
mechanisms with Riak 2.0 should only be turned on after careful
testing in a non-production environment. Security changes the way in
which all applications interact with Riak.

## When Downgrading is No Longer an Option

If you decide to upgrade to version 2.0, you can still downgrade your
cluster to an earlier version of Riak if you wish, _unless_ you perform
one of the following actions in your cluster:

* Index data to be used in conjunction with the new [[Riak Search|Using Search]].
* Create _and_ activate one or more [[bucket types|Using Bucket Types]]. By extension, you will not be able to downgrade your cluster if you have used the following features, both of which rely on bucket types:
	- [[Strong consistency]]
	- [[Riak Data Types|Using Data Types]]

If you use other new features, such as [[Riak Security|Authentication and Authorization]]
or the new [[configuration files]], you can still downgrade your
cluster, but you will no longer be able to use those features after the
downgrade.

## Upgrading Your Configuration System

Riak 2.0 offers a new configuration system that both simplifies 
configuration syntax and utilizes one configuration file, `riak.conf`,
instead of the two files, `app.config` and `vm.args`, required by the
older system. Full documentation of the new system can be found in 
[[Configuration Files]].

If you're upgrading to Riak 2.0 from an earlier version, you have two
configuration options:

1. Manually port your configuration from the older system into the new system.
2. Keep your configuration files from the older system, which are still recognized in Riak 2.0.

If you choose the first option, make sure to consult the [[configuration files]]
documentation, as many configuration parameters have changed names,
some no longer exist, and others have been added that were not
previously available.

If you choose the second option, Riak will automatically determine that
the older configuration system is being used. You should be aware,
however, that some settings must be set in an `advanced.config` file.
For a listing of those parameters, see our documentation on [[advanced configuration|Configuration Files#advanced-configuration]].

## Upgrading Search

Information on upgrading Riak Search to 2.0 can be found in our
[[Search upgrade guide|Upgrading Search from 1.x to 2.x]].
