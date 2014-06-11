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

If you want to take advantage of the new features available in Riak 2.0,
we recommand upgrading your application to an official Basho client
that was built with those features in mind. There exist official
2.0-compatible clients in the following languages:

* [Ruby](https://github.com/basho/riak-ruby-client)
* [Java](https://github.com/basho/riak-java-client)
* [Python](https://github.com/basho/riak-python-client)
* [Erlang](https://github.com/basho/riak-erlang-client)

Clients not built with version 2.0 in mind will not be able to utilize
the following features:

* [[Bucket types|Using Bucket Types]]
* [[Strong consistency]]
* [[Riak Data Types|Using Data Types]]
* [[Riak Security|Authentication and Authorization]]

If you are upgrading to version 2.0 solely for the performance and other
benefits on offer and do not plan on using any of these features,
there's a chance that an older Riak client will still work. This will
need to be determined, however, on a case-by-case, application-specific
basis.

## Bucket Types

In versions of Riak prior to 2.0, the location of objects was determined
by objects' [[bucket|Buckets]] and [[key|Keys and Objects]], while all
bucket-level configurations were managed by setting
[[bucket properties|The Basics#bucket-properties-and-operations]]. In
Riak 2.0, [[bucket types|Using Bucket Types]] are both an additional
namespace for locating objects _and_ a new means of configuring bucket
properties in a more systematic fashion. More comprehensive details on
usage can be found in the documentation on [[using bucket types]]. Here,
we'll list some of the things to be aware of when upgrading.

#### Bucket types and object location

With the introduction of bucket types, the location of all Riak objects
is determined by bucket, key, _and_ bucket type, meaning that there
are three namespaces involved in object location instead of two. If your
application was written in conjunction with a version of Riak prior to
2.0, you should make sure that any endpoint in Riak targeted in terms of
a bucket/key pairing be changed to accommodate a bucket type/bucket/key
location.

Here are some examples of a read request for an object in the bucket
`test_bucket` with the key `test_key` using older versions of the
official Riak clients:

```ruby
bucket = client.bucket('test_bucket')
obj = bucket.get('test_key')
```

```java
// Using an already instantiated "client" object:

Bucket testBucket = client.fetchBucket("test_bucket").execute();
IRiakObject obj = testBucket.fetch("test_key").execute();
```

```python
bucket = client.bucket('test_bucket')
obj = bucket.get('test_key')
```

```erlang
%% Using an already instatiated Pid:

{ok, Obj} = riakc_pb_socket:get(Pid, <<"test_bucket">>, <<"test_key">>).
```

The following code samples use 2.0-specific official Riak clients to
fetch an object with the same bucket and key as before---`test_bucket`
and `test_key`, respectively---but with a bucket type of `custom_bucket_type`:

```ruby
bucket = client.bucket('test_bucket')
obj = bucket.get('test_key', bucket_type: 'custom_bucket_type')
```

```java
// Note that the Java client uses a new Location class to designated
// object location:

Location loc = new Location("test_bucket")
		.setBucketType("custom_bucket_type")
		.setKey("test_key");
FetchValue fetch = new FetchValue.Builder(loc).build();
FetchValue.Response response = client.execute(fetch);
RiakObject obj = response.getValue(RiakObject.class);
```

```python
bucket = client.bucket('test_bucket', bucket_type='custom_bucket_type')
obj = bucket.get('test_key')
```

```erlang
{ok, Obj} = riakc_pb_socket:get(Pid,
	                            {<<"custom_bucket_type">>, <<"test_bucket">>},
	                            <<"test_key">>).
```

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
* Manage multiple sets of bucket properties by specifying those properties for all transactions (not recommended)

#### Features that rely on bucket types

One of the reasons that we recommend using bucket types for Riak 2.0 and
later is because a variety of newer Riak features were built with
bucket types as an essential precondition:

* [[Strong consistency]] --- Using Riak's strong consistency subsystem requires you to set the `consistent` parameter on a bucket type to `true`
* [[Riak Data Types|Using Data Types]] --- In order to use Riak Data Types, you must [[create bucket types|Using Data Types#setting-up-buckets-to-use-riak-data-types]] specific to the Data Type you are using

#### Bucket types and downgrades

If you do decide to use bucket types, please bear in mind that you
cannot [[downgrade|Rolling Downgrades]] your cluster to a version of
Riak prior to 2.0 if you have both (a) created and (b) activated a
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
(a) create and (b) activate.**

If you wish to set `allow_mult` to `false` in version 2.0, you have a
few options:

* Use the default bucket configuration
* Create and activate bucket types that explicitly set `allow_mult` to `false`

More information on handling siblings can be found in our documentation
on [[conflict resolution]].

## When Downgrading is No Longer an Option

If you decide to upgrade to version 2.0, you can still downgrade your
cluster to an earlier version of Riak if you wish, _unless_ you perform
one of the following actions in your cluster:

* Index data to be used in conjunction with the new [[Riak Search|Using Search]] (codename Yokozuna).
* Create _and_ activate one or more [[bucket types|Using Bucket Types]]. By extension, you will not be able to downgrade your cluster if you have used the following features, both of which rely on bucket types:
	- [[Strong consistency]]
	- [[Riak Data Types|Using Data Types]]

If you use other new features, such as [[Riak Security|Authentication and Authorization]]
or the new [[configuration system|Configuration Files]], you can still
downgrade your cluster, but you will no longer be able to use those
features after the downgrade.

## Upgrading Your Configuration System

Riak 2.0 offers a replacement configuration system, based on the
[Cuttlefish](https://github.com/basho/cuttlefish) project, that both
simplifies configuration syntax and utilizes one configuration file,
`riak.conf`, instead of two (`app.config` and `vm.args`). Full
documentation of the new system can be in the [[configuration files]]
document.

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
