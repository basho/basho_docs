---
title: "Riak Redis Add-on"
description: "Redis Add-on for Riak KV"
project: "riak_kv"
project_version: 2.9.2
menu:
  riak_kv-2.9.2:
    name: "Redis Add-on"
    identifier: "add-ons_redis"
    weight: 101
    parent: "add-ons"
toc: true
commercial_offering: true
---


[addon redis develop]: ./developing-rra/
[addon redis features]: ./redis-add-on-features/
[addon redis setup]: ./set-up-rra/
[addon redis use]: ./get-started-with-rra/
[ee]: https://www.tiot.jp/en/about-us/contact-us/

{{% note title="Warning: No longer actively maintained" %}}
Since moving to Open Source, the Riak Redis Add-on is no longer actively maintained. As basic functionality has not changed, we expect the add-on to continue working with newer versions without incident but cannot guarantee this. The text below is left from the last known good version.
{{% /note %}}

Riak Redis Add-on (RRA) is a distributed cache service that joins the power of Redis caching with the eventual consistency guarantees of Riak KV.

RRA enables you to reduce latency for Riak KV reads through the use of a distributed cache layer. This type of caching is most effective for keys that are immutable or have an infrequent change rate.

Whether you are looking to build out a session, shopping cart, advertisement or other dynamically-rendered copy, RRA helps reduce read pressure on your persistent store (Riak KV).

## Compatibility

RRA is supported on the following platforms:

* RHEL/CentOS 6
* RHEL/CentOS 7
* Ubuntu 12.04 LTS "Precise Pangolin"
* Ubuntu 14.04 LTS "Trusty Tahr"
* Debian 7 "Wheezy"
* Debian 8 "Jessie"

RRA is compatible with the following services:

* Riak KV Enterprise (2.1.4+)
* Riak TS Enterprise (1.4.0+)
* Redis 2.x and 3.x (in 3.x, not supporting Redis Cluster)
  * Redis Cluster and RRA's consistent hash are at odds, which surface as errors
    such as MOVED, ASK, and CROSSSLOT messages from Redis, see (WIP):
    https://github.com/antirez/redis-rb-cluster

## Get Started

* [Set up RRA.][addon redis setup]
* [Use RRA with various clients.][addon redis use]
* [Develop with RRA.][addon redis develop]
* [Learn about RRA's features.][addon redis features]
