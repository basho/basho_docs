---
title: "Riak Redis Add-on"
description: "Redis Add-on for Riak KV"
project: "riak_kv"
project_version: "2.0.7"
menu:
  riak_kv-2.0.7:
    name: "Redis Add-on"
    identifier: "add-ons_redis"
    weight: 101
    parent: "add-ons"
toc: true
commercial_offering: true
canonical_link: "https://docs.basho.com/riak/kv/latest/add-ons/redis"
---


[addon redis develop]: ./developing-rra/
[addon redis features]: ./redis-add-on-features/
[addon redis setup]: ./set-up-rra/
[addon redis use]: ./get-started-with-rra/
[ee]: http://basho.com/contact/


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

## Get Started

* [Set up RRA.][addon redis setup]
* [Use RRA with various clients.][addon redis use]
* [Develop with RRA.][addon redis develop]
* [Learn about RRA's features.][addon redis features]

