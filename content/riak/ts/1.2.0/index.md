---
title: "Riak TS"
description: "Riak TS"
menu:
  riak_ts-1.2.0:
    name: "Riak TS"
    identifier: "introduction"
    weight: 100
    pre: riak
project: "riak_ts"
project_version: "1.2.0"
toc: true
aliases:
    - /riakts/1.2.0/
---

[installing]: {{<baseurl>}}riak/ts/1.2.0/installing/
[learnabout]: learn-about/


Riak TS is a distributed NoSQL key/value store optimized for time series data. It provides a time series database solution that is extensible and scalable.

Riak TS includes a complete build of Riak KV, but adds the ability to co-locate keys of the same series within the same quanta for fast and efficient READs. As  an available and partition-tolerant time series database, Riak TS uses a subset of SQL to make querying even easier.


## Supported Operating Systems

* CentOS 6
* CentOS 7
* RHEL 6
* RHEL 7
* Ubuntu 12.04
* Ubuntu 14.04
* Debian 7 (development only)
* OS X 10.8 (development only)

>**Note:** At this release, LevelDB is the only supported backend.


## Get Started

Get started with Riak TS by downloading the package from ZenDesk, then check out how to [install][installing] it. 

If you want to learn more about how Riak TS was designed and how it is structured, checkout our [Learn About Riak TS][learnabout] section.