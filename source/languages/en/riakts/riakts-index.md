---
title: Riak TS
project: riakts
version: 1.0.0+
document: index
toc: true
index: true
audience: beginner
keywords: [riak, riak ts, getting started, time series]
body_id: riakts-index
simple: true
versions: true
---

[installing]: http://docs.basho.com/riakts/1.0.0/installing/installing/
[learnabout]: http://http://docs.basho.com/riakts/1.0.0/learn-about/learn-about/


Riak TS is a distributed NoSQL key/value store optimized for time series data. It provides a performant time series database solution that is extensible and scalable.

Riak TS is an available and partition-tolerate key/value time series database with the ability to co-locate keys of the same series for easy and efficient range queries using a subset of SQL. Riak TS (which includes a complete build of KV) adds the ability to co-locate keys within the same quanta for easy querying.

With Riak TS, you can decided what sort of time-based data is grouped together and where it it stored, which optimizes your READ times.

>**Note:** At this release, LevelDB is the only supported backend.


##Supported Operating Systems

* CentOS 6
* CentOS 7
* Debian 6 
* Debian 7
* OS X 10.8 (Development only)
* RHEL 6
* RHEL 7
* Ubuntu 12.04
* Ubuntu 14.04

##Get Started

Get started with Riak TS by downloading the package from ZenDesk, then check out how to [install][installing] it. 

If you want to learn more about how Riak TS was designed and how it is structured, checkout our [Learn About Riak TS][learnabout] section.

When deleting, a PUT occurs to write the tombstone, then a GET reaps the tombstone. Since PUT and GET are asynchronous, it is possible for the GET to occur before the PUT resulting in the data not actually being deleted. 

Not sure how you want to say this but...the recommended behavior is to issue the DELETE again. If the RACE condition is met with the first time it will delete it on the subsequent DELETE runs...
