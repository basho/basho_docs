---
title: Basho Data Platform 1.1.0 Release Notes
project: dataplatform
version: 1.1.0+
document: guide
audience: beginner
toc: true
keywords: [developers]
---

[bdp downloads]: http://docs.basho.com/dataplatform/1.1.0/downloads/
[bdp install]: http://docs.basho.com/dataplatform/1.1.0/installing/
[riak ts]: http://docs.basho.com/riakts/1.0.0/
[spark-riak connector]: https://github.com/basho/spark-riak-connector

Released December 23, 2015.

This release adds number of new features, most important are the integration with newly released Riak TS (Time Series) 1.0, multiple new features and improvements in Spark integration and addition of write-through cache functionality in Redis integration.

##Changes

###Core

* (Enterprise Only) Incorporates [Riak TS 1.0][riak ts] storage instance. Note: OSS version still bunldes custom version of Riak KV.

###Service Manager

* Command-line interface additions.
* Improve scripting support with exit codes.

###Cache Proxy + Redis

* (Enterprise only) Support for write-through operations (Redis SET & DELETE commands).
* Open sourcing of read-through functionality (Redis GET command).

###Spark-Riak Connector

* Open sourcing of Spark-Riak Connector.
* Improve full bucket reads performance.
* Basic support for Spark DataFrames.
* Added performance benchmarks.
* Support for Riak TS Range Scan queries (currenlty applies to Riak TS Enterprise only)
* Tested with Spark 1.5.2

##Compatibility

Basho Data Platform is compatible with the following operating systems:

* RHEL/CentOS 6
* RHEL/CentOS 7
* Ubuntu 12.04 LTS
* Ubuntu 14.04 LTS
* Amazon Linux

BDP also supports the following operating systems for development:

* OSX 10.10
* Debian 7

##Installing

You can find the install packages for BDP [here][bdp downloads] and the instructions to walk you through the installation process [here][bdp install].

##Bugs Fixed

* [DP-247](https://bashoeng.atlassian.net/browse/DP-247) Service manager starts spurious Spark masters.
* [DP-270](https://bashoeng.atlassian.net/browse/DP-270) Spark-Riak Connector - full bucket read doesn't return all results depending on spark.riak.input.fetch-size value.
* [DP-274](https://bashoeng.atlassian.net/browse/DP-274) Spark-Riak Connector integration test fails sometimes.

##Known Issues

* (Enterprise Only) Bundled Riak TS 1.0.0 needs to be patched to resolve a memory leak while servicing get requests. [Learn more](TODO add link to Riak TS patch here).
