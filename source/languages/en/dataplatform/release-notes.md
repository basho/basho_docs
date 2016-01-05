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

Released January 6, 2016.

This release adds integration with [Riak TS (Time Series) 1.0][riak ts], improvements to Spark integration, and write-through cache functionality with Redis.

##Changes

###Core

* (Enterprise Only) Incorporates official [Riak TS 1.0][riak ts] storage instance. Note: OSS version still bundles custom version of Riak KV.

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
* Support for Riak TS Range Scan queries (currently applies to Riak TS Enterprise only).
* Tested with Spark 1.5.2.

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

##Upgrading

Because of multiple changes, the upgrade path is not supported. Please do clean install of BDP 1.1.0.

##Bugs Fixed

* Service manager starts spurious Spark masters (DP-247).
* Spark-Riak Connector - full bucket read doesn't return all results depending on `spark.riak.input.fetch-size` value ([PR](https://github.com/basho/spark-riak-connector/pull/31) / DP-270).
* Spark-Riak Connector integration test fails sometimes ([Commit](https://github.com/basho/spark-riak-connector/commit/2f5e92336eb3fe0ce2c0d382b546b69aef82eb5c) / DP-274).

##Known Issues

* (Enterprise Only) Bundled Riak TS 1.0.0 needs to be patched to resolve a memory leak while servicing GET requests. [Learn more](https://help.basho.com/forums/23099466-Patches).
