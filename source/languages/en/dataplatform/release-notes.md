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
[spark-riak connector]: #

Released December 23, 2015.

This release is DESCRIPTION HERE

##Changes

###Core

* Incorporate [Riak TS][riak ts] features.

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
* Support for Riak TS Range Scan queries.
* Add performance benchmarks.

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

##Known Issues
