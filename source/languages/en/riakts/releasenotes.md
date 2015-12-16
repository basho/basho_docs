---
title: Riak TS 1.0.0 Release Notes
project: riakts
version: 1.0.0+
document: guide
audience: beginner
toc: true
keywords: [time series, release notes]
---

Released December 15, 2015.

This release is the introductory release of Riak TS. 


##Features

###Customize Tables

Riak TS enables you to define and configure tables of time series data as a riak bucket type, and write data to these tables. The schema of Riak TS's tables are generated as bucket properties and installed as bucket types, which allows you to structure data as it is coming in and store both structured and semi-structured data.

###Data Locality

The structure of Riak TS tables enable data locality based on composite key (including time quanta), which allows for rapid querying and near-linear scaling.

###SQL-like Queries

You can query your data in Riak TS using a subset of SQL.

###Single Key DELETEs and GETs

Riak TS enables single-key DELETs and GETs, which allow you to read and modify data without writing SQL.

###List Key

The list key feature allows you to issue an API call to list all of the keys in your Riak TS database. This can be a useful operation, but it is incredibly resource intensive as all keys must be read and processed. 

###Java, Python, and Erlang Clients

Riak TS offers protobuf requests through three different client APIs: Erlang, Java, and Python.


##Compatibility

Riak TS is compatible with the following operating systems:

* RHEL/CentOS 6
* RHEL/CentOS 7
* Ubuntu 12.04 LTS
* Ubuntu 14.04 LTS
* Debian 6 (development only)
* Debian 7 (development only)
* OSX 10.8 (development only)


##Known Issues

* AAE must be turned off.
* Riak Search is not supported.
* Multi-Datacenter Replication is not supported.
* When deleting, a PUT occurs to write the tombstone, then a GET reaps the tombstone. Since PUT and GET are asynchronous, it is possible for the GET to occur before the PUT resulting in the data not actually being deleted.  If this occurs, issue the DELETE again.