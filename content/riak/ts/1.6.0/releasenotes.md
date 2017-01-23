---
title: "Riak TS Release Notes"
description: "Riak TS 1.6.0 Release Notes"
menu:
  riak_ts-1.6.0:
    name: "Release Notes"
    identifier: "release_notes"
    weight: 101
    parent: "introduction"
project: "riak_ts"
project_version: "1.6.0"
toc: true
aliases:
    - /riakts/1.6.0/releasenotes
canonical_link: "https://docs.basho.com/riak/ts/latest/releasenotes"
---

Released [Month] [Day], [Year].

Riak TS 1.6.0 does some things.


## New Features

* Description
    * [[PR #](LINK)]


## Additions

* Description. [[PR #](LINK)]


## Changes

* We have unified both the way `CREATE TABLE` is serviced and the way we ensure the table is query-ready. [[PR 1577](https://github.com/basho/riak_kv/pull/1577)]
* Query filters are now used in start/end key. With this change, filters on the local key are put into the start/end keys to reduce the range that eLevelDB has to cover. [[PR 1594](https://github.com/basho/riak_kv/pull/1594)]


## Bugfixes

* [[PR 1581](https://github.com/basho/riak_kv/pull/1581 )] A WHERE clause filtered on the quantum column, that used the = operator, would sometimes return a `Neither upper or lower time bounds were specified in the query` error.
* [[PR 1583](https://github.com/basho/riak_kv/pull/1583 )] The configuration property name for the maximum number of quanta allowed in a query was initially using the incorrect key and overriding configured values.
* [[PR 1590](https://github.com/basho/riak_kv/pull/1590 )] Fix for queries not returning results for tables using `DESC` on the local key, which could occur under some table schemas.
* 


## Compatibility

Riak TS is compatible with the following:

* RHEL/CentOS 6
* RHEL/CentOS 7
* Ubuntu 14.04 (Trusty) LTS
* Ubuntu 16.04 (Xenial) LTS*
* Debian 7 "Wheezy"(development only)
* Debian 8 "Jessie"
* OS X 10.11+ (development only)
* Amazon Linux 2016.09


## Known Issues

* AAE must remain turned off.
* You cannot use Bitcask with Riak TS tables.
* `riak_kv.query.timeseries.max_quanta_span` is capped at 1000 due to a bug.

You can see a table of KV and TS features [here](/riak/ts/1.6.0/using/core-fundamentals/).
