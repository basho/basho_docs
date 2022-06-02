---
title: "Riak TS Release Notes"
description: "Riak TS 1.4.0 Release Notes"
menu:
  riak_ts-1.4.0:
    name: "Release Notes"
    identifier: "release_notes"
    weight: 101
    parent: "introduction"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/releasenotes
---


Released August 24, 2016.

Riak TS 1.4.0 delivers a broad range of new functionality and improvements, including: 

* GROUP BY, a SQL statement used with `SELECT`
* Simplified time/date parsing and ISO 8601 support
* Rolling Upgrade/Downgrade support 
* Global data expiry (per cluster)
* New SHOW TABLES SQL statement 
* Enhanced `DESCRIBE`


## New Features

* The GROUP BY statement allows you to pick out and condense rows sharing the same value into a single row. You can read more about `GROUP BY` [here]({{<baseurl>}}riak/ts/1.4.0/using/querying/select/group-by/).
    * [[PR #1445](https://github.com/basho/riak_kv/pull/1445)]
    * [[riak_core PR #848](https://github.com/basho/riak_core/pull/848)]
    * [[riak_ql PR #132](https://github.com/basho/riak_ql/pull/132)]
* [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) strings are now supported as timestamps for SELECT and INSERT statements in Riak TS. You can find out more about timestamps and ISO 8601 [here]({{<baseurl>}}riak/ts/1.4.0/using/timerepresentations/).
    * [[PR #1444](https://github.com/basho/riak_kv/pull/1444)]
    * [[riak_core PR #847](https://github.com/basho/riak_core/pull/847)]
    * [[riak PR #862](https://github.com/basho/riak/pull/862)]
    * [[riak_ee PR #403](https://github.com/basho/riak_ee/pull/403)]
    * [[riak_shell PR #43](https://github.com/basho/riak_shell/pull/43)]
* You can now configure global object expiry (a.k.a. time to live - TTL) for your Riak TS data. Read more about data expiry [here]({{<baseurl>}}riak/ts/1.4.0/using/global-object-expiration/).
    * [[eleveldb PR #210](https://github.com/basho/eleveldb/pull/210)]
* `SHOW TABLES`, which lists all the TS tables you've created, is now available. You can read more about the SHOW TABLES statement [here]({{<baseurl>}}riak/ts/1.4.0/using/querying/show-tables/).
    * [[riak_ql PR #133](https://github.com/basho/riak_ql/pull/133)]
    * [[PR #1448](https://github.com/basho/riak_kv/pull/1448)]
    * [[riak_shell PR #44](https://github.com/basho/riak_shell/pull/44)]
* Riak TS 1.4.0 supports rolling upgrades from 1.3.1 and downgrades to 1.3.1. You can read about how to perform an upgrade or downgrade [here]({{<baseurl>}}riak/ts/1.4.0/setup/).
{{% note title="Note on Downgrading" %}}
If you freshly installed TS 1.4.0 and did NOT upgrade from 1.3.1, and then you choose to downgrade to 1.3.1, you will need to change your riak.conf to preserve your configuration settings. Read more about that process [here]({{<baseurl>}}riak/ts/1.4.0/setup/downgrading).
{{% /note %}}


## Additions

* New configuration settings for Riak TS have been added to riak.conf. You can read more about TS's configuration options [here]({{<baseurl>}}riak/ts/1.4.0/using/configuring/). Additionally, the old configuration settings have been exposed and are being deprecated.
    
    >If you were using the old configuration settings, please update riak.conf to use the new settings. The older settings are scheduled to be deprecated.  


## Changes

* Riak TS now has some TS-specific security settings. You can read more about security topics in Riak TS [here]({{<baseurl>}}riak/ts/1.4.0/using/security/). [[PR #1452](https://github.com/basho/riak_kv/pull/1452)]
* The DESCRIBE statement now returns additional information about the interval and unit of time in your TS table. [[PR #1438](https://github.com/basho/riak_kv/pull/1438)]
* LevelDB now uses LZ4 as an internal compression mechanism. This change should provide a performance boost for LevelDB. [[PR #208](https://github.com/basho/eleveldb/pull/208)]


## Bugfixes

* [[PR #1428](https://github.com/basho/riak_kv/pull/1428)] The security permissions for Riak TS's protocol buffer API have been fixed to return in the correct format of a string rather than an atom.
* [[riak_ql PR#126](https://github.com/basho/riak_ql/pull/126)] `INSERT` now accepts booleans as a valid data type.
* [[repl PR#744](https://github.com/basho/riak_repl/pull/744)] When using Riak TS Enterprise, do not push time series data to the realtime replication queue unless it is enabled.
* [[riak_ql #130](https://github.com/basho/riak_ql/pull/130)] Only one table at a time may be queried with `SELECT` and now there is a better error message if you try to query more than one table.
* [[riak_shell #39](https://github.com/basho/riak_shell/pull/39)] Fix some typos in riak shell's help text. 
* [[PR #1439](https://github.com/basho/riak_kv/pull/1439)] When building a query an error could result if a string was used. This has been fixed.


## Compatibility

Riak TS is compatible with the following:

* RHEL/CentOS 6
* RHEL/CentOS 7
* Ubuntu 12.04 LTS
* Ubuntu 14.04 LTS
* Debian 7 (development only)
* Debian 8 (development only)
* OS X 10.8+ (development only)
* AWS Marketplace

## Known Issues

* The list_keys API may be unreliable in clusters containing a mix of TS 1.3.1 and TS 1.4 nodes.
* AAE must be turned off.
* Riak Search, and subsequently Solr, is not supported for TS.
* HTTP API security is not supported. Security checks are included in the code path, but the permissions are not registered with riak_core, so enabling security in HTTP means disabling any TS functionality. See the code [here](https://github.com/basho/riak_kv/blob/riak_ts-develop/src/riak_kv_app.erl#L214-L215).
* Bitcask backend must not be used with Riak TS.
