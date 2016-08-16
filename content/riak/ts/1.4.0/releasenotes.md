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
canonical_link: "https://docs.basho.com/riak/ts/latest/releasenotes"
---


Released August 18, 2016.

Riak TS 1.4.0 delivers a broad range of new functionality and improvements, including: 

* GROUP BY, a SQL statement used with `SELECT`
* Simplified time/date parsing and ISO 8601 support
* Rolling Upgrade/Downgrade support 
* Data expiry (per cluster), and 
* Additional SQL statements for `EXPLAIN`, `SHOW TABLES`, and `DESCRIBE`.


## New Features

* The GROUP BY statement allows you to pick out and condense rows sharing the same value into a single row. You can read more about `GROUP BY` [here](/riak/ts/1.4.0/using/querying/select/GROUP-BY/).
    * [[PR #1445](https://github.com/basho/riak_kv/pull/1445)]
    * [[riak_core PR #848](https://github.com/basho/riak_core/pull/848)]
    * [[riak_ql PR #132](https://github.com/basho/riak_ql/pull/132)]
* [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) strings are now supported as timestamps for SELECT and INSERT statements in Riak TS. You can find out more about timestamps and ISO 8601 [here](riak/ts/1.4.0/using/timerepresentations/).
    * [[PR #1444](https://github.com/basho/riak_kv/pull/1444)]
    * [[riak_core PR #847](https://github.com/basho/riak_core/pull/847)]
    * [[riak PR #862](https://github.com/basho/riak/pull/862)]
    * [[riak_ee PR #403](https://github.com/basho/riak_ee/pull/403)]
    * [[riak_shell PR #43](https://github.com/basho/riak_shell/pull/43)]
* You can now configure global object expiry and time to live (TTL) for your Riak TS data. Read more about data expiry [here](riak/ts/1.4.0/using/global-object-expiration/).
    * [[eleveldb PR #210](https://github.com/basho/eleveldb/pull/210)]
* Three additional SQL statements are available in Riak TS. `EXPLAIN`, which allows you to see what running a given query will do; `SHOW TABLES`, which lists all the TS tables you've created; and `DESCRIBE`, which returns the definition of your TS table in rows and columns.
    * `EXPLAIN`
      * [[riak_ql PR #135](https://github.com/basho/riak_ql/pull/135)]
      * [[PR # 1458](https://github.com/basho/riak_kv/pull/1458)]
      * [[riak_shell PR #46](https://github.com/basho/riak_shell/pull/46)]
   * `SHOW TABLES`
     * [[riak_ql PR #133](https://github.com/basho/riak_ql/pull/133)]
     * [[PR #1448](https://github.com/basho/riak_kv/pull/1448)]
     * [[riak_shell PR #44](https://github.com/basho/riak_shell/pull/44)]
   * `DESCRIBE`
     * [[PR #1438](https://github.com/basho/riak_kv/pull/1438)]
* Riak TS 1.4.0 supports rolling upgrades from 1.3.0 and downgrades to 1.3.0. You can read about how to perform an upgrade or downgrade [here]([LINK])


## Additions

* ???


##Changes

*  Riak TS now has some TS-specific security settings. You can read more about security topics in Riak TS [here](riak/ts/1.4.0/using/security/). [[PR #1452](https://github.com/basho/riak_kv/pull/1452)]


##Bugfixes

* [[PR #1428](https://github.com/basho/riak_kv/pull/1428)] The security permissions for Riak TS's protocol buffer API have been fixed to return in the correct format of a string rather than an atom.
* [[riak_ql PR#126](https://github.com/basho/riak_ql/pull/126)] `INSERT` now accepts booleans as a valid data type.
* [[repl PR#744](https://github.com/basho/riak_repl/pull/744)] When using [MDC](riak/ts/1.4.0/using/mdc/) and enabling realtime replication from the command line, a function verifies that everything has been enabled before populating the realtime queue. [CHECK THIS WITH JOHN D].
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
* OSX 10.8+ (development only)
* AWS Marketplace

## Known Issues

* AAE must be turned off.
* Riak Search is not supported for TS data.
* HTTP API security is not supported. Security checks are included in the code path, but the permissions are not registered with riak_core, so enabling security in HTTP means disabling any TS functionality. See the code [here](https://github.com/basho/riak_kv/blob/riak_ts-develop/src/riak_kv_app.erl#L214-L215).
* You cannot enable Bitcask and use Riak TS.
* SOLR is not available with Riak TS.