---
title: "Riak TS Release Notes"
description: "Riak TS 1.3.0 Release Notes"
menu:
  riak_ts-1.3.0:
    name: "Release Notes"
    identifier: "release_notes"
    weight: 101
    parent: "introduction"
project: "riak_ts"
project_version: "1.3.0"
toc: true
aliases:
    - /riakts/1.3.0/releasenotes
---


Released May 4, 2016.

Riak TS 1.3.0 is [open source](https://github.com/basho/riak/tree/riak_ts-1.3.0)! In addition to becoming OSS, version 1.3.0 introduces a broad range of new functionality including: an HTTP API, additional SQL commands, and relaxed key restrictions. It also includes Multi-Datacenter (MDC) replication for our Enterprise users.

We've also added AWS AMI support. You can find instructions for installing Riak TS on AWS [here]({{< baseurl >}}riak/ts/1.3.0/installing/aws/).


## New Features

* Riak TS is now open source. You can find the source code [here](https://github.com/basho/riak/tree/riak_ts-1.3.0)! 
    * [[PR #815](https://github.com/basho/riak/pull/815)]
    * [[riak_ql PR #100](https://github.com/basho/riak_ql/pull/100)]
* The HTTP API is a development tool added for development and debugging purposes. It is ideal for quick prototyping efforts, and provides API calls for accessing Riak TS data over HTTP.
    * [[PR #1398](https://github.com/basho/riak_kv/pull/1398)]
* Our supported SQL commands have expanded to include `WITH` and `INSERT`. The `WITH` clause makes creating a TS table even easier, and `INSERT` provides another way to add data to your TS table.
    * [[PR #1369](https://github.com/basho/riak_kv/pull/1369)]
    * [[PR #805](https://github.com/basho/riak_core/pull/805)]
    * [[PR #806](https://github.com/basho/riak_core/pull/806)]
    * [[PR #1328](https://github.com/basho/riak_kv/pull/1328)]
    * [[PR #1332](https://github.com/basho/riak_kv/pull/1332)]
    * [[PR #1364](https://github.com/basho/riak_kv/pull/1364)]
    * [[riak_ql PR #110](https://github.com/basho/riak_ql/pull/110)]
    * [[riak_shell PR #25](https://github.com/basho/riak_shell/pull/25)]
* The relaxed key restrictions mean the family and series keys are no longer required, which makes TS table schemas more flexible, and makes customizing the data you store and how you store even easier. 
    * [[PR #1357](https://github.com/basho/riak_kv/pull/1357)]
    * [[riak_ql PR #108](https://github.com/basho/riak_ql/pull/108)]
* TS now supports MDC replication on TS tables. At this time, MDC support in TS does not include AAE fullsync. To use MDC, you will need to create your TS tables in your clusters and then [configure]({{< baseurl >}}riak/ts/1.3.0/using/mdc/) MDC. 
    * [[riak_repl PR #738](https://github.com/basho/riak_repl/pull/738)]
    * [[PR #1381](https://github.com/basho/riak_kv/pull/1381)]
* Riak TS now offers integration with PHP and .NET clients.
    * [[PR #120](https://github.com/basho/riak-php-client/pull/120) and [PR #2](https://github.com/basho/riak-phppb-client/pull/2)]
    * [[PR #302](https://github.com/basho/riak-dotnet-client/pull/302)]


## Additions

* Support has been added for TTB query messages. [[riak_api PR #109](https://github.com/basho/riak_api/pull/109), [riak_pb PR #182](https://github.com/basho/riak_pb/pull/182), [PR #1378](https://github.com/basho/riak_kv/pull/1378), and [erlang client PR #267](https://github.com/basho/riak-erlang-client/pull/267)]


## Changes

*  AWS AMI is now available. You can find instructions for installing Riak TS on AWS [here]({{< baseurl >}}riak/ts/1.3.0/installing/aws/). [[PR #89](https://github.com/basho/aws-ansible/pull/89)]
* riak shell has had several changes:  the `command` record (which includes result) has been added, optional debugging has been added to exceptions, and SQL commands are allowed to span multiple lines. [[PR #23](https://github.com/basho/riak_shell/pull/23)]
* Several changes have been made to facilitate rolling upgrades/downgrades in future releases:
    * The DDL compiler has been updated to facilitate rolling upgrade/downgrade functionality in future releases. [[riak_ql PR #115](https://github.com/basho/riak_ql/pull/115)]
    * On startup, the Riak node will walk through the riak_kv_compile DETS table and force a recompile, which will help facilitate rolling upgrade/downgrade functionality in future releases. [[PR #1377](https://github.com/basho/riak_kv/pull/1377)]
    * The version that DDLs were compiled with will be stored in the dets table. [[PR #1377](https://github.com/basho/riak_kv/pull/1377)]
    * The DDL compiler's version is registered as a capability. [[PR #1377](https://github.com/basho/riak_kv/pull/1377)]
* elevelDB has been updated to pull in levelDB version 2.0.15. [[eleveldb PR #184](https://github.com/basho/eleveldb/pull/184)]
* node_package has been updated to version 3.0.0 to address a [security issue]({{<baseurl>}}community/productadvisories/codeinjectioninitfiles/) in which arbitrary root access was possible for a local user that had direct access to the Riak account. [[PR #820](https://github.com/basho/riak/pull/820)]
* module_info calls have been removed from riak_core_coverage_fsm:init() to speed up small queries. [[PR #829](https://github.com/basho/riak_core/pull/829)]


## Bugfixes

* [[Issue #29](https://github.com/basho/riak_ee-issues/issues/29)/[PR #111](https://github.com/basho/riak_ql/pull/111)] The quantum time period was not validated correctly. Validation of the partition key has been added to ensure only one quantum (if any) is specified during table creation, as specifying more than one causes errors.
* [[PR #1372](https://github.com/basho/riak_kv/pull/1372)] A bug manifested when using '>' with a right side value one less that the quanta boundary.
* [[PR #1377](https://github.com/basho/riak_kv/pull/1377)] Fix DETs to accomodate downgrades.
* [[PR #1354](https://github.com/basho/riak_kv/pull/1354)] Negation of an aggregate function returned an error, but is now allowed.


## Compatibility

Riak TS is compatible with the following:

* RHEL/CentOS 6
* RHEL/CentOS 7
* Ubuntu 12.04 LTS
* Ubuntu 14.04 LTS
* Debian 7 (development only)
* OS X 10.8+ (development only)
* AWS Marketplace


## Known Issues

* AAE must be turned off.
* Riak Search is not supported for TS data.
* HTTP API security is not supported. Security checks are included in the code path, but the permissions are not registered with riak_core, so enabling security means disabling any TS functionality. [[code](https://github.com/basho/riak_kv/blob/riak_ts-develop/src/riak_kv_app.erl#L214-L215)]
* Quanta with a '0' or negative integers are not supported and will cause errors.
