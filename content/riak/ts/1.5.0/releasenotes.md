---
title: "Riak TS Release Notes"
description: "Riak TS 1.5.0 Release Notes"
menu:
  riak_ts-1.5.0:
    name: "Release Notes"
    identifier: "release_notes"
    weight: 101
    parent: "introduction"
project: "riak_ts"
project_version: "1.5.0"
toc: true
aliases:
    - /riakts/1.5.0/releasenotes
---

Released December 20, 2016.

Riak TS 1.5.0 expands its SQL implementation. It includes additional SELECT query clauses: `ORDER BY` and `LIMIT`. It also includes `DELETE` for a single record. TS 1.5 also includes `ASC` and `DESC` keywords in table schema definition, and better usage of the `NULL` keyword in `WHERE` clause conditions and INSERT statements.

TS 1.5.0 adds `SHOW CREATE TABLE` to review the SQL statement used to create the table. This statement is useful for re-creating your table from a testing to a deployment cluster, or to another data-center.

Data storage of unstructured (binary) or opaque (JSON) data has also gotten a little easier in TS 1.5 with the introduction of a BLOB data type.

Updates in TS 1.5 include multi-line paste functionality, built-in help for SQL commands, and enhanced error handling in riak shell.

Riak TS has significantly improved performance, thanks to streamlining of the on-disk encoding format and increased parallelization of record decoding. These updates have focused on improvement of SQL query latency, and result in typical speedups of 2-4x for large queries, particularly for data sets that are distributed around the cluster.


## New Features

* `ASC` and `DESC` have been added to the CREATE TABLE statement. Adding the ASC/DESC keywords to your local key during `CREATE TABLE` means you can have your data pre-sorted in ascending or descending order as it's input into your TS table. You can read more about `ASC`/`DESC` in the local key [here]({{<baseurl>}}riak/ts/1.5.0/using/planning).
    * [[PR 1427](https://github.com/basho/riak_kv/pull/1427)]
    * [[PR 1500](https://github.com/basho/riak_kv/pull/1500)]
    * [[PR 1558](https://github.com/basho/riak_kv/pull/1558 )]
    * [[PR 1560](https://github.com/basho/riak_kv/pull/1560 )]
    * [[riak_ql PR 117](https://github.com/basho/riak_ql/pull/117 )]
    * [[riak_ql PR 162](https://github.com/basho/riak_ql/pull/162 )]
    * [[riak_test PR 1200](https://github.com/basho/riak_test/pull/1200)]
    * [[riak_test PR 1081](https://github.com/basho/riak_test/pull/1081)]
    * [[riak_test PR 1201](https://github.com/basho/riak_test/pull/1201 )]
* The ORDER BY statement has been added to `SELECT`, allowing you to sort the results of your query in various ways, including: ascending or descending order, or nulls first or last. You can learn about `ORDER BY` [here]({{<baseurl>}}riak/ts/1.5.0/using/querying/select/order-by).
    * [[PR 1479](https://github.com/basho/riak_kv/pull/1479)]
    * [[riak erlang client PR 321](https://github.com/basho/riak-erlang-client/pull/321)]
    * [[riak_pb PR 208](https://github.com/basho/riak_pb/pull/208)]
    * [[riak_test PR 1152](https://github.com/basho/riak_test/pull/1152)]
*  `LIMIT` allows you to specify that you only want a specific number of records from your query, and it can be expanded by `OFFSET`. You can read about how to use the LIMIT statement [here]({{<baseurl>}}riak/ts/1.5.0/using/querying/select/limit).
    * [[PR 1479](https://github.com/basho/riak_kv/pull/1479)]
    * [[riak erlang client PR 321](https://github.com/basho/riak-erlang-client/pull/321)]
    * [[riak_pb PR 208](https://github.com/basho/riak_pb/pull/208)]
    * [[riak_test PR 1152](https://github.com/basho/riak_test/pull/1152)]
* You can now use `DELETE` from riak shell to remove a record from your TS table. Learn all about `DELETE` [here]({{<baseurl>}}riak/ts/1.5.0/using/querying/delete).
    * [[PR 1552](https://github.com/basho/riak_kv/pull/1552)]
    * [[riak_ql PR 145](https://github.com/basho/riak_ql/pull/145)]
    * [[riak_shell PR 23](https://github.com/basho/riak_shell/pull/23)]
    * [[riak_test 1216](https://github.com/basho/riak_test/pull/1216)]
* You can now use `INSERT NULL` to add null values. You can do so explicitly, by specifying NULL in the VALUES column, or implicitly, by omitting the column. This change also allows for column re-ordering within the INSERT statement. You can select records based on whether or not they contain NULL values in specific fields using `WHERE »field« IS [NOT] NULL`.
    * [[PR 1507](https://github.com/basho/riak_kv/pull/1507)]
    * [[PR 1496](https://github.com/basho/riak_kv/pull/1496)]
    * [[riak_ql PR 142](https://github.com/basho/riak_ql/pull/142)]
    * [[riak_ql PR 144](https://github.com/basho/riak_ql/pull/144)]
    * [[riak_shell PR 56](https://github.com/basho/riak_shell/pull/56)]
    * [[riak_test PR 1169](https://github.com/basho/riak_test/pull/1169)]
* You can now run `SHOW CREATE TABLE` to review SQL definition and replication properties of existing Riak TS tables. You can read more about the SHOW CREATE TABLE statement [here]({{<baseurl>}}riak/ts/1.5.0/using/querying/show-create-table).
    * [[PR 1536](https://github.com/basho/riak_kv/pull/1536)
    * [[riak_ql 155](https://github.com/basho/riak_ql/pull/155)]
    * [[riak_ql 159](https://github.com/basho/riak_ql/pull/159 )]
    * [[riak_shell PR 62](https://github.com/basho/riak_shell/pull/62)]
    * [[riak_test PR 1193](https://github.com/basho/riak_test/pull/1193)]
    * [[riak_test PR 1211](https://github.com/basho/riak_test/pull/1211)]
* A BLOB data type is now available. BLOB allows the storage of unstructured data, binary or opaque (JSON), in a Riak TS column. Learn about BLOB data type [here]({{<baseurl>}}riak/ts/1.5.0/using/writingdata/#blob-data).
    * [[PR 1540](https://github.com/basho/riak_kv/pull/1540)]
    * [[riak_pb PR 211](https://github.com/basho/riak_pb/issues/211)]
    * [[riak_ql PR 156](https://github.com/basho/riak_ql/issues/156)]
    * [[riak_ql PR 143](https://github.com/basho/riak_ql/pull/143)]
    * [[riak_shell PR 61](https://github.com/basho/riak_shell/pull/61 )]
    * [[riak_test PR 1212](https://github.com/basho/riak_test/pull/1212)]
* Multi-line paste and in-line SQL help is now available in riak shell. SQL comments are supported in the following types: `/* blah multiline */` and `-- single line`. Riak shell has also been updated to better handle errors. And it now pretty print floats, as well.
    * [[riak_shell PR 60](https://github.com/basho/riak_shell/pull/60)]
    * [[riak_shell PR 58](https://github.com/basho/riak_shell/pull/58)]
    * [[riak_shell PR 57](https://github.com/basho/riak_shell/pull/57)]
    * [[riak_ql 154](https://github.com/basho/riak_ql/pull/154 )]
 * TTB has replaced msgpack as the on-disk encoding format. This replacement has demonstrated an improvement in latency reduction and efficiency.
    * [[PR 1510](https://github.com/basho/riak_kv/pull/1510)]
    * [[eleveldb PR 223](https://github.com/basho/eleveldb/pull/223)]
* Query results are now decoded in a way that leverages parallelization across your TS cluster. This update speeds up large queries particularly for data residing on multiple nodes.
    * [[PR 1538](https://github.com/basho/riak_kv/pull/1538)]


## Additions

* The timestamp type is now able to be used as an argument in aggregate functions. [[riak_ql PR 146](https://github.com/basho/riak_ql/pull/146) & [riak_ql PR 147](https://github.com/basho/riak_ql/pull/147)]
* You can now see the Status field of your TS table when you use `SHOW TABLES`. [[PR 1514](https://github.com/basho/riak_kv/pull/1514 ) and
[PR 1176](https://github.com/basho/riak_test/pull/1176 )]
* Introduced the following new parameters in riak.conf. See the [TS configuration docs]({{<baseurl>}}riak/ts/1.5.0/configuring/riakconf) for details. [[PR 1505](https://github.com/basho/riak_kv/pull/1505)]
    * riak_kv.query.timeseries.max_returned_data_size
    * riak_kv.query.timeseries.max_running_fsms
    * riak_kv.query.timeseries.qbuf_root_path
    * riak_kv.query.timeseries.maximum_query_queue_length


## Changes

* Object size limitations, as set in riak.conf, have been decreased. `object.size.warning_threshold` has been decreased from a default of 5MB to 50kB. `object.size.maximum` has been decreased from 50MB to 500kB. [[PR 1505](https://github.com/basho/riak_kv/pull/1505) & [PR 1218](https://github.com/basho/riak_test/pull/1218/ )]
* If a bucket type property contains a `ddl` property we infer that it's a time series table. New defaults have been also been added in the event they are not specified. [[riak_core PR 870](https://github.com/basho/riak_core/pull/870)]
* The maximum number of index FSMs used to serve TS queries is now configurable. [[PR 1550](https://github.com/basho/riak_kv/pull/1550)]
* Write-once conflict resolution has been changed to be more predictable. It is now based on timestamp rather than SHA-1 hash on value part. [[PR 1512](https://github.com/basho/riak_kv/pull/1512)]
* LevelDB has been updated to version 2.0.33 [[eleveldb PR 231](https://github.com/basho/eleveldb/pull/231)]
* LZ4 is now the default compression for LevelDB. [[leveldb PR 164](https://github.com/basho/leveldb/pull/164) & [eleveldb PR 208](https://github.com/basho/eleveldb/pull/208)]
* Updated the default value for `riak_kv.query.timeseries.max_quanta_span`. See the [TS configuration docs]({{<baseurl>}}riak/ts/1.5.0/configuring/riakconf) for details. **Note:** due to a bug in the code, the `max_quanta_span` is capped at 1000. [[PR 1505](https://github.com/basho/riak_kv/pull/1505)]
* The default value for `OFFSET` is `[ ]`. [[PR 1546](https://github.com/basho/riak_kv/pull/1546)]


## Bugfixes

* [[Issue 1418](https://github.com/basho/riak_kv/issues/1418)/[PR 1544](https://github.com/basho/riak_kv/pull/1544) & [PR 1204](https://github.com/basho/riak_test/pull/1204 )] A bad error atom type was causing the protobuf and TTB services to crash. Error reporting for overload and other types of error conditions have been added.
* [[riak_shell Issue 33](https://github.com/basho/riak_shell/issues/33)/[riak_shell PR 59](https://github.com/basho/riak_shell/pull/59)] When unknown crashes occurred server-side, riak shell would get the report and crash itsef. Now, riak shell does not crash upon receiving unknown server-side crashes, and instead prompts you to report the bug to Basho.
* [[riak_shell PR 68](https://github.com/basho/riak_shell/pull/68), [riak_shell PR 70](https://github.com/basho/riak_shell/pull/70), & [PR 1239](https://github.com/basho/riak_test/pull/1239 )] Unicode in SQL could crash riak shell.
* [[riak_ql PR 148](https://github.com/basho/riak_ql/pull/148)] It was possible to create a Riak TS table without a local key. Doing so would result in the local key becoming an exact copy of the partition key, meaning the quantum would be listed in-full rather than in a plain field name. This caused many breaks and bugs. The local key is now required, and a table creation will be unsuccessful without it.
* [[PR 1479](https://github.com/basho/riak_kv/pull/1479)] The length of the queue was supposed to be configurable, but was being overridden by supervisor. The queue is now actually configurable.
* [[PR 1478](https://github.com/basho/riak_kv/pull/1478)] A rare error in the EXPLAIN statement was caused by a function_clause error in `riak_pb_ts_codec:encode_field_type/1` due to a typo in the code. The typo has been fixed.
* [[Issue 1472](https://github.com/basho/riak_kv/issues/1472)/[PR 1474](https://github.com/basho/riak_kv/pull/1474)] The error message has been improved when selecting an empty time range.
* [[PR 1516](https://github.com/basho/riak_kv/pull/1516)] The equality operator was not working correctly with timestamp. The timestamp can now be queried using equality operators.
* [[PR 1545](https://github.com/basho/riak_kv/pull/1545 ) & [riak_test PR 1208](https://github.com/basho/riak_test/pull/1208)] Attempting to insert data into a non-existant table would cause crash.
* [[PR 1130](https://github.com/basho/riak_ql/pull/148 )] It was possible to create a table without specifying a local key - the primary key would be copied over. This led to strange tables with no valid use. This now correctly returns an error.
* [[PR 1286](https://github.com/basho/riak_ql/pull/147/files )] Aggregation functions like MAX, MIN etc would cast timestamps to sint64 by default. They now correctly return values of type timestamp which appear correctly as times in riak-shell.
* [[PR 157](https://github.com/basho/riak_ql/pull/157 ) & [PR 1206](https://github.com/basho/riak_test/pull/1206 )] SQL insert statements of the form INSERT INTO mytable (field1, field2, field3) VALUES(val1, val2, val3) were buggy.


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

You can see a table of KV and TS features [here]({{<baseurl>}}riak/ts/1.5.0/using/core-fundamentals/).
