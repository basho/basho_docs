---
title: "Creating and Activating Your Riak TS Table"
description: "Creating and Activating Your Riak TS Table"
menu:
  riak_ts-1.3.0:
    name: "Create and Activate Your Table"
    identifier: "creating_activating_riakts"
    weight: 302
    parent: "using"
project: "riak_ts"
project_version: "1.3.0"
toc: true
aliases:
    - /riakts/1.3.0/using/creating-activating/
canonical_link: "docs.basho.com/riak/ts/latest/using/creating-activating"
---


[erlang]: ../../developing/erlang#query2
[java]: ../../developing/java#query
[nodejs]: ../../nodejs#query
[python]: ../../developing/python#query
[ruby]: ../../developing/ruby#sql-queries
[planning]: ../planning/
[writing]: ../writingdata/


Once you have [planned out your table][planning] you can create it by

* Executing a `CREATE TABLE` query, using any Riak client, or
* Running the `riak-admin` command (needs to be run as root, using `su` or `sudo`).

Throughout this document, we will again be using the example table:

```sql
CREATE TABLE GeoCheckin
(
   myfamily    varchar   not null,
   myseries    varchar   not null,
   time        timestamp not null,
   weather     varchar   not null,
   temperature double,
   PRIMARY KEY (
     (myfamily, myseries, quantum(time, 15, 'm')),
     myfamily, myseries, time
   )
)
```


## `CREATE TABLE` Query in Client Library

Using one of the Riak client libraries, execute the `CREATE TABLE` statement via that library's query functionality. This will create and activate the table in one step. The result of the operation is library-dependent:

* [Java][java]: the `QueryResult` object will be returned without any data for rows or columns.
* [Ruby][ruby]: no exception thrown and result collection is empty.
* [Python][python]: no exception thrown. Result object is present with `rows` and `columns` being empty.
* [C#][csharp]: no exception thrown. Result object is present with `Value` and `Columns` being empty.
* [Node.js][nodejs]:  no exception thrown. Result object is present with `rows` and `columns` being empty.
* [Erlang][erlang]: the returned term will consist of two empty lists `{[],[]}`
* [PHP][php]: the response object has a boolean `isSuccess()` instance method


### Using the `WITH` clause

Your Data Definition Language (DDL) may have an optional `WITH` clause, where any table properties can be specified:

```sql
CREATE TABLE (...) WITH (
    n_val=5, key2 = 'string value2',
    prop_with_quotes='single '' quote here',
    custom_prop = 42.24)
```

Please note the following when using the `WITH` clause:

* The property values can be of numeric or string types (parsable as
  `sint64`, `double` or `varchar`, correspondingly). String values
  should be quoted with a `'`; literal single quote characters
  appearing in the string should be doubled (and not escaped with a `\`).
* Values from `WITH` clause will override those specified outside the query statement.


### Verification via Client Library

You can verify that your table was properly created by executing the `DESCRIBE table` query via the query function of your client library, or by using the `riak-admin bucket-type status` command as described in the section below.

The result of the `DESCRIBE table` command is library-dependent:

* [Java][java]: the `QueryResult` object will be returned with rows and columns corresponding to the table's DDL.
* [Ruby][ruby]: no exception thrown and result collection will contain rows and columns corresponding to the table's DDL.
* [Python][python]: no exception thrown. Result object is present with `rows` and `columns` corresponding to the table's DDL.
* [C#][csharp]: no exception thrown. Result object is present with `Value` and `Columns` corresponding to the table's DDL.
* [Node.js][nodejs]:  no exception thrown. Result object is present with `rows` and `columns` corresponding to the table's DDL.
* [Erlang][erlang]: the returned term will consist of two lists corresponding to the table's DDL.
* [PHP][php]: the response object will contain an array of rows, each one representing a column definition for the table's DDL


## `riak-admin`

To create the example table, first run:
(**Note: Mac OS X users can skip this step**)

```bash
sudo su riak
```

This will put you in a shell as the riak user. Then run:

```sh
riak-admin bucket-type create GeoCheckin '{"props":{"table_def": "CREATE TABLE GeoCheckin (myfamily varchar not null, myseries varchar not null, time timestamp not null, weather varchar not null, temperature double, PRIMARY KEY ((myfamily, myseries, quantum(time, 15, 'm')), myfamily, myseries, time))"}}'
```

Please take care with the following:

* The `bucket-type` name must equal the table name.
* The syntax is very sensitive to whitespace and quoting.
* It is easy to create a very long bucket type name with no corresponding
  timeseries table if you leave out the space between the bucket type name
  and the opening quote of the JSON properties.
* The table and field names are currently constrained to ASCII.

Also note that if you discover something wrong with the setup of your DDL, you will need to create it again and decide whether to scrap the data in the existing table or move it from the old table to the new one.


### Activating Your Table

You activate your table as follows:

```sh
riak-admin bucket-type activate »TABLE NAME«
```

For the example `GeoCheckin` table:

```sh
riak-admin bucket-type activate GeoCheckin
```


### Verify Creation and Activation

You can verify that your table was properly created by looking at the `ddl` section of the `riak-admin bucket-type status` response. For example:

```sh
$ riak-admin bucket-type status GeoCheckin
GeoCheckin is active
...
ddl: {ddl_v1,<<"GeoCheckin">>,
             [{riak_field_v1,<<"myfamily">>,1,binary,false},
              {riak_field_v1,<<"myseries">>,2,binary,false},
              {riak_field_v1,<<"time">>,3,timestamp,false},
              {riak_field_v1,<<"weather">>,4,binary,false},
              {riak_field_v1,<<"temperature">>,5,double,true}],
             {key_v1,[{hash_fn_v1,riak_ql_quanta,quantum,
                                  {param_v1,[<<"myfamily">>]},
                      {param_v1,[<<"myseries">>]}]},
                      [{param_v1,[<<"time">>]},15,m],
                                  timestamp},
             {key_v1,[{param_v1,[<<"time">>]},
                      {param_v1,[<<"myfamily">>]},
                      {param_v1,[<<"myseries">>]}]}}
```


## Next Steps

Now that you've created and activated your Riak TS table, you can [write data][writing] to it.
