---
title: "Creating and Activating Your Riak TS Table"
description: "Creating and Activating Your Riak TS Table"
menu:
  riak_ts-1.2.0:
    name: "Create and Activate Your Table"
    identifier: "creating_activating_riakts"
    weight: 302
    parent: "using"
project: "riak_ts"
project_version: "1.2.0"
toc: true
aliases:
    - /riakts/1.2.0/using/creating-activating/
---

[erlang]: ../../developing/erlang#query2
[java]: ../../developing/java#query
[nodejs]: ../../developing/nodejs#query
[python]: ../../developing/python#query
[ruby]: ../../developing/ruby#sql-queries
[planning]: ../planning/
[writing]: ../writingdata/

Once you have [planned out your table][planning] you can create it using `riak-admin`.

>**Note:** You will need to have access to `sudo` and `su` with the below commands, unless you are operating on Mac OS X.

## Creating Your Table

Remember the example table?

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

To create the example table, first run:
(**Note: Mac OS X users can skip this step**)

```bash
sudo su riak
```

This will put you in a shell as the riak user. Then run:

```sh
riak-admin bucket-type create GeoCheckin '{"props":{"table_def": "CREATE TABLE GeoCheckin (myfamily varchar not null, myseries varchar not null, time timestamp not null, weather varchar not null, temperature double, PRIMARY KEY ((myfamily, myseries, quantum(time, 15, 'm')), myfamily, myseries, time))"}}'
```

Please take care:

* The `bucket-type` name **must** equal the table name.
* The syntax is very sensitive to whitespace and quoting.
* It is easy to create a very long bucket type name with no corresponding
  timeseries table if you leave out the space between the bucket type name
  and the opening quote of the JSON properties.
* The table and field names are currently constrained to ASCII.

Also note that if you discover something wrong with the setup of your DDL, you will need to create it again and decide whether to scrap the data in the existing table or move it from the old table to the new one.

## Activating Your Table

You activate your table just like you would activate a bucket type:

```sh
riak-admin bucket-type activate »TABLE NAME«
```

For the example `GeoCheckin` table:

```sh
riak-admin bucket-type activate GeoCheckin
```

## Verify Creation and Activation

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

## Creation via Client Library

Using one of the Riak Client libraries, execute the `CREATE TABLE` statement via that library's query functionality. This will create and activate the table in one step. The result of the operation is library-dependent:

* [Java][java]: the `QueryResult` object will be returned without any data for rows or columns.
* [Erlang][erlang]: the returned term will consist of two empty lists `{[],[]}`
* [Ruby][ruby]: no exception thrown and result collection is empty.
* [Python][python]: no exception thrown. Result object is present with `rows` and `columns` being empty.
* [Node.js][nodejs]:  no exception thrown. Result object is present with `rows` and `columns` being empty.

## Verification via Client Library

You can verify that your table was properly created by executing the `DESCRIBE table` query via the query function of your client library, or by using the `riak-admin bucket-type status` command as described above.

The result of the `DESCRIBE table` command is library-dependent:

* [Java][java]: the `QueryResult` object will be returned with rows and columns corresponding to the table's DDL.
* [Erlang][erlang]: the returned term will consist of two lists corresponding to the table's DDL.
* [Ruby][ruby]: no exception thrown and result collection will contain rows and columns corresponding to the table's DDL.
* [Python][python]: no exception thrown. Result object is present with `rows` and `columns` corresponding to the table's DDL.
* [Node.js][nodejs]:  no exception thrown. Result object is present with `rows` and `columns` corresponding to the table's DDL.

## Next Steps

Now that you've created and activated your Riak TS table, you can [write data][writing] to it.
