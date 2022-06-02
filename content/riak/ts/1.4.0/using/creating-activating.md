---
title: "Creating and Activating Your Riak TS Table"
description: "Creating and Activating Your Riak TS Table"
menu:
  riak_ts-1.4.0:
    name: "Create and Activate Your Table"
    identifier: "creating_activating_riakts"
    weight: 302
    parent: "using"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/using/creating-activating/
---


[csharp]: ../../developing/csharp#query
[describe]: ../querying/describe/
[erlang]: ../../developing/erlang/#query-2
[java]: ../../developing/java#query
[nodejs]: ../../developing/nodejs/#query
[php]: ../../developing/php#query
[python]: ../../developing/python#query
[ruby]: ../../developing/ruby#sql-queries
[planning]: ../planning/
[writing]: ../writingdata/


Once you have [planned out your table][planning] you can create it by:

* Executing a CREATE TABLE statement using any Riak TS client, 
* Using riak shell, or
* Running the `riak-admin` command (as root, using `su` or `sudo`).

Throughout this document, we will again be using the example table:

```sql
CREATE TABLE GeoCheckin
(
   id           SINT64    NOT NULL,
   region       VARCHAR   NOT NULL,
   state        VARCHAR   NOT NULL,
   time         TIMESTAMP NOT NULL,
   weather      VARCHAR NOT NULL,
   temperature  DOUBLE,
   PRIMARY KEY (
     (id, QUANTUM(time, 15, 'm')),
      id, time
   )
)
```


## `CREATE TABLE` in Client Library

Using one of the Riak TS client libraries, execute the CREATE TABLE statement via that library's query functionality. This will create and activate the table in one step. The result of the operation is library-dependent:

* [Java][java]: the `QueryResult` object will be returned without any data for rows or columns.
* [Ruby][ruby]: no exception thrown and result collection is empty.
* [Python][python]: no exception thrown; result object is present with `rows` and `columns` being empty.
* [C#][csharp]: no exception thrown; result object is present with `Value` and `Columns` being empty.
* [Node.js][nodejs]:  no exception thrown; result object is present with `rows` and `columns` being empty.
* [Erlang][erlang]: the returned term will consist of two empty lists `{[],[]}`.
* [PHP][php]: the response object has a boolean `isSuccess()` instance method.


### Using the WITH clause

Your data definition language (DDL) may have an optional WITH clause, where any table properties can be specified:

```sql
CREATE TABLE (...) WITH (
    n_val=5, key2 = 'string value2',
    prop_with_quotes='single '' quote here',
    custom_prop = 42.24)
```

Please note the following when using `WITH`:

* The property values can be of numeric or string types (parseable as
  `sint64`, `double` or `varchar`, correspondingly). String values
  should be quoted with a `'`; literal single quote characters
  appearing in the string should be doubled (and not escaped with a `\`).
* Values from the WITH clause will override those specified outside the query statement.


### Verification via Client Library

You can verify that your table was properly created by executing the [DESCRIBE statement][describe] via the query function of your client library, or by using the [`riak-admin bucket-type status` command](#verify-creation-and-activation).



## Create a table with riak shell

You can use riak shell to create a table by running:

```
riak-shell>CREATE TABLE GeoCheckin (id SINT64 NOT NULL, region VARCHAR NOT NULL, state VARCHAR NOT NULL, time  TIMESTAMP NOT NULL, weather  VARCHAR NOT NULL, temperature DOUBLE, PRIMARY KEY ((id, QUANTUM(time, 15, 'm')), id, time));
```

Please take care with the following:

* The syntax is sensitive to whitespace and quoting.
* The table and column names are currently constrained to ASCII.

## `riak-admin`

To create the example table, first run:

(**Note: Mac OS X users can skip this step**)

```bash
sudo su riak
```

This will put you in a shell as the riak user. Then run:

```sh
riak-admin bucket-type create GeoCheckin '{"props":{"table_def": "CREATE TABLE GeoCheckin (id SINT64 NOT NULL, region VARCHAR NOT NULL, state VARCHAR NOT NULL, time TIMESTAMP NOT NULL, weather VARCHAR NOT NULL, temperature DOUBLE, PRIMARY KEY ((id, QUANTUM(time, 15, 'm')), id, time))"}}'
```

Please take care with the following:

* The `bucket-type` name must equal the table name.
* The syntax is very sensitive to whitespace and quoting.
* It is easy to create a very long bucket type name with no corresponding
  TS table if you leave out the space between the bucket type name
  and the opening quote of the JSON properties.
* The table and column names are currently constrained to ASCII.

Also note that if you discover something wrong with the setup of your data definition language (DDL), you will need to create it again and decide whether to scrap the data in the existing table or move it from the old table to the new one.


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
             [{riak_field_v1,<<"id">>,1,sint64,false},
              {riak_field_v1,<<"region">>,2,varchar,false},
              {riak_field_v1,<<"state">>,3,varchar,false},
              {riak_field_v1,<<"time">>,4,timestamp,false},
              {riak_field_v1,<<"weather">>,5,varchar,false},
              {riak_field_v1,<<"temperature">>,6,double,true}],
             {key_v1,[{param_v1,[<<"id">>]},
                      {hash_fn_v1,riak_ql_quanta,quantum,
                                  [{param_v1,[<<"time">>]},15,m],
                                  timestamp}]},
             {key_v1,[{param_v1,[<<"id">>]},{param_v1,[<<"time">>]}]}}
```


## Editing Your Table

Once created, you cannot edit your Riak TS table. If you discover something wrong with the setup of your Riak TS table, you will need to create it again. You will also need to decide whether to scrap the data in the existing table or move it from the old table to the new one.


## Next Steps

Now that you've created and activated your Riak TS table, you can [write data][writing] to it.
