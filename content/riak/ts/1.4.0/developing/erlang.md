---
title: "Erlang Client API"
description: "Erlang Client API"
menu:
  riak_ts-1.4.0:
    name: "Erlang"
    identifier: "ts_erlang_api"
    weight: 401
    parent: "develop"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/developing/erlang/
canonical_link: "https://docs.basho.com/riak/ts/latest/developing/erlang"
---


You can develop applications and tools using Riak TS with the Riak Erlang client.
This document covers the Erlang API for Riak TS.

Language | Source | Documentation | Download
:--------|:-------|:--------------|:--------
Erlang | [riak-erlang-client (riakc)](https://github.com/basho/riak-erlang-client) | [edoc](http://basho.github.com/riak-erlang-client/) | [GitHub](https://github.com/basho/riak-erlang-client)


## Data Types

### table_name()

`table_name() = binary()`


### ts_value()

`ts_value() = number() | binary()`


## Function Index

* `delete/4` - Delete a row by primary key.
* `get/4` - Get a row by primary key.
* `put/3` - Make data rows and insert them individually into a Riak TS table.
* `query/2` - Execute a `SELECT ...` query with the client.
* `stream_list_keys/3` - List all keys.

>**Note:** `stream_list_keys/3` is an extremely expensive function and will list every key.


### Function Details

#### `delete/4`

```
delete(Pid::pid(),
       Table::[table_name()](#type-table_name),
       Key::[[ts_value()](#type-ts_value)],
       Options::[proplists:proplist()](proplists.html#type-proplist)) ->
            ok | {error, Reason::term()}
```

Delete a row by primary key. Specify the primary key by using a list of terms that match the primary key values. The order of the terms must match the order of the values in the primary key. `Options` is a proplist which can include values for 'vclock' and 'timeout'. Unless 'vclock' is supplied (see `get/4` below), a GET is called in order to obtain one.


#### `get/4`

```
get(Pid::pid(),
    Table::[table_name()](#type-table_name),
    Key::[[ts_value()](#type-ts_value)],
    Options::[proplists:proplist()](proplists.html#type-proplist)) ->
        {Columns::[binary()], Record::[[ts_value()](#type-ts_value)]}
```

Get a row by primary key. Specify the primary key by using a list of terms that match the primary key values. The order of the terms must match the order of the values in the primary key. Returns a tuple with a list of column names in its first element, and a record found as a list of values in its second element. If no record is found, the return value is `{[], []}`. `Options` is a proplist which can include a value for 'timeout'.


#### `put/3`

```
put(Pid::pid(),
    Table::[table_name()](#type-table_name),
    Data::[{[ts_value()](#type-ts_value)}]) ->
        ok | {error, Reason::term()}
```

Insert rows into a Riak TS table using client PID. Rows are represented as a list of tuples, where each row is a tuple of values of appropriate types for the complete set of table columns in the order in which they appear in table's DDL. Successful PUTs will return 'ok', while unsuccessful PUTs return an `{error, Reason}` tuple.

>**Note:** Type validation is done on the first row only. If any subsequent row contains fewer or more elements than there are columns, or some element fails to convert to the appropriate type, the rest of the rows will not get inserted.


#### `query/2`

```
query(Pid::pid(),
      Query::string()) ->
        {ColumnNames::[binary()], Rows::[tuple()]} | {error, Reason::term()}
```

Execute a `SELECT ...` query with the client. The result returned is either a tuple containing a list of columns as binaries in the first element and a list of records, each represented as a list of values, in the second element, or an `{error, Reason}` tuple.


#### `stream_list_keys/3`

```
stream_list_keys(pid(), table_name(), proplists:proplist()) ->
    {ok, req_id()} | {error, term()}.
```

Streaming list keys in a Riak TS table using client Pid. The parameter option is a proplist that can include a value for 'timeout'. A successful request returns `{ok, ReqId}`, while an unsuccessful request returns `{error, Reason}`.

>**Warning:** Listing keys is a very expensive operation for a Riak TS cluster.
