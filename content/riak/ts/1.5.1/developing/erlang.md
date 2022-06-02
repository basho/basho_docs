---
title: "Erlang Client API"
description: "Erlang Client API"
menu:
  riak_ts-1.5.1:
    name: "Erlang"
    identifier: "ts_erlang_api"
    weight: 401
    parent: "develop"
project: "riak_ts"
project_version: "1.5.1"
toc: true
aliases:
    - /riakts/1.5.1/developing/erlang/
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

To insert a `null` cell value, use an empty list (`[]`) when building the Data list.

>**Note:** Type validation is done on the first row only. If any subsequent row contains fewer or more elements than there are columns, or some element fails to convert to the appropriate type, the rest of the rows will not get inserted.


#### `query/2`

```
query(Pid::pid(),
      Query::string()) ->
        {ok, {ColumnNames::[binary()], Rows::[tuple()]}} | {error, Reason::term()}
```

See `query/4`.

#### `query/4`

```
query(Pid::pid(),
      Query::string()|binary(),
      Interpolations::[{binary(), binary()}],
      Cover::term()) ->
        {ok, {ColumnNames::[binary()], Rows::[tuple()]}} | {error, term()}.
```

Execute a `SELECT ...` query with the client. The result returned is either an `ok` tuple with a nested tuple containing a list of columns as binaries in the first element and a list of records, each represented as a list of values, in the second element, or an `{error, Reason}` tuple.

`Interpolations` is currently ignored; eventually it will support
parameter binding. Send an empty list if you use `query/4`.

`Cover` is `undefined` by default. Otherwise, it is an opaque binary
extracted from the return value to `get_coverage/3` (or
`replace_coverage/4`/`replace_coverage/5`).

#### `stream_list_keys/3`

```
stream_list_keys(pid(), table_name(), proplists:proplist()) ->
    {ok, req_id()} | {error, term()}.
```

Streaming list keys in a Riak TS table using client Pid. The parameter option is a proplist that can include a value for 'timeout'. A successful request returns `{ok, ReqId}`, while an unsuccessful request returns `{error, Reason}`.

>**Warning:** Listing keys is a very expensive operation for a Riak TS cluster.

#### `get_coverage/3`

```
get_coverage(pid(), table_name(), QueryText::iolist()) ->
   {ok, Entries::[term()]} | {error, term()}.
```

Request a list of coverage plan components. Each element in the
returned list will include an IP address and port number to connect
to, a human-readable description of the item, and an opaque binary.

Each element correponds to one partition and quantum range in the
database, so an invocation of `query/4` with the opaque binary will
return only those values which fall within that single quantum.

This allows for queries to be executed in parallel, and allows allows
for the quantum limit to be effectively bypassed.

#### `replace_coverage/4`

```
replace_coverage(Pid::pid(),
                 Table::table_name(),
                 QueryText::iolist(),
                 Cover::binary()) ->
  {ok, Entries::[term()]} | {error, term()}.
```

See `replace_coverage/5`.

#### `replace_coverage/5`

```
replace_coverage(Pid::pid(),
                 Table::table_name(),
                 QueryText::iolist(),
                 Cover::binary(),
                 OtherCover::[binary()]) ->
  {ok, Entries::[term()]} | {error, term()}.
```

Request a replacement coverage plan component by providing an opaque
coverage binary which the client has been unable to use because the
server is not responding (or is responding with an error).

The `OtherCover` argument, a list of additional binaries which have
proven problematic, is useful to Riak to avoid providing a client with
a connection to a server to which it currently cannot connect.
