---
title: Erlang Client API 
project: riakts
version: 1.0.0+
document: reference
toc: true
index: true
audience: advanced
---


You can develop with Riak TS through the Erlang client. This document covers the Erlang protobuf requests to Riak TS.


##Data Types

###table_name()

`table_name() = binary()`

###ts_value()

`ts_value() = number() | binary()`


##Function Index

* `delete/4` - Delete a record by primary key.
* `get/4` - Get a record by primary key.
* `put/3` - Make data records and insert them individually into a Riak TS table.
* `query/2` - Execute a `SELECT ...` query with the client.
* `query/3` - Execute a `SELECT ...` query with client PID using interpolations.


###Function Details

####`delete/4`

```
delete(Pid::pid(), Table::[table_name()](#type-table_name), Key::[[ts_value()](#type-ts_value)], Options::[proplists:proplist()](proplists.html#type-proplist)) -> ok | {error, Reason::term()}
```

Delete a record by primary key. Specify the primary key by using a list of terms that match the primary key values. The order of the terms must match the order of the values in the primary key. `Options` is a proplist which can include values for 'vclock' and 'timeout'. Unless 'vclock' is supplied (see `get/4` below), a GET is called in order to obtain one.


####`get/4`

```
get(Pid::pid(), Table::[table_name()](#type-table_name), Key::[[ts_value()](#type-ts_value)], Options::[proplists:proplist()](proplists.html#type-proplist)) -> {Columns::[binary()], Record::[[ts_value()](#type-ts_value)]}
```

Get a record by primary key. Specify the primary key by using a list of terms that match the primary key values. The order of the terms must match the order of the values in the primary key. Returns a tuple with a list of column names in its 1st element, and a record found as a list of values in its 2nd element. If no record is found, the return value is `{[], []}`. `Options` is a proplist which can include a value for 'timeout'.


####`put/3`

```
put(Pid::pid(), Table::[table_name()](#type-table_name), Data::[[[ts_value()](#type-ts_value)]]) -> ok | {error, Reason::term()}<
```  

Make data records and insert them individually into a Riak TS table using client PID. Each record is a list of values of appropriate types for the complete set of table columns, in the order in which they appear in table's DDL. Successful PUTs will return 'ok', while unsuccessful PUTs return an `{error, Reason}` tuple.

>**Note:** Type validation is done on the first record only. If any subsequent record contains fewer or more elements than there are columns, or some element fails to convert to the appropriate type, the rest of the records will not get inserted.


####`query/2`

```
query(Pid::pid(), Query::string()) -> {ColumnNames::[binary()], Rows::[tuple()]} | {error, Reason::term()}
``` 

Execute a `SELECT ...` query with the client. The result returned is either a tuple containing a list of columns as binaries in the 1st element and a list of records, each represented as a list of values, in the 2nd element, or an `{error, Reason}` tuple.

####`query/3`

```
query(Pid::pid(), Query::string(), Interpolations::[{binary(), binary()}]) -> {ColumnNames::[binary()], Rows::[tuple()]} | {error, term()}
```

Execute a `SELECT ...` query with the client PIS using interpolations. The result returned is either a tuple containing a list of columns as binaries in the 1st element and a list of records, each represented as a list of values, in the 2nd element, or an `{error, Reason}` tuple.