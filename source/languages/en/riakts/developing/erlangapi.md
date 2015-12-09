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

* `delete/4` - Delete a record, if there is one, having the fields constituting the primary key in the Table equal to the composite Key (given as a list), using client Pid.
* `get/4` - Get a record, if there is one, having the fields constituting the primary key in the Table equal to the composite Key (supplied as a list), using client Pid.
* `put/3` - Make data records from Data and insert them, individually, into a time-series Table, using client Pid.
* `put/4` - Make data records from Data and insert them, individually, into a time-series Table, using client Pid.
* `query/2` - Execute a "SELECT ..." Query with client.
* `query/3` - Execute a "SELECT ..." Query with client Pid, using Interpolations.


###Function Details

####`delete/4`

```
delete(Pid::pid(), Table::[table_name()](#type-table_name), Key::[[ts_value()](#type-ts_value)], Options::[proplists:proplist()](proplists.html#type-proplist)) -> ok | {error, Reason::term()}
```

Delete a record, if there is one, having the fields constituting the primary key in the Table equal to the composite Key (given as a list), using client Pid. Options is a proplist which can include values for 'vclock' and 'timeout'. Unless vclock is supplied, a get (@see get/4) is called in order to obtain one.

####`get/4`

```
get(Pid::pid(), Table::[table_name()](#type-table_name), Key::[[ts_value()](#type-ts_value)], Options::[proplists:proplist()](proplists.html#type-proplist)) -> {Columns::[binary()], Record::[[ts_value()](#type-ts_value)]}
```

Get a record, if there is one, having the fields constituting the primary key in the Table equal to the composite Key (supplied as a list), using client Pid. Options is a proplist which can include a value for 'timeout'. Returns a tuple with a list of column names in its 1st element, and a record found as a list of values, further as a single element in enclosing list, in its 2nd element. If no record is found, the return value is {[], []}.

####`put-3`

```
put(Pid::pid(), Table::[table_name()](#type-table_name), Data::[[[ts_value()](#type-ts_value)]]) -> ok | {error, Reason::term()}<
```  

Make data records from Data and insert them, individually, into a time-series Table, using client Pid. Each record is a list of values of appropriate types for the complete set of table columns, in the order in which they appear in table's DDL. On success, 'ok' is returned, else an {error, Reason} tuple.

Note: Type validation is done on the first record only. If any subsequent record contains fewer or more elements than there are columns, or some element fails to convert to the appropriate type, the rest of the records will not get inserted.

####`put/4`

```
put(Pid::pid(), Table::[table_name()](#type-table_name), Columns::[binary()], Data::[[[ts_value()](#type-ts_value)]]) -> ok | {error, Reason::term()}
```

Make data records from Data and insert them, individually, into a time-series Table, using client Pid. Each record is a list of values of appropriate types for the complete set of table columns, in the order in which they appear in table's DDL. On success, 'ok' is returned, else an {error, Reason} tuple. Also @see put/3.

As of 2015-11-05, Columns parameter is ignored, the function expexts the full set of fields in each element of Data.

####`query/2`

```
query(Pid::pid(), Query::string()) -> {ColumnNames::[binary()], Rows::[tuple()]} | {error, Reason::term()}
``` 

Execute a "SELECT ..." Query with client. The result returned is a tuple containing a list of columns as binaries in the first element, and a list of records, each represented as a list of values, in the second element, or an {error, Reason} tuple.

####`query/3`

```
query(Pid::pid(), Query::string(), Interpolations::[{binary(), binary()}]) -> {ColumnNames::[binary()], Rows::[tuple()]} | {error, term()}
```

Execute a "SELECT ..." Query with client Pid, using Interpolations. The result returned is a tuple containing a list of columns as binaries in the first element, and a list of records, each represented as a list of values, in the second element, or an {error, Reason} tuple.