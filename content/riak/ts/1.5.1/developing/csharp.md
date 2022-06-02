---
title: "C# Client API"
description: "C# Client API"
menu:
  riak_ts-1.5.1:
    name: "C#"
    identifier: "ts_csharp_api"
    weight: 401
    parent: "develop"
project: "riak_ts"
project_version: "1.5.1"
toc: true
aliases:
    - /riakts/1.5.1/developing/csharp/
---


You can develop applications and tools using Riak TS with the Riak .NET client.
This document covers the .NET API for Riak TS.


## Overview

The `RiakClient.Commands.TS` namespace covers the public API for Riak TS in the .NET client.

Language | Source | Documentation | Download
:--------|:-------|:--------------|:--------
C# | [riak-dotnet-client](https://github.com/basho/riak-dotnet-client) | [api docs](http://basho.github.io/riak-dotnet-client-api/), [wiki](https://github.com/basho/riak-dotnet-client/wiki) | [NuGet package](http://www.nuget.org/List/Packages/RiakClient), [GitHub Releases](https://github.com/basho/riak-dotnet-client/releases)


## Data Types

 * `Cell` - Holds a single piece of data.
 * `Row` - Holds a collection of Cells.
 * `Column` - A metadata description of a column definition in a Riak TS table.


### Data Type Details

#### `Cell`

A cell contains a piece of data for a row in a Riak TS table.

>**Note:** Cells are immutable once created.

Use the `Cell` implementation that takes a generic type to define the data type. The following .NET types can be saved into Riak TS:

* `string`
* `byte[]` - will be interpreted as UTF-8 encoded string data.
* All integer types. Will be returned as `long` values.
* `DateTime` - stored as UTC timestamp with millisecond resolution. Will be returned as UTC `DateTime` value.
* `bool` - a true/false value.


##### Constructors

Cell constructors accept a value matching the generic type of the class:

 * `var c = new Cell<string>("string value")`
 * `var c = new Cell<byte[]>(bytesOfUtf8Data)`
 * `var c = new Cell<uint32>(123456)` - other integer types are allowed
 * `var c = new Cell<float>(12.34F)`
 * `var c = new Cell<double>(56.78)`
 * `var c = new Cell<bool>(false)`
 * `var c = new Cell<DateTime>(DateTime.Now)` - will be converted to UTC for you
 * `var c = new Cell()` - represents a `null` value
 * `var c = Cell.Null` - also represents a `null` value


#### `Row`

A row contains a collection of cells.

>**Note:** Rows are immutable once created.


#### `Column`

The column is a metadata description of a column definition in a Riak TS table, and contains both a column name and type.


##### Constructor

`Column(string name, ColumnType type)`

```
public enum ColumnType
{
    Varchar,
    Int64,
    Double,
    Timestamp,
    Boolean
}
```

## Command Classes Index

All command classes have a static inner `Builder` class to create and build each command.

* `Delete` - Deletes a single row by it's key values.
* `Get` - Gets a single row by it's key values.
* `Query` - Allows you to query a Riak TS table with the given query string.
* `Store` - Stores data in the Riak TS table.
* `ListKeys` - Lists the primary keys of all the rows in a Riak TS table.

>**Warning:** `ListKeys` is a very expensive operation.


### Command Class Details

Each command is created through a static `Builder` subclass. This pattern ensures the commands are created as correctly as possible. To create the command from the builder, call the `.Build()` method.

To execute any command, you must have an instance of a `RiakClient` object. You then pass the command object as a parameter into the `Execute()` or `ExecuteAsync()` methods.


#### `Delete`

Deletes a single row by its key values.


##### Builder

The builder for `Delete` takes the table name and a `Row` that identify the primary key. The order of the cells within the `Row` instance must match the order of the values in the primary key.

 * `WithTable(string table)`
 * `WithKey(Row key)`

There is also an instance method to specify a command timeout in milliseconds:

 * `WithTimeout(int timeout)`


##### Return Value

 * `Response`


#### `Get`

Gets a single row by its key values.


##### Builder

The builder for `Get` takes the table name and a `Row` that identify the primary key. The order of the cells within the `Row` instance must match the order of the values in the primary key.

 * `WithTable(string table)`
 * `WithKey(Row key)`

There is also an instance method to specify a command timeout in milliseconds:

 * `WithTimeout(int timeout)`


##### Return Value

* `GetResponse` - 1 row if a match was found; 0 rows if no match was found.


#### `ListKeys`

Lists the primary keys of all the rows in a Riak TS table via streaming.


##### Builder

The builder takes the table name to list keys from:

 * `WithTable(string table)`

You may also specify a callback that will be called every time data is available from the streaming operation. If no callback is specified rows will be buffered completely in memory until the operation completes:

 * `WithCallback(Action<ListKeysResponse> callback)`

There is also an instance method to specify a command timeout in milliseconds:

 * `WithTimeout(int timeout)`


##### Return Value

* `ListKeysResponse` - will contain the complete set of rows if no callback specified, otherwise an empty set of rows since they will have all been delivered via the callback.


#### `Query`

Allows you to query a Riak TS table with the given query string.

```csharp
var qfmt = "SELECT * FROM GeoCheckin WHERE time > {0} and time < {1} and region = 'Pacific' and state = 'Washington'";
var q = string.Format(
    qfmt,
    DateTimeUtil.ToUnixTimeMillis(TenMinsAgo),
    DateTimeUtil.ToUnixTimeMillis(Now));

var cmd = new Query.Builder()
    .WithTable("GeoCheckin")
    .WithQuery(q)
    .Build();

RiakResult rslt = client.Execute(cmd);
```
