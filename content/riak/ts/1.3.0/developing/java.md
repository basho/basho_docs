---
title: "Java Client API"
description: "Java Client API"
menu:
  riak_ts-1.3.0:
    name: "Java"
    identifier: "ts_java_api"
    weight: 401
    parent: "develop"
project: "riak_ts"
project_version: "1.3.0"
toc: true
aliases:
    - /riakts/1.3.0/developing/java/
canonical_link: "https://docs.basho.com/riak/ts/latest/developing/java"
---


You can develop applications and tools using Riak TS with the Riak Java client.
This document covers the Java API for Riak TS.


## Overview

There are two packages that cover the public API for TS in the Java client:

1. The `com.basho.riak.client.api.commands.timeseries` package contains executable commands, and
2. the `com.basho.riak.client.core.query.timeseries` package contains the common data types.

Language | Source | Documentation | Download
:--------|:-------|:--------------|:--------
Java | [riak-java-client](https://github.com/basho/riak-java-client) | [javadoc](http://basho.github.com/riak-java-client), [wiki](https://github.com/basho/riak-java-client/wiki) | [Maven Central](http://search.maven.org/?#search%7Cgav%7C1%7Cg%3A%22com.basho.riak%22%20AND%20a%3A%22riak-client%22) |


## Data Types

 * `Cell` - Holds a single piece of data.
 * `Row` - Holds a collection of Cells.
 * `ColumnDescription` - A metadata description of a column definition in a Riak TS table.
 * `QueryResult` - Holds a result set from a query, key list, or fetch command.


### Data Type Details

#### `Cell`

A cell contains a piece of data for a row in a Riak TS table.

>**Note:** Cells are immutable once created.

A cell can hold 5 different types of raw data:

* `Varchar` - byte arrays commonly used to store encoded strings.
* `SInt64` - any signed 64-bit integers.
* `Double` - any 64-bit floating point numbers.
* `Timestamp` - any Unix epoch timestamp; millisecond resolution is required.
* `Boolean` - a true/false value.

##### Constructors

Cell constructors accept: strings (`Varchar`), longs (`SInt64`), double (`Double`), boolean (`Boolean`), calendar (`Timestamp`), and date (`Timestamp`).

 * `public Cell(String varcharValue)`
 * `public Cell(long sint64Value)`
 * `public Cell(double doubleValue)`
 * `public Cell(boolean booleanValue)`
 * `public Cell(Calendar timestampValue)`
 * `public Cell(Date timestampValue)`

There is also a special static helper for creating cells with raw timestamps.

 * `public static Cell newTimestamp(long value)`

##### Instance Methods

Each data type has the following methods: `has_X` and `get_X`.


#### `Row`

A row contains a collection of cells.

>**Note:** Rows are immutable once created.

##### Constructors

 * `public Row(Collection<Cell> cells)`
 * `public Row(Cell... cells)`

##### Instance Methods

 * `int getCellsCount()` - Gets the total count of all cells in this row.
 * `List<Cell> getCellsCopy()` - Returns a shallow copy of the immutable cell collection.
 * `Iterator<Cell> iterator()` - Returns an iterator to the immutable cell collection.


#### `ColumnDescription`

The column description is a metadata description of a column definition in a Riak TS table, and contains both a column name and type.

##### Constructors

 * `ColumnDescription(String name, ColumnType type)`

##### Instance Methods

 * `string getName()` - Returns the name of the column.
 * `ColumnType getType()` - Returns the type of the column.

##### Subclasses

 * `ColumnType` - Holds an enumeration of all the column types.

```
public enum ColumnType
    {
        VARCHAR,
        SINT64,
        DOUBLE,
        TIMESTAMP,
        BOOLEAN
    }
```


#### `QueryResult`

The query result is the result set from a query, key list, or fetch command.

>**Note:** Query results are immutable.

##### Constructors

There are no constructors for `QueryResult`.

##### Instance Methods

 * `List<ColumnDescription> getColumnDescriptionsCopy()` - Returns a deep copy of the query result's column descriptions (if any).
 * `int getRowCount()` - Gets the total count of all rows in this result.
 * `List<Row> getRowsCopy()` - Returns a shallow copy of the immutable row collection.
 * `Iterator<Row> iterator()` - Returns an iterator to the immutable row collection.


## Command Classes Index

All command classes have a static inner `Builder` class to create and build each command.

* `Delete` - Deletes a single row by it's key values.
* `Fetch` - Fetches a single row by it's key values.
* `Query` - Allows you to query a Riak TS table with the given query string.
* `Store` - Stores data in the Riak TS table.
* `ListKeys` - Lists the primary keys of all the rows in a Riak TS table.

>**Warning:** `ListKeys` is a very expensive operation.


### Command Class Details

Each command is created through a static `Builder` subclass. This pattern ensures the commands are created as correctly as possible. To create the command from the builder, call the `.build()` method.

To execute any command, you must have an instance of a `RiakClient` object. You then pass the command object as a parameter into the `execute()` or `executeAsync()` methods.

```java
RiakClient client = RiakClient.newClient(10017, "myriakdb.host");

String queryText = "select weather, temperature from GeoCheckin " +
                   "where time > 1234560 and time < 1234569 and " +
                   "region = 'South Atlantic' and state = 'South Carolina'";

Query query = new Query.Builder(queryText).build();

// With the synchronous execute, any errors encountered will be thrown.
QueryResult queryResult = client.execute(query);

// With the executeAsync method, any errors will be stored for review.
final RiakFuture<QueryResult, String> queryFuture = client.executeAsync(storeCmd);
bool success = queryFuture.isSuccess();
QueryResult result = queryFuture.get();
Throwable error = queryFuture.cause();
```


#### `Delete`

Deletes a single row by it's key values.

##### Builder

The builder for `Delete` takes the table name and a list of cells that identify the primary key. The order of the cells must match the order of the values in the primary key.

 * `public Builder(String tableName, List<Cell> keyValues)`

There is also an instance method to specify a command timeout in milliseconds:

 * `public Builder withTimeout(int timeout)`

##### Return Value

 * `void`


#### `Fetch`

Fetches a single row by it's key values.

##### Builder

The builder for `Fetch` takes the table name and a list of cells that identify the primary key. The order of the cells must match the order of the values in the primary key.

* `public Builder(String tableName, List<Cell> keyValues)`

There is also an instance method to specify a command timeout in milliseconds:

 * `public Builder withTimeout(int timeout)`

##### Return Value

* `QueryResult` - 1 row if a match was found; 0 rows if no match was found.


#### `ListKeys`

Lists the primary keys of all the rows in a Riak TS table.

##### Builder

The builder only takes the table name to list keys from:

 * `public Builder(String tableName)`

There is also an instance method to specify a command timeout in milliseconds:

 * `public Builder withTimeout(int timeout)`

##### Return Value

* `QueryResult` - each primary key's cells as a row. May not contain values for column descriptions.


#### `Query`

Allows you to query a Riak TS table with the given query string.

##### Builder

The builder only takes the query text:

 * `public Builder(String queryText)`

##### Return Value

 * `QueryResult` - contains all matching rows.


#### `Store`

Stores data in the Riak TS table.

##### Builder

The builder constructor takes the table name:

 * `public Builder(String tableName)`

 To add rows to store, use one of the `withRow()` or `withRows()` methods:

  * `public Builder withRow(Row row)`
  * `public Builder withRows(Collection<Row> rows)`

##### Return Value

* `void`