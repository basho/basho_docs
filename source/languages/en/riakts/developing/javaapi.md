---
title: Java Client API 
project: riakts
version: 1.0.0+
document: reference
toc: true
index: true
audience: advanced
---


You can develop with Riak TS through the Java client. This document covers the Java protobuf requests to Riak TS.

## Overview

There are two packages that cover the public API for TS in the Java client.  The `com.basho.riak.client.api.commands.timeseries` package contains executable commands, and the `com.basho.riak.client.core.query.timeseries` package contains the common data types.

## Data Types

 * Cell - Holds a single piece of data. 
 * Row - Holds a collection of Cells.
 * ColumnDescription - A Metadata description of a column in a Time Series table.
 * QueryResult - Holds a result set from a query, keylist, or fetch command.

### Data Types Details

####`Cell`
Holds a piece of data for a Time Series Row.
Note: Cells are immutable once created.

A cell can hold 5 different types of raw data:

**Varchar**s, which can hold byte arrays. Commonly used to store encoded strings.
**SInt64**s, which can hold any signed 64-bit integers.
**Double**s, which can hold any 64-bit floating point numbers.
**Timestamp**s, which can hold any unix/epoch timestamp. Millisecond resolution is required.
**Boolean**s, which can hold a true/false value. 

##### Constructors
There are Cell constructors that accept Strings(Varchar type), Longs(SInt64 type), Double (Double type), Boolean (Boolean type), Calendar (Timestamp type), and Date(Timestamp type).  There is also a special static helper for creating Cells with raw timestamps: `public static Cell newTimestamp(long value)`.

##### Instance Methods
There are `has_X` and `get_X` methods for each data type. 


####`Row`
Holds a collection of cells. 
Note: Rows are immutable once created.

##### Constructors

There are two Row constructors, one takes a known Collection of cells (`public Row(Collection<Cell> cells)`), the other takes a varargs of cells (`public Row(Cell... cells)`).

##### Public Instance Methods

 * `int getCellsCount()` - Gets the total count of all cells in this row. 
 * `List<Cell> getCellsCopy()` - Returns a shallow copy of the immutable cell collection.
 * `Iterator<Cell> iterator()` - Returns an iterator to the immutable cell collection.


####`ColumnDescription`
A Metadata description of a column in Riak Time Series.
Contains a column name and column type.

##### Constructors

There is one constructor, that takes a name string, and column type enum value.  (`ColumnDescription(String name, ColumnType type)`)

##### Public Instance Methods

 * `string getName()` - Returns the name of the column.
 * `ColumnType getType()` - Returns the type of the column.

##### Public Subclasses
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

####`QueryResult`
Holds a result set from a query, keylist, or fetch.
Note: QueryResults are immutable.

##### Constructors

There are no public constructors for QueryResults.

##### Public Instance Methods

 * `List<ColumnDescription> getColumnDescriptionsCopy()` - Returns a deep copy of the Query Result's Column Descriptions (if any).
 * `int getRowCount()` - Gets the total count of all rows in this result. 
 * `List<Row> getRowsCopy()` - Returns a shallow copy of the immutable row collection.
 * `Iterator<Row> iterator()` - Returns an iterator to the immutable row collection.



##Command Classes Index

All command classes have a static inner `Builder` to create and build each command. 

* Delete - Deletes a single time series row by it's key values.
* Fetch - Fetches a single time series row by it's key values.
* ListKeys - Lists the primary keys of all the rows in a Time Series table.
* Query - Allows you to query a Time Series table, with the given query string.
* Store - Stores data into the Time Series table.


###Command Classes Details



####`Delete`


####`Fetch` 

####`ListKeys`

####`Query`

####`Store`
