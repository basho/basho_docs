---
title: "Go Client API"
description: "Go Client API"
menu:
  riak_ts-1.3.1:
    name: "Go"
    identifier: "ts_golang_api"
    weight: 401
    parent: "develop"
project: "riak_ts"
project_version: "1.3.1"
toc: true
aliases:
    - /riakts/1.3.1/developing/golang/
canonical_link: "https://docs.basho.com/riak/ts/latest/developing/golang"
---


You can develop applications and tools using Riak TS with the Riak Go client.
This document covers the Go API for Riak TS.


## Overview

TS support within the Go client is implemented through the following 4 command builders, each with a factory:

* TsStoreRowsCommandBuilder which is assembled via NewTsStoreRowsCommandBuilder
* TsFetchRowCommandBuilder which is assembled via NewTsFetchRowCommandBuilder
* TsDeleteRowCommandBuilder which is assembled via NewTsDeleteRowCommandBuilder
* TsQueryCommandBuilder which is assembled via NewTsQueryCommandBuilder
* TsListKeysCommandBuilder which is assembled via NewTsListKeysCommandBuilder

## Data Types

* `TsColumnDescription` - Holds the data that describes a column
* `TsCell` - Holds the data and data type
* `Row` - A slice of cells

### Data Type Details

#### `TsColumnDescription`

A TsColumnDescription contains the column name and column data type. They are generated when receiving responses from the `TsFetchRowCommand` and `TsQueryCommand` operations.

#### `TsCell`

A cell contains a piece of data for a row in a Riak TS table and can hold 5 different types of raw data:

* `Varchar` - standard Go string
* `SInt64` - any signed 64-bit integers
* `Double` - any 64-bit floating point number
* `Timestamp` - any Unix epoch timestamp as int64
* `Boolean` - a true/false value.

##### Constructors

A cell is constructed by using one of the cell factory methods for the various data types, e.g. `cell := NewStringTsCell("South Atlantic")`.

##### Instance Methods

Cells have `struct` getters to retrieve the cell value and the value's data type.

#### `Row`

A row contains a slice of cells, e.g. `row := make([]TsCell, 5)`

### Responses

#### TsStoreRowsCommand, TsDeleteRowCommand

Have a boolean response value to signal if the operation was successful.

#### TsFetchRowCommand

Returns a `TsFetchRowResponse` containing `IsNotFound` a flag for if the row was found, a `[]TsColumnDescription` describing the table's columns, and a `[]TsCell` providing the row data.

#### TsQueryCommand

Returns a `TsQueryResponse` containing a `[]TsColumnDescription` describing the table's columns and a `[][]TsCell` providing the data for each row.

#### TsListKeysCommand

Returns a `TsListKeysResponse` containing a `[][]TsCell` with each outer slice element representing a Primary Key for a row.
