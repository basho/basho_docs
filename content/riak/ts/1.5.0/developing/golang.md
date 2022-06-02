---
title: "Go Client API"
description: "Go Client API"
menu:
  riak_ts-1.5.0:
    name: "Go"
    identifier: "ts_golang_api"
    weight: 401
    parent: "develop"
project: "riak_ts"
project_version: "1.5.0"
toc: true
aliases:
    - /riakts/1.5.0/developing/golang/
---


You can develop applications and tools using Riak TS with the Riak Go client.
This document covers the Go API for Riak TS.

You will need to use [version 1.7.0 or later](https://github.com/basho/riak-go-client/releases/latest)
of the Go client to use the TS API.

## Overview

TS support within the Go client is implemented through the following 5 command builders, each with a factory:

Command    | Builder                                                                                                  |
:----------|:---------------------------------------------------------------------------------------------------------|
Store      | [TsStoreRowsCommandBuilder](https://godoc.org/github.com/basho/riak-go-client#TsStoreRowsCommandBuilder) |
Fetch Row  | [TsFetchRowCommandBuilder](https://godoc.org/github.com/basho/riak-go-client#TsFetchRowCommandBuilder)   |
Delete Row | [TsDeleteRowCommandBuilder](https://godoc.org/github.com/basho/riak-go-client#TsDeleteRowCommandBuilder) |
Query      | [TsQueryCommandBuilder](https://godoc.org/github.com/basho/riak-go-client#TsQueryCommandBuilder)         |
List Keys  | [TsListKeysCommandBuilder](https://godoc.org/github.com/basho/riak-go-client#TsListKeysCommandBuilder)   |

Language | Source | Documentation
:--------|:-------|:-------------
Go | [riak-go-client](https://github.com/basho/riak-go-client) | [GoDoc](https://godoc.org/github.com/basho/riak-go-client)


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

A cell is constructed by using one of the cell factory methods for the various data types.

 * `cell := NewStringTsCell("South Atlantic")`
 * `cell := NewSint64TsCell(42816067)`
 * `cell := NewDoubleTsCell(49.0)`
 * `cell := NewBooleanTsCell(true)`
 * `cell := NewTimestampTsCell(time.Now())` - from a time.Time object, will be converted to UTC for you
 * `cell := NewTimestampTsCellFromInt64(1443806900000)` - from a unix timestamp
 * `cell := nil` - represents a `null` value

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
