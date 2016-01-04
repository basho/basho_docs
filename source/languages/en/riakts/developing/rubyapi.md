---
title: Ruby Client API
project: riakts
version: 1.0.0+
document: reference
toc: true
index: true
audience: advanced
---

You can develop applications and tools using Riak TS with the Riak Ruby client.
This document covers the Ruby API for Riak TS.

## Overview

Riak Ruby Client versions 2.3.0 and newer have new objects in the
`Riak::TimeSeries` module, including the five operations and data types
necessary to make sense of these operations.

## Data Types

* Scalar: not strictly a class itself, just the basic Ruby core/stdlib `String`,
`Fixnum`, `Bignum`, `Float`, and of course, `Time` instances that represent a
single cell in a time series collection.
* `Row`: an `Array` subclass that holds a collection of scalars.
* `Collection`: an `Array` subclass that holds a collection of `Row`s.

### Scalars in Ruby and Riak TS

* Ruby `nil`s round-trip to Riak TS `NULL`s without issue, iff the Riak TS cell
is nullable.
* Ruby `String`s and Riak TS `varchar`s are interchanged without loss
* Ruby `Float`s on platforms with 64-bit `double`s and Riak TS `double`s are
interchanged without loss
* Ruby `Fixnum`s turn into Riak TS `sint64`s and back without loss
* Low-magnitude Ruby `Bignum`s that fit in a `sint64` are interchanged without
loss
* High-magnitude Ruby `Bignum`s raise a
`Riak::TimeSeriesError::SerializeBigIntegerError`
* Ruby `BigDecimal` numbers are converted to a `Float` and serialized as a Riak
TS `double`, which will later de-serialize as a Ruby `Float`
* Ruby `Complex` numbers raise a
`Riak::TimeSeriesError::SerializeComplexNumberError`
* Ruby `Rational` numbers raise a
`Riak::TimeSeriesError::SerializeRationalNumberError`

## Operations

Riak TS supports five basic operations: single-key reads and deletes, key
listing, SQL queries, and writes/submissions.

### Single-key Reads

To load a single row with a given key, use `Riak::TimeSeries::Read`.

```ruby
read_operation = Riak::TimeSeries::Read.new client, 'GeoCheckins'
read_operation.key = ['myfamily', 'myseries', Time.now]
results = read_operation.read!
```

#### Constructor

The `new` class method takes two arguments: `client`, the `Riak::Client` to use,
and `table_name`, a `String` of the name of the table.

#### Instance Accessors

* `key`: read/write, this is how you tell the `Read` object what key to find
* `client`: read-only, the `Riak::Client` this `Read` will use
* `table_name`: read-only, the `String` table name this `Read` will use

#### Instance Method

* `read!`: issues the read operation to Riak, and returns a `Row` of data. If
no data are found, returns `nil`.

### Single-key Deletes

To delete a single row with a given key, use `Riak::TimeSeries::Delete`.

```ruby
delete_operation = Riak::TimeSeries::Deletion.new client, 'GeoCheckins'
delete_operation.key = ['myfamily', 'myseries', Time.now]
delete_operation.delete!
```

#### Constructor

The `new` class method takes two arguments: `client`, the `Riak::Client` to use,
and `table_name`, a `String` of the name of the table.

#### Instance Accessors

* `key`: read/write, this is how you tell the `Deletion` object what key to find
and delete
* `client`: read-only, the `Riak::Client` this `Deletion` will use
* `table_name`: read-only, the `String` table name this `Deletion` will use

#### Instance Method

* `delete!`: issues the deletion to Riak

### Key Listing

To list keys in a table, use the `Riak::TimeSeries::List` class.

**WARNING:** listing keys is a very expensive operation for a Riak TS cluster.

```ruby
list_operation = Riak::TimeSeries::List.new client, 'GeoCheckins'
list_operation.issue! do |key|
  puts key
end

results = list_operation.issue!
```

The `List` is only available by streaming: if the `issue!` method is called
with a block, the block will be `yield`ed each key `Row` in order of receiving.
Without a block, `issue!` returns and stores the listing.

#### Constructor

The `new` class method takes two arguments: `client`, the `Riak::Client` to use,
and `table_name`, a `String` of the name of the table.

#### Instance Accessors

* `client`: read-only, the `Riak::Client` this `Deletion` will use
* `table_name`: read-only, the `String` table name this `Deletion` will use
* `timeout`: how many milliseconds Riak should wait for listing
* `results`: the `Riak::TimeSeries::Collection` of found keys, or `nil` if
the listing wasn't retrieved for storage

#### Instance Method

The `issue!` method that starts the key listing can work in two different ways.

Without a block, the streaming key listing will be buffered up into a
`Riak::TimeSeries::Collection`. The `Collection` will also be available from the
`results` accessor. **WARNING:** on a table with many rows, the `Collection` can
be extremely large, and will cause extra memory load, garbage collections, and
other issues.

With a block, each key from the listing will be `yield`ed into your block as a
`Riak::TimeSeries::Row`, and not otherwise saved.

## SQL Queries

SQL queries are sent with a `Riak::TimeSeries::Query` object.

```ruby
query = Riak::TimeSeries::Query.new client, sql
query.issue!
query.results
```

#### Constructor

The `new` class method takes two arguments: `client`, the `Riak::Client` to use,
and a `String` of `query_text`.

#### Instance Accessors

* `query_text`: read/write SQL query `String`
* `client`: read-only, the `Riak::Client` this `Query` will use
* `results`: read-only, the `Riak::TimeSeries::Collection` of results

#### Instance Method

The `issue!` method issues the query to Riak and populates the `results`
accessor with the results.

## Writing

Data can be submitted to Riak TS with the `Riak::TimeSeries::Submission` class.
Measurements are expected as an `Array`, representing a collection that contains
one or more `Array`s representing rows, that contains scalar values for cells.

```ruby
submission = Riak::TimeSeries::Submission.new client, 'GeoCheckins'
submission.measurements = [
  ['myfamily', 'myseries', Time.now - 5, 1, 2, '3'],
  ['myfamily', 'myseries', Time.now - 4, 4, 5, '6']
]

submission.write!
```

#### Constructor

The `new` class method takes two arguments: `client`, the `Riak::Client` to use,
and a `String` of `query_text`.

#### Instance Accessors

* `client`: read-only, the `Riak::Client` this `Deletion` will use
* `table_name`: read-only, the `String` table name this `Deletion` will use
* `measurements`: read-write, the `Array<Array<scalar>>` of measurements. The
inner `Array`s of scalars are order-sensitive: data will be written in the order
the cells are in the table's DDL.

#### Instance Method

The `write!` method writes the data to Riak TS.
