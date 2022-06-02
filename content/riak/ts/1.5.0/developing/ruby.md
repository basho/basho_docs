---
title: "Ruby Client API"
description: "Ruby Client API"
menu:
  riak_ts-1.5.0:
    name: "Ruby"
    identifier: "ts_ruby_api"
    weight: 401
    parent: "develop"
project: "riak_ts"
project_version: "1.5.0"
toc: true
aliases:
    - /riakts/1.5.0/developing/ruby/
---

You can develop applications and tools using Riak TS with the Riak Ruby client.
This document covers the Ruby API for Riak TS.

## Overview

Riak Ruby client versions 2.3.0+ have new objects in the
`Riak::TimeSeries` module, including the necessary operations and data types
to make sense of these operations.

Language | Source | Documentation | Download
:--------|:-------|:--------------|:--------
Ruby | [riak-ruby-client](https://github.com/basho/riak-ruby-client) | [GitHub Pages](http://basho.github.io/riak-ruby-client/) | [RubyGems](https://rubygems.org/gems/riak-client)


## Data Types

* `Scalar` - Not strictly a class itself, contains the basic Ruby core/stdlib: `String`, `Fixnum`, `Bignum`, `Float`, and `Time` instances that represent a
single cell in a time series collection.
* `Row` - an `Array` subclass that holds a collection of scalars.
* `Collection` -  an `Array` subclass that holds a collection of rows.

### Scalars in Ruby and Riak TS

* Ruby's `nil` round-trips to Riak TS's `NULL` without issue so long as the Riak TS cell is nullable.
* Ruby's `String` and Riak TS's `varchar` are interchanged without loss.
* `Float` instances in Ruby on platforms with 64-bit `double`s will interchange with Riak TS's `double` without loss.
* Ruby's `Fixnum` turns into Riak TS's `sint64` and back without loss.
* Low-magnitude Ruby `Bignum` instances that fit in a `sint64` are interchanged without loss.
* High-magnitude Ruby `Bignum` instances raise the error: `Riak::TimeSeriesError::SerializeBigIntegerError`.
* Ruby `BigDecimal` numbers are converted to a `Float` and serialized as a Riak
TS `double`, which will later de-serialize as a Ruby `Float`.
* Ruby `Complex` numbers raise the error:
`Riak::TimeSeriesError::SerializeComplexNumberError`.
* Ruby `Rational` numbers raise the error:
`Riak::TimeSeriesError::SerializeRationalNumberError`.


## Operations

Riak TS supports five basic operations: single-key reads and deletes, key
listing, SQL queries, and writes/submissions.

The examples on this page will assume you are using the following table schema:

```sql
CREATE TABLE GeoCheckin
(
   region       varchar   not null,
   state        varchar   not null,
   time         timestamp not null,
   weather      varchar not null,
   temperature  double,
   PRIMARY KEY (
     (region, state, quantum(time, 15, 'm')), /* <-- PARTITION KEY */
     region, state, time /* <-- LOCAL KEY */
   )
)
```

### Single-key Reads

To load a single row with a given key use `Riak::TimeSeries::Read`:

```ruby
read_operation = Riak::TimeSeries::Read.new client, 'GeoCheckin'
read_operation.key = ['South Atlantic', 'South Carolina', Time.now]
results = read_operation.read!
```

#### Constructor

The `new` class method takes two arguments: `client` (the `Riak::Client` to use) and `table_name` (a `String` of the name of the table).

#### Instance Accessors

* `key` - (read/write) this is how you tell the `Read` object what key to find.
* `client` - (read-only) the `Riak::Client` this `Read` will use.
* `table_name` - (read-only) the `String` table name this `Read` will use.

#### Instance Method

* `read!` - issues the read operation to Riak and returns a `Row` of data. If
no data are found, returns `nil`.


### Single-key Deletes

To delete a single row with a given key, use `Riak::TimeSeries::Delete`:

```ruby
delete_operation = Riak::TimeSeries::Deletion.new client, 'GeoCheckin'
delete_operation.key = ['South Atlantic', 'South Carolina', Time.now]
delete_operation.delete!
```

#### Constructor

The `new` class method takes two arguments: `client` (the `Riak::Client` to use)
and `table_name` (a `String` of the name of the table).

#### Instance Accessors

* `key`-  (read/write) this is how you tell the `Deletion` object what key to find
and delete.
* `client` - (read-only) the `Riak::Client` this `Deletion` will use.
* `table_name` -  (read-only) the `String` table name this `Deletion` will use.

#### Instance Method

* `delete!` - issues the deletion to Riak.


### Key Listing

To list keys in a table, use the `Riak::TimeSeries::List` class.

>**WARNING:** Listing keys is a very expensive operation for a Riak TS cluster.

```ruby
list_operation = Riak::TimeSeries::List.new client, 'GeoCheckin'
list_operation.issue! do |key|
  puts key
end

results = list_operation.issue!
```

`List` is only available via streaming API by invoking the `issue!` method. If the `issue!` method is called with a block, `issue!` will `yield` each key (which will be a `Row` object) to the provided block in the order that it receives them. When called without a block, `issue!` returns and stores the listing of all keys.

#### Constructor

The `new` class method takes two arguments: `client` (the `Riak::Client` to use)
and `table_name` (a `String` of the name of the table).

#### Instance Accessors

* `client` - (read-only) the `Riak::Client` this `List` will use.
* `table_name` - (read-only) the `String` table name this `List` will use.
* `timeout` - how many milliseconds Riak should wait for listing.
* `results` - either the `Riak::TimeSeries::Collection` of found keys or `nil` if
the listing wasn't retrieved for storage.

#### Instance Method

The `issue!` method that starts the key listing can work in two different ways.

Without a block, the streaming key listing will be buffered up into a
`Riak::TimeSeries::Collection`. The `Collection` will also be available from the
`results` accessor.

>**WARNING:** on a table with many rows, the `Collection` can
be extremely large and will cause extra memory load, garbage collections, and
other issues.

With a block, each key from the listing will be `yield`ed into your block as a
`Riak::TimeSeries::Row` and not otherwise saved.

### SQL Queries

SQL queries are sent with a `Riak::TimeSeries::Query` object:

```ruby
query = Riak::TimeSeries::Query.new client, sql
query.issue!
query.results
```

#### Constructor

The `new` class method takes two arguments: `client` (the `Riak::Client` to use)
and a `String` of `query_text`.

#### Instance Accessors

* `query_text` - (read/write) the SQL query `String`.
* `client` - (read-only) the `Riak::Client` this `Query` will use.
* `results` - (read-only) the `Riak::TimeSeries::Collection` of results.

#### Instance Method

The `issue!` method issues the query to Riak and populates the `results`
accessor with the results.


### Writing

Data can be submitted to Riak TS with the `Riak::TimeSeries::Submission` class.
Measurements are expected as an `Array` which represents a collection containing
one or more `Array` instances representing rows that contain scalar values for cells.

```ruby
submission = Riak::TimeSeries::Submission.new client, 'GeoCheckin'
submission.measurements = [
  ['South Atlantic', 'South Carolina', Time.now - 5, 1, 2, '3'],
  ['South Atlantic', 'South Carolina', Time.now - 4, 4, 5, '6']
]

submission.write!
```

#### Constructor

The `new` class method takes two arguments: `client` (the `Riak::Client` to use)
and a `String` of `query_text`.

#### Instance Accessors

* `client` - (read-only) the `Riak::Client` this `Write` will use.
* `table_name` - (read-only) the `String` table name this `Write` will use.
* `measurements` - (read-write) the `Array<Array<scalar>>` of measurements. The
inner `Array`s of scalars are order-sensitive: data will be written in the order
the cells are in the table's DDL.

#### Instance Method

The `write!` method writes the data to Riak TS.