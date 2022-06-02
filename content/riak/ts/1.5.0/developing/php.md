---
title: "PHP Client API"
description: "PHP Client API"
menu:
  riak_ts-1.5.0:
    name: "PHP"
    identifier: "ts_php_api"
    weight: 401
    parent: "develop"
project: "riak_ts"
project_version: "1.5.0"
toc: true
aliases:
    - /riakts/1.5.0/developing/php/
---


You can develop applications and tools using Riak TS with the Riak PHP client.
This document covers the PHP API for Riak TS.


## Overview

Language | Source | Documentation |
:--------|:-------|:--------------|
PHP | [riak-php-client](https://github.com/basho/riak-php-client) | [apigen](http://basho.github.io/riak-php-client)

TS support within the PHP client is implemented through the following command builders, all beginning Basho\Riak\Command\Builder\TimeSeries:

* StoreRows
* FetchRow
* DeleteRow
* Query
* DescribeTable


## Data Types

 * `Cell` - Holds the cell name and a single piece of data.
 * `Row` - An array of cells.


### Data Type Details

#### `Cell`

A cell contains a piece of data for a row in a Riak TS table.

>**Note:** Cells are immutable once created.

A cell can hold 5 different types of raw data:

* `Varchar` - standard PHP string
* `SInt64` - any signed 64-bit integers.
* `Double` - any 64-bit floating point numbers.
* `Timestamp` - any Unix epoch timestamp
* `Boolean` - a true/false value.

##### Constructors

A cell is constructed by providing the name of the column the cell resides in, e.g. `$cell = (new Cell("region"))->setValue("South Atlantic")`

##### Instance Methods

Cells have object getters to retrieve the column name, the cell value and the data type of the value.

#### `Row`

A row contains an array of cells.


### `Response`

The object returned by all non-query commands (`Store`, `Fetch`, `Delete`). `Fetch` command will have values populated in instance method `getRow()`.


### `Query Response`

The query response is the result set from a `query` command. The response object will have the first row available within `getResult()` and all results within `getResults()`.

>**Note:** Query results are immutable.


## Command Classes Index

All command classes have a `Builder` class to create and build each command.

* `Delete` - Deletes a single row by it's key values.
* `Fetch` - Fetches a single row by it's key values.
* `Query` - Allows you to query a Riak TS table with the given query string.
* `Store` - Stores data in the Riak TS table.


### Command Class Details

Each command is created through a `Builder` class. This pattern ensures the commands are created as correctly as possible. To create the command from the builder, call the `.build()` method.

To execute any command, you must have an instance of a `\Basho\Riak` object. You then pass the Riak object as a parameter into the constructor of the command builder.



#### `Delete`

Deletes a single row by it's key values.

```php
# delete a row
$response = (new Command\Builder\TimeSeries\DeleteRow($riak))
    ->atKey([
        (new Cell("region"))->setValue("South Atlantic"),
        (new Cell("state"))->setValue("South Carolina"),
        (new Cell("time"))->setTimestampValue(1420113600),
    ])
    ->inTable('GeoCheckins')
    ->build()
    ->execute();

if (!$response->isSuccess()) {
    echo $response->getMessage();
    exit;
}
```


#### `Fetch`

Fetches a single row by it's key values.

```php
/** @var Command\TimeSeries\Response $response */
$response = (new Command\Builder\TimeSeries\FetchRow($riak))
    ->atKey([
        (new Cell("region"))->setValue("South Atlantic"),
        (new Cell("state"))->setValue("South Carolina"),
        (new Cell("time"))->setTimestampValue(1420113600),
    ])
    ->inTable('GeoCheckins')
    ->build()
    ->execute();

if (!$response->isSuccess()) {
    echo $response->getMessage();
    exit;
}

# output row data
foreach ($response->getRow() as $index => $column) {
    switch ($column->getType()) {
        case Riak\TimeSeries\Cell::INT_TYPE:
            printf("Column %d: %s is an integer equal to %d\n", $index, $column->getName(), $column->getValue());
            break;
        case Riak\TimeSeries\Cell::DOUBLE_TYPE:
            printf("Column %d: %s is a double equal to %d\n", $index, $column->getName(), $column->getValue());
            break;
        case Riak\TimeSeries\Cell::BOOL_TYPE:
            printf("Column %d: %s is a boolean equal to %s\n", $index, $column->getName(), $column->getValue());
            break;
        case Riak\TimeSeries\Cell::TIMESTAMP_TYPE:
            printf("Column %d: %s is a timestamp equal to %d\n", $index, $column->getName(), $column->getValue());
            break;
        default:
            printf("Column %d: %s is a string equal to %s\n", $index, $column->getName(), $column->getValue());
            break;
    }
}
```


#### `Query`

Allows you to query a Riak TS table with the given query string.

```php
$response = (new Command\Builder\TimeSeries\Query($riak))
    ->withQuery("select * from GeoCheckins where region = 'South Atlantic' and state = 'South Carolina' and (time > 1420113500 and time < 1420116000)")
    ->build()
    ->execute();

# output rows
foreach ($response->getResults() as $row_index => $row) {
    foreach ($row as $column_index => $column) {
        switch ($column->getType()) {
            case Riak\TimeSeries\Cell::INT_TYPE:
                printf("Column %d: %s is an integer equal to %d\n", $index, $column->getName(), $column->getValue());
                break;
            case Riak\TimeSeries\Cell::DOUBLE_TYPE:
                printf("Column %d: %s is a double equal to %d\n", $index, $column->getName(), $column->getValue());
                break;
            case Riak\TimeSeries\Cell::BOOL_TYPE:
                printf("Column %d: %s is a boolean equal to %s\n", $index, $column->getName(), $column->getValue());
                break;
            case Riak\TimeSeries\Cell::TIMESTAMP_TYPE:
                printf("Column %d: %s is a timestamp equal to %d\n", $index, $column->getName(), $column->getValue());
                break;
            default:
                printf("Column %d: %s is a string equal to %s\n", $index, $column->getName(), $column->getValue());
                break;
        }
    }
}
```


#### `Store`

Stores data in the Riak TS table.

```php
# store a row
$response = (new Command\Builder\TimeSeries\StoreRows($riak))
    ->inTable('GeoCheckins')
    ->withRow([
        (new Cell("region"))->setValue("South Atlantic"),
        (new Cell("state"))->setValue("South Carolina"),
        (new Cell("time"))->setTimestampValue(1420113600),
        (new Cell("weather"))->setValue("hot"),
        (new Cell("temperature"))->setValue(23.5),
    ])
    ->build()
    ->execute();

if (!$response->isSuccess()) {
    echo $response->getMessage();
    exit;
}


# store rows
$response = (new Command\Builder\TimeSeries\StoreRows($riak))
    ->inTable('GeoCheckins')
    ->withRows([
        [
            (new Cell("region"))->setValue("South Atlantic"),
            (new Cell("state"))->setValue("South Carolina"),
            (new Cell("time"))->setTimestampValue(1420115400),
            (new Cell("weather"))->setValue("hot"),
            (new Cell("temperature"))->setValue(22.4),
        ],
        [
            (new Cell("region"))->setValue("South Atlantic"),
            (new Cell("state"))->setValue("South Carolina"),
            (new Cell("time"))->setTimestampValue(1420117200),
            (new Cell("weather"))->setValue("warm"),
            (new Cell("temperature")), # null value
        ],
    ])
    ->build()
    ->execute();

if (!$response->isSuccess()) {
    echo $response->getMessage();
    exit;
}
```
