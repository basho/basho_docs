---
title: "Python Client API"
description: "Python Client API"
menu:
  riak_ts-1.0.0:
    name: "Python"
    identifier: "ts_python_api"
    weight: 404
    parent: "developing"
project: "riak_ts"
project_version: "1.0.0"
toc: true
aliases:
    - /riakts/1.0.0/developing/python/
canonical_link: "https://docs.basho.com/riak/ts/latest/developing/python"
---


You can develop with Riak TS through the Python client. This document covers the Python protobuf requests to Riak TS.

## Overview
To use Time Series with the Python client, we've added 5 new operations to the `riak.client.RiakClient` object.

The Time Series API is only available through Protocol Buffers and not HTTP, so please set up your Python client accordingly.

Language | Source | Documentation | Download
:--------|:-------|:--------------|:--------
Python | [riak-python-client](https://github.com/basho/riak-python-client) | [sphinx](http://basho.github.com/riak-python-client) | [PyPI](http://pypi.python.org/pypi?:action=display&name=riak#downloads)


## RiakClient TS Operations

### Operations Index
 * Get - Fetch a single row based on the primary key values provided.
 * Put - Store 1 or more rows to a Time Series table.
 * Delete - Delete a single row based on the primary key values provided.
 * Query - Allows you to query a Time Series table, with the given query string.
 * Stream ListKeys - Lists the primary keys of all the rows in a Time Series table.

### Operations Details

#### Get

`ts_get(table, key)`

Retrieve timeseries value by key.

*Note* - This request is automatically retried.
   times if it fails due to network error.

|Parameter| Parameter Type                             | Description                 |
|---------|--------------------------------------------|-----------------------------|
|`table`  | string or class `Table <riak.table.Table>` | The timeseries table.       |
|`key  `  | list or dict                               | The timeseries value's key. |

*Return Type*: class`TsObject <riak.ts_object.TsObject>`
 
#### Put

`ts_put(tsobj)`

Stores time series data in the Riak cluster.

*Note* - This request is automatically retried.
   times if it fails due to network error.

|Parameter| Parameter Type       | Description                      |
|---------|----------------------|----------------------------------|
|`tsobj`  | class `RiakTsObject` | The time series object to store. |

*Return Type*: boolean

#### Delete

`ts_delete(table, key)`

Delete timeseries value by key.

*Note* - This request is automatically retried.
   times if it fails due to network error.

|Parameter| Parameter Type                             | Description                 |
|---------|--------------------------------------------|-----------------------------|
|`table`  | string or class `Table <riak.table.Table>` | The timeseries table.       |
|`key  `  | list or dict                               | The timeseries value's key. |

*Return Type*: boolean

#### Query

`ts_query(table, query)`

Queries time series data in the Riak cluster.

*Note* - This request is automatically retried.
   times if it fails due to network error.

|Parameter| Parameter Type                             | Description          |
|---------|--------------------------------------------|----------------------|
|`table`  | string or class `Table <riak.table.Table>` | The timeseries table.|
|`query`  | string                                     | The timeseries query.|

*Return Type*: Class `TsObject <riak.ts_object.TsObject>`

#### Stream ListKeys

`riak.client.RiakClient:ts_stream_keys(table, timeout=None)`

Lists all keys in a time series table via a stream. This is a
generator method which should be iterated over.
The caller should explicitly close the returned iterator,
either using :func:`contextlib.closing` or calling `close()`
explicitly. Consuming the entire iterator will also close the
stream. If it does not, the associated connection might
not be returned to the pool. Example::

    
```python 
from contextlib import closing
# Using contextlib.closing
with closing(client.ts_stream_keys(mytable)) as keys:
    for key_list in keys:
        do_something(key_list)
# Explicit close()
stream = client.ts_stream_keys(mytable)
for key_list in stream:
     do_something(key_list)
stream.close()
```

|Parameter| Parameter Type                             | Description                         |
|---------|--------------------------------------------|-------------------------------------|
|`table`  | string or class `Table <riak.table.Table>` | the table from which to stream keys |
|`timeout`| int                                        | A timeout value in milliseconds     |


*Return Type*: iterator