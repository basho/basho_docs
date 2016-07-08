---
title: "Python Client API"
description: "Python Client API"
menu:
  riak_ts-1.2.0:
    name: "Python"
    identifier: "ts_python_api"
    weight: 404
    parent: "develop"
project: "riak_ts"
project_version: "1.2.0"
toc: true
aliases:
    - /riakts/1.2.0/developing/python/
canonical_link: "https://https://docs.basho.com/riak/ts/latest/developing/python"
---


You can develop applications and tools using Riak TS with the Riak Python client. This document covers the Python API for Riak TS.


## Overview

To use Riak TS with the Python client, we've added 5 new operations to the `riak.client.RiakClient` object.

The Riak TS API is only available through Protocol Buffers and not HTTP, so please set up your Python client accordingly.


## Prerequisites

In order for the Python client to install successfully on Ubuntu and CentOS, you must install some dependencies.

**Ubuntu**

```
sudo apt-get install -y python-setuptools python-dev libffi-dev libssl-dev
```

**CentOS**

```
sudo yum install -y python-setuptools python-devel libffi-devel openssl-deve
```


## Client Operations

### Operations Index

 * `Get` - Fetch a single row based on the primary key values provided.
 * `Put` - Store 1 or more rows to a Riak TS table.
 * `Delete` - Delete a single row based on the primary key values provided.
 * `Query` - Query a Riak TS table with the given query string.
 * `Stream ListKeys` - Lists the primary keys of all the rows in a Riak TS table.


### Operations Details

#### Get
Retrieve time series value by key.

`ts_get(table, key)`

>**Note:** This request is automatically retried 3 times if it fails due to network error.

|Parameter| Parameter Type                             | Description                 |
|---------|--------------------------------------------|-----------------------------|
|`table`  | string or class `Table <riak.table.Table>` | The time series table.       |
|`key  `  | list or dict                               | The time series value's key. |

**Return Type**: class `TsObject <riak.ts_object.TsObject>`


#### Put
Stores time series data in the Riak TS cluster.

`ts_put(tsobj)`

>**Note:** This request is automatically retried 3 times if it fails due to network error.

|Parameter| Parameter Type       | Description                      |
|---------|----------------------|----------------------------------|
|`tsobj`  | class `RiakTsObject` | The time series object to store. |

**Return Type**: boolean


#### Delete
Delete time series value by key.

`ts_delete(table, key)`

>**Note:** This request is automatically retried 3 times if it fails due to network error.

|Parameter| Parameter Type                             | Description                 |
|---------|--------------------------------------------|-----------------------------|
|`table`  | string or class `Table <riak.table.Table>` | The time series table.       |
|`key  `  | list or dict                               | The time series value's key. |

**Return Type**: boolean


#### Query
Queries time series data in the Riak cluster.

`ts_query(table, query)`

>**Note:** This request is automatically retried 3 times if it fails due to network error.

|Parameter| Parameter Type                             | Description          |
|---------|--------------------------------------------|----------------------|
|`table`  | string or class `Table <riak.table.Table>` | The timeseries table.|
|`query`  | string                                     | The timeseries query.|

**Return Type**: Class `TsObject <riak.ts_object.TsObject>`


#### Stream ListKeys
Lists all keys in a Riak TS table via a stream. This is a
generator method which should be iterated over. The caller should explicitly close the returned iterator,
either using :func:`contextlib.closing` or calling `close()`
explicitly. Consuming the entire iterator will also close the
stream. If it does not, the associated connection might
not be returned to the pool. 

`riak.client.RiakClient:ts_stream_keys(table, timeout=None)`

Example:

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
|`table`  | string or class `Table <riak.table.Table>` | The table from which to stream keys. |
|`timeout`| int                                        | A timeout value in milliseconds.     |

**Return Type**: iterator