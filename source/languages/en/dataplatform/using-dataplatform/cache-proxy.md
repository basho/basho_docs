---
title: Using Cache Proxy
project: dataplatform
version: 1.0.0+
document: guide
toc: true
index: true
audience: beginner 
---

## Locating packaged Redis files

Redis files are located under the `priv` subtree of the Basho Data Platform.

For example, on CentOS 7:

```bash
/usr/lib64/riak/lib/data_platform-1/priv/redis
```

## Changing Redis server settings

Basho Data Platform 1.0 does not provide a default `redis.conf` file. If required, you can create a `redis.conf` and modify Redis launch command (`start.sh`) to include a link to it. 

>**Note:** This is not officially supported functionality.

## Configuring Redis server maximal memory

By setting the `maxmemory` parameter in `redis.conf`.

