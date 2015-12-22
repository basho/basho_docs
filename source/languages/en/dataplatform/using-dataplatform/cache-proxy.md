---
title: Using Cache Proxy
project: dataplatform
version: 1.0.0+
document: guide
toc: true
index: true
audience: beginner 
---

[setup a cluster]: http://docs.basho.com/dataplatform/1.1.0/using-dataplatform/configuration/setup-a-cluster/
[getting started cache proxy#Config]: http://docs.basho.com/dataplatform/1.1.0/using-dataplatform/configuration/getting-started-with-cache-proxy/#Configure-Cache-Proxy
[using bdp#Start-Services]: http://docs.basho.com/dataplatform/1.1.0/using-dataplatform/using-bdp/#Start-Services
[stop service command]: http://docs.basho.com/dataplatform/1.1.0/using-dataplatform/dataplatform-commands/#stop-service
[start service command]: http://docs.basho.com/dataplatform/1.1.0/using-dataplatform/dataplatform-commands/#start-service

## Adding more Redis Servers

To add more Redis Servers to your cluster:

* Add a service configuration for the new Redis Server using BDP Service Manager.
* Register this new Redis server with the service group of Cache Proxies, by adding its IP address to Cache Proxy group configuration using ‘--force’ option.

Check out [Set Up a Basho Data Platform Cluster][setup a clister] for instructions. 

Once you've registed the Redis Server:

* [Start the new Redis Server instance using Service Manager][using bdp#Start-Services].
* Restart each Cache Proxy instance by [stopping][stop service command] and then [starting][start service command] it using Service Manager.

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

