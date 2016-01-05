---
title: Working with Redis
project: dataplatform
version: 1.1.0+
document: guide
toc: true
index: true
audience: beginner 
---

[setup a cluster]: http://docs.basho.com/dataplatform/1.1.0/using-dataplatform/configuration/setup-a-cluster/
[using bdp#start services]: http://docs.basho.com/dataplatform/1.1.0/using-dataplatform/using-bdp/#Start-Services
[command stop-service]: http://docs.basho.com/dataplatform/1.1.0/using-dataplatform/dataplatform-commands/#stop-service
[command start-service]: http://docs.basho.com/dataplatform/1.1.0/using-dataplatform/dataplatform-commands/#start-service

## Locating Redis files

Redis files are located in the BDP install folder under the `priv` directorty.

For example, on CentOS 7:

```bash
/usr/lib64/riak/lib/data_platform-1/priv/redis
```

## Adding Redis configuration

BDP does not provide a default `redis.conf` file.

If required, create a `redis.conf` and modify the Redis launch command in `start.sh` to include a link to it.

>**Note:** This is not officially supported functionality.

## Configuring maximal memory used by Redis

Add a `redis.conf` file by following the instructions above and set the `maxmemory` parameter in the `redis.conf` file.

## Adding more Redis servers

To add another Redis server to a BDP cluster:

* Add service config for the new Redis Server using BDP Service Manager.
* Register the new Redis server by adding its IP address to Cache Proxy group configuration using the ‘--force’ option.

Check out [Set Up a Basho Data Platform Cluster][setup a cluster] for instructions.

After registering the new Redis server:

* Start the new Redis Server instance [using BDP Service Manager][using bdp#start services].
* Restart each Cache Proxy instance by [stopping][command stop-service] and then [starting][command start-service] it using Service Manager.
