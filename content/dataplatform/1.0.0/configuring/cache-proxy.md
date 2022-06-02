---
title: "Getting Started with Cache Proxy"
description: ""
project: "dataplatform"
project_version: "1.0.0"
menu:
  dataplatform-1.0.0:
    name: "Getting Started with Cache Proxy"
    identifier: "configuring_cache_proxy"
    weight: 102
    parent: "configuring"
toc: true
aliases:
  - /dataplatform/1.0.0/using-dataplatform/configuration/getting-started-with-cache-proxy/
  - /dataplatform/latest/configuring/cache-proxy/
---

[bdp install]: {{<baseurl>}}dataplatform/1.0.0/installing/
[bdp configure]: {{<baseurl>}}dataplatform/1.0.0/configuring/setup-a-cluster/
[bdp configure add services]: {{<baseurl>}}dataplatform/1.0.0/configuring/setup-a-cluster/#add-services
[ee]: http://info.basho.com/Wiki_Riak_Enterprise_Request.html


> Cache proxy is available to [Enterprise users only][ee].

Now that you’ve [set up a Basho Data Platform cluster][bdp configure], which included [adding a service configuration for Redis and cache proxy][bdp configure add services], you’re ready to use cache proxy with any Redis client that supports the `GET` command.

This page will walk you through configuring and using BDP cache proxy.

## Prerequisites

### Setting Your Environment

Before you begin using cache proxy, you may want to set your environment variables. You will be using these paths frequently, and you may find it easier to use their logical names rather than typing the paths out each time.

We suggest the following environment settings (and the remainder of this page assumes you are using these settings).

If you are running Ubuntu, set this environment variable at the command line:

```bash
BDP_PRIV="/usr/lib/riak/lib/data_platfor-1/priv"
```

If you are running CentOS, set this environment variable at the command line:

```bash
BDP_PRIV="/usr/lib64/riak/lib/data_platform-1/priv"
```

## Configure Cache Proxy

Cache proxy was configured for you during the general BDP install and configuration process. BDP Service Manager provides everything you need to configure, start, and stop the cache proxy.

If you skipped the configuration instructions or would otherwise like to set up a different cache proxy configuration, this section will show you how.


### Configuration File

If you are curious about cache proxy's internal configuration or just want to look at your configuration to make sure it is correct or to see what hostname and port is being used, you can find it in `$BDP_PRIV/cache_proxy/config/cache_proxy_$CACHE_PROXY_PORT.yml`.

{{% note %}}
This level of knowledge is not necessary for normal operation, but may come in
handy when diagnosing and resolving issues, such as a firewall rule not
allowing ingress to the cache proxy host on the configured port.
{{% /note %}}


Once you've opened the file, you might find the following settings helpful:

* `CACHE_PROXY_PORT` - Specifies the cache proxy client port which uses a reduced set of the Redis protocol, default: 22122.
* `CACHE_PROXY_STATS_PORT` - Sets the cache proxy statistics port which is useful for monitoring, default: 22123.
* `CACHE_TTL` - Specifies the cache time-to-live (TTL) expiry, which is used when cache proxy sets a value to Redis, default: "15s" (15 seconds).
* `RIAK_KV_SERVERS` - A comma-separated list of IP:PORT where the PORT is the protocol buffer (pb) port of the Riak KV servers.
* `REDIS_SERVERS` - A comma-separated list of IP:PORT where the PORT is the Redis client port.

### Redis Service Configuration

To add a Redis service configuration:

```bash
sudo data-platform-admin add-service-config my-redis redis HOST="0.0.0.0" REDIS_PORT="»REDIS_PORT«"
```

### Cache Proxy Service Configuration

To add a Cache Proxy service configuration:

```bash
sudo data-platform-admin add-service-config my-cache-proxy cache-proxy HOST="0.0.0.0" CACHE_PROXY_PORT="»CACHE_PROXY_PORT«" CACHE_PROXY_STATS_PORT="»CACHE_PROXY_STATS_PORT«" CACHE_TTL="15s" RIAK_KV_SERVERS="»RIAK_IP_1«:»RIAK_PB_PORT«,»RIAK_IP_2«:»RIAK_PB_PORT«" REDIS_SERVERS="»REDIS_IP_1«:»REDIS_PORT«,»REDIS_IP_2«:»REDIS_PORT«"
```

### Start Redis Service

To start the Redis service on a BDP node, run:

```bash
sudo data-platform-admin start-service »RIAK_NODENAME«@»RIAK_IP« my-group my-redis
```

### Start Cache Proxy Service

To start the cache proxy service on a BDP node, run:

```shell
sudo data-platform-admin start-service »RIAK_NODENAME«@»RIAK_IP« my-group my-cache-proxy
```

Here's a simple way to test your cache proxy configuration:

```bash
# NOTE: BDP_PRIV set above
RIAK_IP=»RIAK_IP«
RIAK_HTTP_PORT=»RIAK_HTTP_PORT«
CACHE_PROXY_IP=»CACHE_PROXY_IP«
CACHE_PROXY_PORT=»CACHE_PROXY_PORT«
REDIS_IP=»REDIS_IP«
REDIS_PORT=»REDIS_PORT«
BUCKET="my-bucket"
KEY="my-key"
curl -s -X PUT -d "my-value" "http://$RIAK_IP:$RIAK_HTTP_PORT/buckets/$BUCKET/keys/$KEY"
$BDP_PRIV/redis/bin/redis-cli -h $CACHE_PROXY_IP -p $CACHE_PROXY_PORT get $BUCKET:$KEY
$BDP_PRIV/redis/bin/redis-cli -h $REDIS_IP -p $REDIS_PORT get $BUCKET:$KEY
```

### Run the Read-Through Test

You can test your cache proxy setup with a specific script if you installed the Basho Data Platform Extras package (Enterprise only).

The test ensures that your configuration correctly tracks values obtained from Riak KV and cache proxy. The main actions of the read-through test are:

* Delete the Riak object at the `test` bucket with the key `foo`, which checks that there are no siblings.
* PUT a Riak object with the value 'bar' at the `test` bucket with the key `foo`.
* GET the Riak object at the `test` bucket with the key `foo`.
* GET the string-representation of the object from cache proxy using the key `test:foo`. (Cache proxy should parse out the first portion of the Redis colon-separated key (namespace) to identify which Riak bucket to perform the backend read from.)
* Assert that the value obtained from the previous cache proxy GET is 'bar'.

The testing script is located in `$BDP_PRIV/cache-proxy/test/read_through_test.sh`, and takes the following command-line arguments:

* $1 - Riak KV HTTP port
* $2 - Riak KV HTTP host
* $3 - Riak KV test bucket name

Before we get to the command to run the test script, let's look at the general bash commands the read-through test is running:

```bash
# set test environment
RIAK_HTTP_IP="127.0.0.1"
RIAK_HTTP_PORT="8098"
CACHE_PROXY_IP="127.0.0.1"
CACHE_PROXY_PORT="22122"
CACHE_PROXY_STATISTICS_PORT="22123"
RIAK_TEST_BUCKET="test"
KEY="foo"
VALUE="bar"

# DELETE Riak object, ensure no siblings
curl -s -X DELETE "http://$RIAK_HTTP_IP:$RIAK_HTTP_PORT/buckets/$RIAK_TEST_BUCKET/keys/$KEY"

# PUT Riak object
curl -s -X PUT -d "$VALUE" "http://$RIAK_HTTP_IP:$RIAK_HTTP_PORT/buckets/$RIAK_TEST_BUCKET/keys/$KEY"

# GET Riak object
RIAK_VALUE=$(curl -s -X GET "http://$RIAK_HTTP_IP:$RIAK_HTTP_PORT/buckets/$RIAK_TEST_BUCKET/keys/$KEY")

# GET Cache Proxy value
CACHE_VALUE=$(redis-cli -h "$CACHE_PROXY_IP" -p "$CACHE_PROXY_PORT" "$RIAK_TEST_BUCKET:$KEY"

# DELETE Riak object, cleanup
curl -s -X DELETE "http://$RIAK_HTTP_IP:$RIAK_HTTP_PORT/buckets/$RIAK_TEST_BUCKET/keys/$KEY"

# Assert
if [[ "RIAK_VALUE" == "$CACHE_VALUE" ]]; then
    RESULT="Success"
else
    RESULT="FAIL"
fi
echo "$RESULT - read $RIAK_VALUE from Riak and $CACHE_VALUE from Cache Proxy."
```

Are you ready to run the read-through test? You can run it as follows:

```bash
./read_through_test.sh 22122 8098 test
```

#### Exceptions
If the test does not pass, verify that both Redis and cache proxy are running. You can do this by running:

```bash
sudo data-platform-admin services
```

The result should list `my-redis` and `my-cache-proxy` with the Running Services table.

Also, verify that Riak KV is started and listening on the protocol buffer port specified:

```bash
sudo riak config effective |grep proto
```

If cache proxy is misconfigured, reconfigure it by updating the service config with the `--force` option.

If cache proxy is configured correctly and all required services are running, you may want to restart each service from front to back as follows:

1. Stop cache proxy
2. Stop Redis.
3. [Optional] Restart Riak KV (This should only be necessary if Riak KV is not responding to protocol buffer requests.)
4. Start Redis.
5. Start cache proxy.

```bash
sudo data-platform-admin stop-service 'my-cache-group' 'my-cache-proxy'
sudo data-platform-admin stop-service 'my-cache-group' 'my-cache-proxy'

# optional
sudo riak restart

sudo data-platform-admin start-service 'my-cache-group' 'my-cache-proxy'
sudo data-platform-admin start-service 'my-cache-group' 'my-cache-proxy'
```

## Using Cache Proxy
Once you've successfully configured cache proxy and established a Riak KV and Redis client in the language of your choosing, you're ready to start using cache proxy.

For objects that should not be cached, interact with Riak KV as usual issuing GET, PUT, and DELETE commands through the Riak client.

For objects that should be cached, read from cache proxy issuing GET commands through the Redis client, and write to Riak KV issuing PUT and DELETE commands through the Riak client.

### Monitoring

#### Cache Proxy

Internally, the BDP service manager monitors processes, so will automatically restart a service with the correct configuration in the event that the service’s process was killed or terminated by other means.

For additional monitoring, cache proxy provides statistics on service availability.  The statistics provided are generally useful in monitoring the health of the cache proxy service.

For example, running the following:

```bash
telnet $CACHE_PROXY_IP $CACHE_PROXY_STATISTICS_PORT
```

Returns statistic results:

```json
{
    "bdp_cache_proxy": {
        "192.168.50.2:6379": {
            "in_queue": 0,
            "in_queue_bytes": 0,
            "out_queue": 0,
            "out_queue_bytes": 0,
            "request_bytes": 216,
            "requests": 9,
            "response_bytes": 39,
            "responses": 4,
            "server_connections": 1,
            "server_ejected_at": 0,
            "server_eof": 0,
            "server_err": 0,
            "server_timedout": 0
        },
        "192.168.50.3:6379": {
            "in_queue": 0,
            "in_queue_bytes": 0,
            "out_queue": 0,
            "out_queue_bytes": 0,
            "request_bytes": 0,
            "requests": 0,
            "response_bytes": 0,
            "responses": 0,
            "server_connections": 0,
            "server_ejected_at": 0,
            "server_eof": 0,
            "server_err": 0,
            "server_timedout": 0
        },
        "192.168.50.4:6379": {
            "in_queue": 0,
            "in_queue_bytes": 0,
            "out_queue": 0,
            "out_queue_bytes": 0,
            "request_bytes": 90,
            "requests": 5,
            "response_bytes": 258,
            "responses": 2,
            "server_connections": 0,
            "server_ejected_at": 0,
            "server_eof": 0,
            "server_err": 0,
            "server_timedout": 0
        },
        "client_connections": 0,
        "client_eof": 6,
        "client_err": 0,
        "forward_error": 0,
        "fragments": 0,
        "server_ejects": 0
    },
    "curr_connections": 4,
    "service": "nutcracker",
    "source": "vagrant",
    "timestamp": 1438301846,
    "total_connections": 10,
    "uptime": 7227,
    "version": "0.4.0"
}
```

Using the above, you should be able to determine metrics changes that would flag a change in service health.  With this information you can implement monitoring to help guarantee the overall health of cache proxy, other BDP services, and the custom software within your overall solution.  While we do not specifically endorse a specific monitoring solution, the open interface to statistics allows for several monitoring solutions, the following is a brief listing:

* Custom - https://github.com/gfranxman/NutcrackerMonitor
* NewRelic - http://newrelic.com/plugins/schoology/245
* Nagios - https://github.com/schoology/twemproxy_nagios

#### Redis

Various Redis monitoring solutions exist in the market and like monitoring the cache proxy, these monitoring solutions make underlying calls to obtain Redis statistics, typically via the `info` command alone.

As with cache proxy, Redis statistics available on the Redis client port allow for monitoring via solutions such as the following:

* Custom - http://volumelabs.net/redis_monitoring/
* NewRelic - http://newrelic.com/plugins/poison-pen-llc/28
* Nagios - https://exchange.nagios.org/directory/Plugins/Databases/check_redis-2Epl/details
