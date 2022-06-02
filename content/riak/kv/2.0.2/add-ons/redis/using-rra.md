---
title: "Using Riak Redis Add-on"
description: ""
project: "riak_kv"
project_version: "2.0.2"
menu:
  riak_kv-2.0.2:
    name: "Using Redis Addon"
    identifier: "add-ons_redis_getstarted"
    weight: 302
    parent: "add-ons_redis"
toc: true
commercial_offering: true
aliases:
  - /riak/kv/2.0.2/add-ons/redis/get-started-with-rra
---

[addon redis develop]: ../developing-rra/
[addon redis setup]: ../set-up-rra/
[dev api http]: {{<baseurl>}}riak/kv/2.0.2/developing/api/http/
[ee]: http://basho.com/contact/


Now that you’ve [set up Riak Redis Add-on (RRA)][addon redis setup], you're ready to use RRA with any Redis client which supports `GET`, `PUT` and `DEL` operations.

This page will walk you through using RRA.

## Prerequisites

We assume that the Redis client (`redis-cli`) is installed, either alongside the Redis server or on a test machine.

You will need the list of Riak KV and Riak Redis Add-on host:port combinations. For testing, Riak KV values are obtained via the [HTTP API][dev api http].

## Run the Read-Through Test

Throughout this test example, the bucket "test" and key "foo" are used to
demonstrate how to address the hieararchical namespace support in Riak KV
through the flat Redis key. The bucket type is not specified in this example,
so is effectively the default bucket type, named "default". For additional
information regarding key namespace, see [develop Riak Redis Add-on (RRA)][addon redis develop].

The read-through test ensures that your configuration correctly tracks values obtained from Riak KV and Riak Redis Add-on (RRA). The main actions of the test are:

* DELETE the Riak object at the `test` bucket with the key `foo`, which checks that there are no siblings.
* PUT a Riak object with the value 'bar' at the `test` bucket with the key `foo`.
* GET the Riak object at the `test` bucket with the key `foo`.
* GET the string-representation of the object from the cache proxy service using the key `test:foo`. (The cache proxy service should parse out the first portion of the Redis colon-separated key (namespace) to identify which Riak bucket to perform the backend read from.)
* Assert that the value obtained from the previous cache proxy GET is 'bar'.

First, create a file named`read_through_test.sh` with the following content:

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

Then, once you've created the file, run it as follows:

```bash
./read_through_test.sh 22122 8098 test
```

### Exceptions

If the test does not pass, verify that both Redis and RRA are running. You can do this by running:

```bash
ps aux |grep [r]edis
ps aux |grep [n]utcracker
```

The result should list `redis` and `nutcracker` respectively.

Also, verify that Riak KV is started and listening on the protocol buffer port specified:

```bash
sudo riak config effective |grep proto
```

If RRA is misconfigured, [reconfigure][redis add-on setup] it, and restart the service with the following:

```bash
sudo service cache_proxy restart
```

If RRA is configured correctly and all required services are running, you may want to restart each service from front to back as follows:

1. Stop RRA.
2. Stop Redis.
3. *Optional* Restart Riak KV (This should only be necessary if Riak KV is not responding to protocol buffer requests.)
4. Start Redis.
5. Start RRA.

```bash
sudo service cache_proxy stop
sudo service redis stop

# optional
sudo riak restart

sudo service redis start
sudo service cache_proxy start
```

## Using Riak Redis Add-on

Once you've successfully configured Riak Redis Add-on (RRA) and established a Riak KV and Redis client in the language of your choosing, you're ready to start using RRA.

For objects that should not be cached, interact with Riak KV as usual: issuing GET, PUT, and DELETE commands through the Riak client.

For objects that should be cached, read from RRA: issuing GET, SET, and DEL commands through the Redis client.

### Monitoring

#### RRA

Since RRA is installed as a service, the system service monitoring daemon will automatically restart a service with the correct configuration in the event that the service’s process was killed or terminated by other means.

The log file for RRA is stored by default in /var/log/cache_proxy.log . RRA is logrotate friendly, responding to the signal to reopen the log file following a rotate.

For additional monitoring, RRA provides statistics on service availability.  The statistics provided are generally useful in monitoring the health of the RRA service.

For example, running the following command (using the loopback interface and the default statistics port as an example):

```bash
telnet 127.0.0.1 22123
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

Using the above results, you should be able to determine metrics changes that would flag a change in service health. With this information you can implement monitoring to help guarantee the overall health of the cache proxy service in RRA and the custom software within your overall solution.

While we do not endorse a specific monitoring solution, the open interface to statistics allows you to use the monitoring solution of your choice. The following is a brief listing of compatible monitoring solutions:

* Custom - https://github.com/gfranxman/NutcrackerMonitor
* NewRelic - http://newrelic.com/plugins/schoology/245
* Nagios - https://github.com/schoology/twemproxy_nagios

#### Redis

Various Redis monitoring solutions exist in the market and, like monitoring RRA, these monitoring solutions make underlying calls to obtain Redis statistics, typically via the `info` command alone.

As with RRA, Redis statistics available on the Redis client port allow for monitoring via solutions such as the following:

* Custom - http://volumelabs.net/redis_monitoring/
* NewRelic - http://newrelic.com/plugins/poison-pen-llc/28
* Nagios - https://exchange.nagios.org/directory/Plugins/Databases/check_redis-2Epl/details
