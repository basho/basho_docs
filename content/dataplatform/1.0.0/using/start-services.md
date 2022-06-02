---
title: "Starting Services on Basho Data Platform"
description: ""
project: "dataplatform"
project_version: "1.0.0"
menu:
  dataplatform-1.0.0:
    name: "Start Services"
    identifier: "using_bdp"
    weight: 100
    parent: "using"
toc: true
aliases:
  - /dataplatform/1.0.0/using-dataplatform/using-bdp/
  - /dataplatform/latest/using/start-services/
---

[bdp configure]: {{<baseurl>}}dataplatform/1.0.0/configuring/setup-a-cluster/
[bdp configure add services]: {{<baseurl>}}dataplatform/1.0.0/configuring/setup-a-cluster/#add-services
[bdp install]: {{<baseurl>}}dataplatform/1.0.0/installing/
[bdp reference]: {{<baseurl>}}dataplatform/1.0.0/learn/service-manager/


You've [installed][bdp install] Basho Data Platform (BDP), [configured][bdp configure] your cluster, and [added services][bdp configure add services] to your nodes. The setup of your BDP cluster is complete! Now you can begin using your BDP cluster. 

## Start Services

The very first thing you can do with your BDP cluster is start the services you added. In the last section of the [configuration instructions][bdp configure add services], you added the following services:

* Spark-master
* Spark-worker
* Redis
* Cache proxy

### Spark-Master Service

To start the spark-master service, run the following, using the name and IP of the node you wish to start the service on:

```bash
sudo data-platform-admin start-service »NODENAME«@»IPADDRESS« my-spark-group my-spark-master
```

Then verify that the spark-master service is running:

```bash
ps -ef | grep [s]park-master
```

You should see the output like this:

```
riak     23418     1  7 20:32 ?        00:00:13 /usr/local/jdk/bin/java -cp /usr/lib/riak/lib/data_platform-1/priv/spark-master/sbin/../conf/:/usr/lib/riak/lib/data_platform-1/priv/spark-master/lib/spark-assembly-1.4.0-hadoop2.6.0.jar:/usr/lib/riak/lib/data_platform-1/priv/spark-master/lib/datanucleus-api-jdo-3.2.6.jar:/usr/lib/riak/lib/data_platform-1/priv/spark-master/lib/datanucleus-rdbms-3.2.9.jar:/usr/lib/riak/lib/data_platform-1/priv/spark-master/lib/datanucleus-core-3.2.10.jar -Dspark.deploy.recoveryMode=CUSTOM -Dspark.deploy.recoveryMode.factory=org.apache.spark.deploy.master.RiakEnsembleRecoveryModeFactory -Dspark.deploy.leadelect.namespace=spark_leaders -Dspark.deploy.leadelect.service=localhost:10012 -Dspark.deploy.riak.metadata.map=spark-cluster-map -Dspark.deploy.riak.consistent.bucket=spark-bucket -Dspark.deploy.leadelect.timeout=10000 -Dspark.riak.connection.host=localhost:10017 -cp /usr/lib/riak/lib/data_platform-1/priv/spark-master/sbin/../lib/* -Xms512m -Xmx512m org.apache.spark.deploy.master.Master --ip 172.28.128.3 --port 7077 --webui-port 8080
```

If you need to stop the spark-master, the proper sequence is:

```bash
sudo data-platform-admin stop-service »NODENAME«@»IPADDRESS« my-spark-group my-spark-master
```

### Spark-Worker Service

To start the spark-worker service, run the following, using the name and IP of the node you wish to start the service on:

```bash
sudo data-platform-admin start-service »NODENAME«@»IPADDRESS« my-spark-group my-spark-worker
```
You can repeat this command (using a different nodename and IP address) to add additional spark-worker services.

To stop a spark-worker service, you need to stop the service first:

```bash
sudo data-platform-admin stop-service »NODENAME«@»IPADDRESS« my-spark-group my-spark-worker
```

### Redis Service

To start the Redis service, run the following, using the name and IP of the node you wish to start the service on:

```bash
sudo data-platform-admin start-service »NODENAME«@»IPADDRESS« my-redis-group my-redis
```

### Cache Proxy Service

To start the cache proxy service, run: 

```bash
sudo data-platform-admin start-service »NODENAME«@»IPADDRESS« my-cache-proxy-group my-cache-proxy
```

## Verify The Services Are Running Correctly

### Redis and Cache Proxy

Do you use CentOS or Ubuntu?

#### CentOS

Set the base path of BDP:

```bash
export BDP_HOME=/usr/lib64/riak/lib/data_platform-1
```

#### Ubuntu

Set the base path of BDP:

```bash
export BDP_HOME=/usr/lib/riak/lib/data_platform-1
```

#### Verification

First, add the Redis CLI utility to your `PATH` and specify Ips and ports for the services involved in the test:

```bash
export PATH="$PATH:$BDP_HOME/priv/redis/bin"
export RIAK_IP=»RIAK_IP
export RIAK_HTTP_PORT=»RIAK_HTTP_PORT«
export CACHE_PROXY_IP=»CACHE_PROXY_IP«
export CACHE_PROXY_PORT=»CACHE_PROXY_PORT«
export REDIS_IP=»REDIS_IP«
export REDIS_PORT=»REDIS_PORT«
```

Then, add some test data to your Riak cluster, using the IP:HttpPort of your Riak KV node:

```bash
curl -s -X PUT -d "my-value" "http://$RIAK_IP:$RIAK_HTTP_PORT/buckets/my-bucket/keys/my-key"
```

Retrieve the test data via cache proxy, using the IP address you set for cache proxy:

```bash
redis-cli -h $CACHE_PROXY_IP -p $CACHE_PROXY_PORT get my-bucket:my-key
```

Finally, confirm that the data is now also in Redis:

```bash
redis-cli $REDIS_IP -p $REDIS_PORT get my-bucket:my-key
```
