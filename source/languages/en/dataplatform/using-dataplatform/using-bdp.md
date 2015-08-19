---
title: Using your Basho Data Platform Cluster
project: data-platform
version: 1.0.0+
document: guide
toc: true
index: true
audience: beginner 
---


[bdp configure]: LINK 
[bdp configure add services]: LINK
[bdp install]: ./dataplatform/installing.html
[bdp reference]: LINK


You've [installed][bdp install] Basho Data Platform (BDP), [configured][bdp configure] your cluster, and [added services][bdp configure add services] to your nodes. The setup of your BDP cluster is complete! Now you can begin using your BDP cluster. 

##Start Services


The very first thing you can do with your BDP cluster is start the services you added. In the last section of the [configuration instructions][bdp configure add services], you added the following services:

* [Spark-master](#spark-master-service)
* [Spark-worker](#spark-worker-service)
* [Redis](#redis-service)
* [Cache proxy](#cache-proxy-service)

###Spark-Master Service


To start the spark-master service, run the following, using the name and IP of the node you wish to start the service on:

```shell
data-platform-admin start-service »NODENAME«@»IPADDRESS« my-spark-group my-spark-master
```

Then verify that the spark-master service is running:

```shell
$ ps -ef | grep [s]park-master
```

You should see the output like this:

```
riak     23418     1  7 20:32 ?        00:00:13 /usr/local/jdk/bin/java -cp /usr/lib/riak/lib/data_platform-1/priv/spark-master/sbin/../conf/:/usr/lib/riak/lib/data_platform-1/priv/spark-master/lib/spark-assembly-1.4.0-hadoop2.6.0.jar:/usr/lib/riak/lib/data_platform-1/priv/spark-master/lib/datanucleus-api-jdo-3.2.6.jar:/usr/lib/riak/lib/data_platform-1/priv/spark-master/lib/datanucleus-rdbms-3.2.9.jar:/usr/lib/riak/lib/data_platform-1/priv/spark-master/lib/datanucleus-core-3.2.10.jar -Dspark.deploy.recoveryMode=CUSTOM -Dspark.deploy.recoveryMode.factory=org.apache.spark.deploy.master.RiakEnsembleRecoveryModeFactory -Dspark.deploy.leadelect.namespace=spark_leaders -Dspark.deploy.leadelect.service=localhost:10012 -Dspark.deploy.riak.metadata.map=spark-cluster-map -Dspark.deploy.riak.consistent.bucket=spark-bucket -Dspark.deploy.leadelect.timeout=10000 -Dspark.riak.connection.host=localhost:10017 -cp /usr/lib/riak/lib/data_platform-1/priv/spark-master/sbin/../lib/* -Xms512m -Xmx512m org.apache.spark.deploy.master.Master --ip 172.28.128.3 --port 7077 --webui-port 8080
```

If you need to stop the spark-master, the proper sequence is:

```shell
$ data-platform-admin stop-service »NODENAME«@»IPADDRESS« my-spark-group my-spark-master
```

###Spark-Worker Service


To start the spark-worker service, run the following, using the name and IP of the node you wish to start the service on:

```shell
data-platform-admin start-service »NODENAME«@»IPADDRESS« my-spark-group my-spark-worker
```
You can repeat this command (using a different nodename and IP address) to add additional spark-worker services.

To stop a spark-worker service, you need to stop the service first:

```shell
$ data-platform-admin stop-service »NODENAME«@»IPADDRESS« my-spark-group my-spark-worker
```

###Redis Service

To start the Redis service, run the following, using the name and IP of the node you wish to start the service on:

```shell
$ data-platform-admin start-service »NODENAME«@»IPADDRESS« my-redis-group my-redis
```

###Cache Proxy Service


To start the cache proxy service, run: 

```shell
$ data-platform-admin start-service »NODENAME«@»IPADDRESS« my-cache-proxy-group my-cache-proxy
```

##Verify The Services Are Running Correctly

###Redis and Cache Proxy

Do you use [CentOS](#centos) or [Ubuntu](#ubuntu)?

####CentOS

First, add the Redis CLI utility to your `PATH`:

```shell
$ export PATH=$PATH:/usr/lib64/riak/lib/data_platform-1/priv/redis/bin
```

Then, add some test data to your Riak cluster, using the IP:Port of your Riak KV node:

```shell
$ curl -s -X PUT -d "my-value" "http://172.28.128.3:8098/buckets/my-bucket/keys/my-key"
```

Retrieve the test data via cache proxy, using the IP address you set for cache proxy:

```shell
$ redis-cli -h 172.28.128.3 -p 11211 get my-bucket:my-key
```

Finally, confirm that the data is now also in Redis:

```shell
$ redis-cli -h 172.28.128.3 -p 6379 get my-bucket:my-key
```

####Ubuntu

First, install cURL to use an HTTP client for Riak KV:

```shell
$ sudo apt-get install curl
```

Then add the Redis CLI utility to your `PATH`:

```shell
$ export PATH=$PATH:/usr//riak/lib/data_platform-1/priv/redis/bin
```

Then, add some test data to your Riak cluster:

```shell
$ curl -s -X PUT -d "my-value" "http://172.28.128.3:8098/buckets/my-bucket/keys/my-key"
```

Retrieve the test data via cache proxy:

```shell
$ redis-cli -h 172.28.128.3 -p 11211 get my-bucket:my-key
```

Finally, confirm that the data is now also in Redis:

```shell
$ redis-cli -h 172.28.128.3 -p 6379 get my-bucket:my-key
```
