---
title: "Replacing Spark Cluster Manager with the Basho Data Platform Cluster Manager"
description: ""
project: "dataplatform"
project_version: "1.0.0"
menu:
  dataplatform-1.0.0:
    name: "Replacing Spark Cluster Manager"
    identifier: "configuring_replace_spark"
    weight: 101
    parent: "configuring"
toc: true
aliases:
  - /dataplatform/1.0.0/using-dataplatform/configuration/replace-spark-cluster-manager/
  - /dataplatform/latest/configuring/replace-spark-cluster-manager/
---

[bdp install]: {{<baseurl>}}dataplatform/1.0.0/installing/
[bdp configure]: {{<baseurl>}}dataplatform/1.0.0/configuring/setup-a-cluster/
[bdp configure spark master]: {{<baseurl>}}dataplatform/1.0.0/configuring/setup-a-cluster/#set-up-spark-cluster-metadata
[ee]: http://info.basho.com/Wiki_Riak_Enterprise_Request.html
[riak data types]: {{<baseurl>}}riak/kv/2.1.3/developing/data-types/


> The Basho Data Platform cluster manager is available to [Enterprise users only][ee].

You can simplify your operations by using the Basho Data Platform (BDP) cluster manager instead of Apache Zookeeper to manage your Spark cluster. This document will walk you through the steps.

Apache Zookeeper is an open-source application that provides a consistent distributed storage for client data as well as a set of synchronization primitives. BDP provides all the necessary functionality required for Spark Master high availability without the need to maintain and manage another software system (Zookeeper), creating a simple, centrally-managed, robust solution.

## Prerequisites

Before you replace Spark Standalone Cluster Manager with the one provided by BDP you must:

* Have the latest BDP build [installed][bdp install] and [configured][bdp configure]. This includes having `riak_ensemble` enabled and activated on at least 3 nodes, as well as having the leader election service configured and running.
* Have CRDT enabled and activated as a bucket type. See how to do that [here][riak data types].
* Have an operational Spark cluster (version 1.4.0+). 
* Have the following information available:
  * IP addresses of Riak cluster nodes and protobuf ports. They will look like: 172.31.9.125:10017, 172.31.9.126:10017, 172.31.9.127:10017.
  * IP addresses and ports of LES service. They will look  like: 172.31.9.125:10012, 172.31.9.126:10012, 172.31.9.127:10012.
  * Name of the group for leader election. (It is set to `spark_leaders` by default.)
  * Name of consistent bucket for Spark metadata storage (you set this up during [configuration][bdp configure]).
  * Name of CRDT map for metadata storage (i.e. `spark-cluster-map`). The CRDT map stores serialized classes containing Workers, Applications and Jobs data. This data is stored in the map as a key-value pair with key composed of a prefix of entity type plus unique id (i.e. `app_42343sdfd33`), and value containing serialized (by serializer passed in by spark-master) class for Worker, Application or Job.
* You also will want to think about how many spark-workers you will want to establish. A node can effectively serve as either a spark-master or a spark-worker. A spark-master node can and will do work. Spark-worker nodes are helpful when there are enough spark-master nodes to delegate work so some nodes can be dedicated to only doing work, a.k.a. spark-worker nodes.

## Enabling BDP Spark Cluster Manager

To replace your Spark Cluster Manager with the BDP cluster manager, you will do the following:

1. Add BDP service configs for spark-master and spark-worker.
2. Activate the services.
3. Verify the services are running.

### Add BDP Service Configs

1. On any node in your BDP cluster, run:


```bash
sudo data-platform-admin add-service-config my-spark-master spark-master /
LEAD_ELECT_SERVICE_HOSTS="»IP:PORTS from `listener.leader_latch.internal` in riak.conf«" /
RIAK_HOSTS="»IP:PORTS from `listener.protobuf.internal` in riak.conf«"
```

   Once this command is run, all nodes in the BDP cluster can start spark-master via the BDP Service Manager.

1. Then, on any node in your BDP cluster, run:

```bash
sudo data-platform-admin add-service-config my-spark-worker spark-worker MASTER_URL="spark://»PUBLICIPOFMASTERNODE«:»DEFAULTSPARKMASTERPORT«"
```

### Activate The Services

1. On any node in your BDP cluster, run: 


```bash
sudo data-platform-admin start-service riak@»PUBLICIPOFMASTERNODE« my-spark-group my-spark-master
```
   where the IP address is the address passed to [Spark master service config][bdp configure spark master].  This will start the Spark master on that machine.

1. You can verify that the previous step was successful by running the following on the manager node:


```bash
ps -ef | grep [s]park-master
```

A successful start of the spark-master service should cause an output like the following (with the IP address and port number of the spark-master you specified): `master.Master --ip 172.28.128.3 --port 7077 --webui-port 8080`.
   
> Note: If you see a hostname rather than an IP address OR if this is your first time starting the manager service, you must:
> 
> 1. Stop the service: `sudo data-platform-admin stop-service riak@»PUBLICIPOFMASTERNODE« my-spark-group my-spark-master`
> 2. Kill the process: `sudo pkill -f deploy.master.Master`
> 3. And then restart the service: `sudo data-platform-admin start-service riak@»PUBLICIPOFMASTERNODE« my-spark-group my-spark-master`.

1. Once your spark-master service has been started successfully, you should activate your spark-worker services. Do this by running the following command from any node in your BDP cluster:


```bash
data-platform-admin start-service riak@»PUBLICIPOFWORKERNODE« my-spark-group my-spark-worker
```

   To add multiple spark-worker services, run this command for each spark-worker you'd like to add.

1. You can verify that the previous step was successful by running the following on the manager node:

```bash
ps -ef | grep [s]park-worker
```

A successful activation should cause an output like the following (with the IP address and port number of the spark-master service you specified): `worker.Worker --webui-port 8081 spark://172.28.128.3:7077`.

{{% note %}}
If you see a hostname rather than an IP address OR if this is your first time
starting the worker service, you must:

1. Stop the service: `sudo data-platform-admin stop-service
   riak@»PUBLICIPOFWORKERNODE« my-spark-group my-spark-worker`

2. Kill the process: `sudo pkill -f deploy.worker.Worker`

3. And then restart the service: `sudo data-platform-admin start-service
   riak@»PUBLICIPOFWORKERNODE« my-spark-group my-spark-worker`.
{{% /note %}}

### Verify The Services Are Running
At this point, your BDP manager and Spark cluster should be ready to go! Here are some ways to verify that your Spark cluster is connected to the BDP manager and running correctly. 

1. For a readout of your running and available services, run 

```bash
sudo data-platform-admin services
```

You should see something like the following, with the IP addresses you input:

```
Running Services:
+--------------+---------------+-----------------+
|    Group     |    Service    |      Node       |
+--------------+---------------+-----------------+
|my-spark-group|my-spark-master|riak@172.28.128.3|
|my-spark-group|my-spark-worker|riak@172.28.128.4|
+--------------+---------------+-----------------+

Available Services:
+---------------+------------+
|    Service    |    Type    |
+---------------+------------+
|my-spark-master|spark-master|
|my-spark-worker|spark-worker|
+---------------+------------+
```

2. To see how many workers are successfully joined with the manager, run:


```bash
curl »SPARK_MASTER_IP«:»SPARK_MASTER_HTTP_PORT«
cat index.html
```
You will see an output like: `<li><strong>Workers:</strong> 1</li>`. The '1' indicates that one worker node has been joined with the manager node.

You can clean up the index.html file when you are done by running: `rm index.html`.

3. To verify that the spark-worker is operational, run:


```bash
curl »SPARK_WORKER_IP«:»SPARK_WORKER_HTTP_PORT«
grep index.html
```
You will see an output like:  `<li><strong>Master URL:</strong> spark://172.28.128.3:7077</li>`. The IP address displayed should be the IP of the spark-master service.

You can clean up the index.html file when you are done by running: `rm index.html`.

4. You can also test if the cluster works by performing a job. From the command-line, within the spark-worker path under the BDP priv path, run the following:

```bash
./bin/spark-shell --master spark://»SPARK_MASTER_IP«:»SPARK_MASTER_PORT«
```

Make sure the README.md is in pwd. Then enter:

```scala
val textFile = sc.textFile("README.md")
textFile.count()
exit()
```

You should have exited the prompt. Then run:

```bash
wget »SPARK_MASTER_IP«:»SPARK_MASTER_HTTP_PORT«
cat index.html
```

You should see results like the following:

```
<li><strong>Applications:</strong>
    0 Running,
    1 Completed </li>
```
