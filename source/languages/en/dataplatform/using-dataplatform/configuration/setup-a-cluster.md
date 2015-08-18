---
title: Set Up a Basho Data Platform Cluster
project: dataplatform
version: 1.0.0+
document: guide
toc: true
index: true
audience: beginner
---

[bdp install]: LINK
[riak cluster setup]: http://docs.basho.com/riak/2.1.1/ops/building/basic-cluster-setup/
[riak configure]: http://docs.basho.com/riak/2.1.1/ops/building/basic-cluster-setup/
[riak_ensemble]: https://github.com/basho/riak_ensemble
[riak kv]: http://docs.basho.com/riak/2.1.1
[riak strong consistency]: http://docs.basho.com/riak/2.1.1/ops/advanced/strong-consistency/#Enabling-Strong-Consistency

Now that you've [installed Basho Data Platform][bdp install], you're ready to set up a Basho Data Platform (BDP) cluster. This page will guide you through this process.

This page also lists the [default port connections for BDP](#configuration-defaults).

##Prerequisites

* We recommend running BDP on at least 5 nodes. Minimally, you will need 3 available, with BDP installed on all 3 nodes.
* You must have basic Riak configuration parameters, including listen interfaces (`listen.protobuf.internal` and `listen.http.internal`) and nodename, configured before you begin. You can view a guide to the process [here][riak configure].
* You need to have joined the nodes you wish to run Riak Kv on together before you continue. Check out [Basic Cluster Setup][riak cluster setup].
* This document assumes you have basic knowledge of configuring and running Riak KV cluster. If not, please read our [Riak KV documentation][riak kv].
* You must have root access on the nodes in your cluster.
* If you have a firewall, it should be configured to allow traffic for all network ports used by BDP.
* All of the steps in this guide assume you are working with your BDP nodes rather than any pre-existing Riak cluster.

<div class="note">
<div class="title">Note on Amazon Web Services</div>
AWS security profile must allow incoming and outgoing traffic from ip/ports used by Riak, Spark, and BDP.  A [list of default ports](LINK) is at the end of this document. 
</div>

<div class="warning">
  DO NOT join a BDP node to a pre-existing Riak cluster.
</div>

##Configure a BDP Cluster

1. First, [start your BDP nodes](#start-your-bdp-nodes).
2. Then, [join your BDP nodes together](#join-bdp-nodes).
3. Next, [make sure riak_ensemble is running](#start-riakensemble).
4. Then, [configure the leader election service](#configure-the-leader-election-service)
5. Next, if you're running a Spark cluster, [set up your Spark cluster metadata](#set-up-spark-cluster-metadata).
6. After that, you'll need to [confirm that Java is correctly configured](#confirm-javahome-is-set-correctly).
7. Finally, [add services to the nodes](#add-services).
 
###Start Your BDP Nodes

First, start all the nodes you have installed BDP on:

```shell
user@machine1:~$ sudo riak start
user@machine2:~$ sudo riak start
user@machine3:~$ sudo riak start
```

###Join BDP Nodes


Then, join your BDP nodes together by running this command on all nodes in the cluster:

```shell
user@machine2:~$ sudo data-platform-admin join .»NODENAME (riak@IPADDRESS)« 
```

### Start `riak_ensemble`

Once your BDP nodes are started and joined together, you need to set up [riak_ensemble][riak_ensemble].

<div class="note">
Basho Data Platform will not function correctly if `riak_ensemble` is not running. 
</div>

<div class="note">
We do not recommend enabling `riak_ensemble` on more than 5 nodes, as performance will degrade.
</div>

To make sure `riak_ensemble` has started, you will need to enter the Riak console and run the enable command. To do this run:

 ```shell
sudo riak attach
riak_ensemble_manager:enable().
```

You will then need to Ctrl-C twice to exit the Erlang shell `riak attach` brought you into. Once out, run:

```shell
sudo riak-admin ensemble-status
```

If `riak_ensemble` is enabled, you will see the following output :

```
  ============================== Consensus System ==========================
  Enabled:     true
  Active:      true
  Ring Ready:  true
  Validation:  strong (trusted majority required)
  Metadata:    best-effort replication (asynchronous)
```

For more information on why this is important, please see our [strong consistency docs][riak strong consistency].

###Configure The Leader Election Service

Before enabling the leader election service, you must have enabled `riak_ensemble` (as directed in the [previous step](#start-riakensemble)). If you skipped the previous step for any reason, please go back and do it now.

1. Locate and open `riak.conf`.
2. Find the line `## listener.leader_latch.internal = 127.0.0.1:5323`.
3. Uncomment the line and set to your node's IP and port. (**Note:** Any port will work as long as it matches what you set in the [Spark master step](#spark-master) below.)

<div class="note">
The leader election service provides no authentication mechanism. We strongly suggest that you use a network shielded from external connection attempts, otherwise you run the risk of an attacker performing a Denial of Service attack against your cluster.
</div>

Add any additional interface/port pairs to listen on and change the '.internal' to whatever name helps you identify your interfaces. For instance:

```riak.conf
listener.leader_latch.internal = 127.0.0.1:5323
listener.leader_latch.external = 10.10.1.2:15323
listener.leader_latch.testing = 192.168.0.42:12345
```

<div class="note">
Changes to the `listener.leader_latch` setting will not have an impact on a live running node. You must restart the node for changes to take effect.
</div>


###Set Up Spark Cluster Metadata

<div class="note">
Follow these steps ONLY if you are running a Spark cluster. Otherwise, skip to [Add Services](#add-services).
</div>

If you are running a Spark cluster, you need to connect it with BDP.

First, set up a consistent bucket called 'spark-bucket'  on your spark master node by running: 

```shell
riak-admin bucket-type create strong '{"props":{"consistent":true}}'
```

Then enable the `map` bucket type by first running: 

```shell
riak-admin bucket-type create maps '{"props":{"datatype":"map"}}'
```

Next activate the `map` bucket type by running:

```
riak-admin bucket-type activate maps
```

Finally, verify that the `map` bucket type was successfully set up by running:

```shell
riak-admin bucket-type status maps
```

A successful response should contain:

```
maps is active
datatype: map
active: true
```

###Confirm `JAVA_HOME` Is Set Correctly

If you are using Spark, you need to set `JAVA_HOME` for the 'riak' user  on all the spark-master and spark-worker nodes. To confirm that it’s set correctly, run:

```shell
$ sudo su - riak
$ {JAVA_HOME}/bin/java -version
```

The first line of output from that command should begin with `java version "1.8…"`. For example:

```
${JAVA_HOME}/bin/java -version
java version "1.8.0_45"
Java(TM) SE Runtime Environment (build 1.8.0_45-b14)
Java HotSpot(TM) 64-Bit Server VM (build 25.45-b02, mixed mode)
```

If you are using Ubuntu, you need to preserve your environment when running data platform commands as a super user. To do this, add `JAVA_HOME` and append `$JAVA_HOME/bin` to `PATH` in /etc/environment.

```shell
$ sudo vi /etc/sudoers
```
Add the following line: `Defaults        env_keep += "JAVA_HOME"`

Then restart Riak

```shell
$ sudo riak restart
```

###Add Services

You are ready to add services to your started, joined BDP nodes. There are several different types of services you can add to your BDP nodes:

* [Spark master](#spark-master)
* [Spark worker](#spark-worker)
* [Redis](#redis)
* [Cache proxy](#cache-proxy)

####Spark Master

First, specify the IP address your Spark instance binds to:

```shell
sudo bash -c "echo "SPARK_MASTER_IP=<YOUR PUBLIC IP" >> /<YOUR_PATH_TO BDP>/priv/spark-master/conf/spark-env.sh"
```

Then add a Spark master service to a node:

```shell
$ data-platform-admin add-service-config my-spark-master spark-master LEAD_ELECT_SERVICE_HOSTS="»IPADDRESS«:»PORT«,»IPADDRESS2«:»PORT2«,»IPADDRESS3«:»PORT3«" RIAK_HOSTS="»IPADDRESS«:»PORT«,»IPADDRESS2«:»PORT2«,»IPADDRESS3«:»PORT3«"
```

The IP addresses and ports you provide should be the IP addresses/ports of the 3 BDP nodes you started and joined earlier.

####Spark Worker

To add a Spark worker service to a node:

```shell
data-platform-admin add-service-config my-spark-worker spark-worker MASTER_URL="spark://»HOSTNAME«:7077"
```
If you have multiple masters, list them after 'spark://' as a comma-separated list of hostname:port entries (i.e. MASTER_URL=”spark://my-host-1:7077,my-host-2:7077”).

The hostnames you enter must match what you set as `SPARK_MASTER_IP` in priv/spark-master/conf/spark-env.sh.

####Redis

To add a Redis service to a node:

```shell
data-platform-admin add-service-config my-redis redis HOST="0.0.0.0" REDIS_PORT="6379"
```

####Cache Proxy

To add a cache proxy service to a node:

```shell
data-platform-admin add-service-config my-cache-proxy cache-proxy  HOST="0.0.0.0" CACHE_PROXY_PORT="11211" CACHE_PROXY_STATS_PORT="22123" CACHE_TTL="15s" RIAK_KV_SERVERS="»IPADDRESS«:»PROTOCOLBUFFERSPORT« »IPADDRESS2«:»PROTOCOLBUFFERSPORT« »IPADDRESS3«:»PROTOCOLBUFFERSPORT«" REDIS_SERVERS="»IPADDRESS«:6379 »IPADDRESS2«:6379 »IPADDRESS3«:6379"
```
The IP addresses you provide should be the IP addresses of the 3 BDP nodes you started and joined earlier.


##Configuration Defaults


Each service has one or more default ports if a port has not been specified when [adding a service configuration](#add-services).

For example, you can add a service configuration from the command line for Redis using a specified port:

```
data-platform-admin add-service-config my-redis redis HOST="10.0.0.1" REDIS_PORT="9999"
```

Or you can use the default port by leaving out the `REDIS_PORT` parameter:

```
data-platform-admin add-service-config my-redis redis HOST="10.0.0.1"
```

In the above example, the Redis service will use the default port 6379.

### Default Ports

| Service | Parameter | Default |
| ---------- | -------------- | ---------- |
| Redis | `REDIS_PORT` | 6379 |
| Cache-Proxy | `CACHE_PROXY_PORT` | 22122 |
| Cache-Proxy | `CACHE_PROXY_STATS_PORT` | 22123 |
| Spark | `SPARK_MASTER_PORT` | 7077 |
| Spark | `SPARK_MASTER_WEBUI_PORT` | 8080 |
| Spark | `SPARK_WORKER_PORT` | 7078 |
| Spark | `SPARK_WORKER_WEBUI_PORT` | 8081 |
