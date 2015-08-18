---
title: Set Up a Basho Data Platform Cluster on AWS
project: dataplatform
version: 1.0.0+
document: guide
toc: true
index: true
audience: beginner
---

[bdp install aws]: LINK
[riak configure]: http://docs.basho.com/riak/2.1.1/ops/building/basic-cluster-setup/
[riak_ensemble]: https://github.com/basho/riak_ensemble
[riak strong consistency]: http://docs.basho.com/riak/2.1.1/ops/advanced/strong-consistency/#Enabling-Strong-Consistency

Now that you've [installed Basho Data Platform on AWS][bdp install aws], you're ready to set up a Basho Data Platform (BDP) cluster. This page will guide you through this process.

<div class="note">
AWS security profile must allow incoming and outgoing traffic from ip/ports used by Riak, Spark, and BDP.  A [list of default ports](LINK) is at the end of this document. 
</div>

##Prerequisites

* We recommend running BDP on at least 5 nodes. Minimally, you will need 3 available, with BDP installed on all 3 nodes.
* You must have basic Riak configuration parameters, including listen interfaces (`listen.protobuf.internal` and `listen.http.internal`) and nodename, configured before you begin. You can view a guide to the process [here][riak configure].
* You must have root access on the nodes in your cluster.
* AWS security profile should be configured to allow traffic for all network ports used by BDP.
* All of the steps in this guide assume you are working with your BDP nodes rather than any pre-existing Riak cluster.

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



run this on spark worker nodes 

sudo riak-admin cluster join riak@SPARKMASTERIP

run this on spark master node

sudo riak-admin cluster plan
sudo riak-admin cluster commit
sudo riak-admin cluster status

run this on spark master node

sudo riak-admin bucket-type create strong '{"props":{"consistent":true}}'
sudo riak-admin bucket-type create maps '{"props":{"datatype":"map"}}'
sudo riak-admin bucket-type activate maps
sudo riak-admin bucket-type status maps

run this on all nodes

sudo riak stop
sudo riak start

wait for ensemble to start up
check ensemble by running this on any node

sudo riak-admin ensemble-status

should see this followed by a list of partitions and ip address of nodes in the cluster

  ============================== Consensus System ==========================
  Enabled:     true
  Active:      true
  Ring Ready:  true
  Validation:  strong (trusted majority required)
  Metadata:    best-effort replication (asynchronous)
================================== Ensembles ==================================
 Ensemble     Quorum        Nodes      Leader
-------------------------------------------------------------------------------
   root       4 / 4         4 / 4      riak@172.28.128.3
    2         3 / 3         3 / 3      riak@172.28.128.4
    3         3 / 3         3 / 3      riak@172.28.128.5

run this on spark master node
where ip:port correspond to all nodes in the cluster separated by ,

sudo data-platform-admin add-service-config my-spark-master spark-master -f \
HOST='SPARKMASTERIP' \
LEAD_ELECT_SERVICE_HOSTS='IP:PORT,IP:PORT...’ \
RIAK_HOSTS='IP:PORT,IP:PORT...'

data-platform-admin add-service-config my-spark-worker spark-worker MASTER_URL="spark://SPARKMASTERIP:7077" SPARK_WORKER_PORT=8081

now start and stop all services once by doing the following

on spark master node run 

sudo data-platform-admin start-service riak@SPARKMASTERIP my-spark-group my-spark-master 

for each spark worker node run this on master

sudo data-platform-admin start-service riak@SPARKWORKERIP my-spark-group my-spark-worker

Now stop the service 

run on each spark worker node with service started

sudo data-platform-admin stop-service riak@SPARKWORKERIP my-spark-group my-spark-worker

sudo pkill -f spark-worker 

on spark master node run

sudo data-platform-admin stop-service riak@MASTERIP my-spark-group my-spark-master 
sudo pkill -f spark-master 

now you can start up spark master node and spark worker node and have them communicate properly

run this on spark master node

sudo data-platform-admin start-service riak@SPARKMASTERIP my-spark-group my-spark-master

run this on any node

sudo data-platform-admin start-service riak@SPARKWORKERIP my-spark-group my-spark-worker 

check that they are communicating with wget sparkmasterip:8080 then nano index.html

Test Spark:

run this from any node in the cluster.  The ip address is that of the spark master

/usr/lib/riak/lib/data_platform-1/priv/spark-master/bin/spark-submit \
--master spark://172.31.6.239:7077 \
/usr/lib/riak/lib/data_platform-1/priv/spark-master/examples/src/main/python/pi.py 100

wget 172.28.128.3:8080
nano index.html

should see

<li><strong>Applications:</strong>
    0 Running,
    1 Completed </li>



### Default Ports

Riak
EPMD listener = 4369/tcp
PBC (Protocol Buffers Client) Listener = 8087/tcp
HTTP Listener = 8098/tcp
Riak Handoff port = 8099/tcp
Solr = 8093
Solr JMX = 8985
Leader Latch = 5323
Cluster Manager = 9080
Riak EE JMX = 41110 (Only for Enterprise Edition)

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
