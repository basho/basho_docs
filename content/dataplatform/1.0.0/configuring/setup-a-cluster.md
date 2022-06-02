---
title: "Setup a Basho Data Platform Cluster"
description: ""
project: "dataplatform"
project_version: "1.0.0"
menu:
  dataplatform-1.0.0:
    name: "Setup a Data Platform Cluster"
    identifier: "configuring_setup"
    weight: 100
    parent: "configuring"
toc: true
aliases:
  - /dataplatform/1.0.0/using-dataplatform/configuration/setup-a-cluster/
  - /dataplatform/latest/configuring/setup-a-cluster/
---

[bdp install]: {{<baseurl>}}dataplatform/1.0.0/installing/
[riak cluster setup]: {{<baseurl>}}riak/kv/2.1.3/using/running-a-cluster/
[riak configure]: {{<baseurl>}}riak/kv/2.1.3/configuring/
[riak_ensemble]: https://github.com/basho/riak_ensemble
[riak kv]: {{<baseurl>}}riak/kv/2.1.3/
[riak strong consistency]: {{<baseurl>}}riak/kv/2.1.3/using/reference/strong-consistency
[aws marketplace]: {{<baseurl>}}riak/kv/2.1.3/setup/installing/amazon-web-services/
[set spark ip]: {{<baseurl>}}dataplatform/1.0.0/configuring/spark-ip-address/
[default ports]: {{<baseurl>}}dataplatform/1.0.0/configuring/default-ports/

Now that you've [installed Basho Data Platform][bdp install], you're ready to set up a Basho Data Platform (BDP) cluster. This page will guide you through this process.

This page also lists the default port connections for BDP.

## Prerequisites

* We recommend running BDP on at least 5 nodes. Minimally, you will need 3 available, with BDP installed on all 3 nodes.
* You must have basic Riak configuration parameters, including listen interfaces (`listen.protobuf.internal` and `listen.http.internal`) and nodename, configured before you begin. You can view a guide to the process [here][riak configure].
* You need to have joined the nodes you wish to run Riak KV on together before you continue. Check out [Basic Cluster Setup][riak cluster setup]. (We strongly advise you not to join your Redis or Spark nodes to nodes that are part of your Riak KV cluster. Join them to your BDP cluster, instead.)
* This document assumes you have basic knowledge of configuring and running Riak KV cluster. If not, please read our [Riak KV documentation][riak kv].
* You must have root access on the nodes in your cluster.
* If you have a firewall, it should be configured to allow traffic for all network ports used by BDP. A list of default ports can be found [here][default ports].
* All of the steps in this guide assume you are working with your BDP nodes rather than any pre-existing Riak cluster.

> **Note on Amazon Web Services**
>
> AWS security profile must allow incoming and outgoing traffic from ip/ports used by Riak, Spark, and BDP. A list of default ports can be found [here][default ports]. Check out [Installing on Amazon Web Services][aws marketplace] for instructions on configuring security group settings to work with Riak.


<div class="warning">
  DO NOT join a BDP node to a pre-existing Riak cluster.
</div>

## Configure a BDP Cluster

1. First, start your BDP nodes.
2. Then, join your BDP nodes together.
3. Next, make sure riak_ensemble is running.
4. Then, configure the leader election service.
5. Next, if you're running a Spark cluster, set up your Spark cluster metadata.
6. After that, you'll need to confirm that Java is correctly configured.
7. Finally, add services to the nodes.
 
### Start Your BDP Nodes

First, start all the nodes you have installed BDP on:

```bash
user@machine1:~$ sudo riak start
user@machine2:~$ sudo riak start
user@machine3:~$ sudo riak start
```

### Join BDP Nodes


Then, join your BDP nodes together by running this command on all nodes in the cluster:

```bash
user@machine2:~$ sudo data-platform-admin join »NODENAME, ie riak@IPADDRESS)« 
```

### Check `riak_ensemble`

Once your BDP nodes are started and joined together, make sure [riak_ensemble][riak_ensemble] has started.

{{% note %}}
Basho Data Platform will not function correctly if `riak_ensemble` is not
running.
{{% /note %}}

To check the status of `riak_ensemble`:

```bash
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

### Configure The Leader Election Service (Enterprise Edition Only)

>The following section is for Basho Data Platform Enterprise Edition. If you perform these steps in an open source version the Riak KV node under the BDP node will not start.

Before enabling the leader election service, you must have enabled `riak_ensemble` (as directed in the previous step). If you skipped the previous step for any reason, please go back and do it now.

1. Locate and open `riak.conf`.
2. Find the line `## listener.leader_latch.internal = 127.0.0.1:5323`.
3. Uncomment the line and set to your node's IP and port. (**Note:** Any port will work as long as it matches what you set in the Spark master step below.)

{{% note %}}
The leader election service provides no authentication mechanism. We strongly
suggest that you use a network shielded from external connection attempts,
otherwise you run the risk of an attacker performing a Denial of Service
attack against your cluster.
{{% /note %}}

Add any additional interface/port pairs to listen on and change the '.internal' to whatever name helps you identify your interfaces. For instance:

```riakconf
listener.leader_latch.internal = 127.0.0.1:5323
listener.leader_latch.external = 10.10.1.2:15323
listener.leader_latch.testing = 192.168.0.42:12345
```

{{% note %}}
Changes to the `listener.leader_latch` setting will not have an impact on a
live running node. You must restart the node for changes to take effect.
{{% /note %}}


### Set Up Spark Cluster Metadata

{{% note %}}
Follow these steps ONLY if you are running a Spark cluster. Otherwise, skip to
Add Services.
{{% /note %}}

If you are running a Spark cluster, you need to connect it with BDP.

First, set up a consistent bucket called 'spark-bucket'  on your spark master node by running: 

```bash
sudo riak-admin bucket-type create strong '{"props":{"consistent":true}}'
```

Then enable the `map` bucket type by first running: 

```bash
sudo riak-admin bucket-type create maps '{"props":{"datatype":"map"}}'
```

Next activate the `map` bucket type by running:

```
sudo riak-admin bucket-type activate maps
```

Finally, verify that the `map` bucket type was successfully set up by running:

```bash
sudo riak-admin bucket-type status maps
```

A successful response should contain:

```
maps is active
datatype: map
active: true
```

### Confirm `JAVA_HOME` Is Set Correctly

If you are using Spark, you need to set `JAVA_HOME` for the 'riak' user  on all the spark-master and spark-worker nodes. To confirm that it’s set correctly, run:

```bash
sudo -u riak bash -c '$JAVA_HOME/bin/java -version'
```

The first line of output from that command should begin with `java version "1.8…"`. For example:

```
java version "1.8.0_45"
Java(TM) SE Runtime Environment (build 1.8.0_45-b14)
Java HotSpot(TM) 64-Bit Server VM (build 25.45-b02, mixed mode)
```

If you are using Ubuntu, you need to preserve your environment when running data platform commands as a super user. To do this, add `JAVA_HOME` and append `$JAVA_HOME/bin` to `PATH` in /etc/environment.

```bash
sudo vi /etc/sudoers
```
Add the following line: `Defaults        env_keep += "JAVA_HOME"`

Then restart Riak

```bash
sudo riak restart
```

### Add Services

You are ready to add services to your started, joined BDP nodes. There are several different types of services you can add to your BDP nodes:

* Spark master
* Spark worker
* Redis
* Cache proxy

#### Spark Master

>The Spark Master IP Address is selected automatically. If you need to set it manually, check out [Set Spark IP Address][set spark ip].

To register the service configuration for Spark master, issue the following command:

```bash
sudo data-platform-admin add-service-config my-spark-master spark-master LEAD_ELECT_SERVICE_HOSTS="»RIAK_IP_1:»LEADER_ELECTION_PORT«,»RIAK_IP_2«:»LEADER_ELECTION_PORT«" RIAK_HOSTS="»RIAK_IP_1«:»RIAK_PB_PORT«,»RIAK_IP_2«:»RIAK_PB_PORT«"
```

The IP addresses and ports you provide should be the IP addresses/ports of the 3 BDP nodes you started and joined earlier.

#### Spark Worker

To register the service configuration for Spark worker, issue the following command:

```bash
sudo data-platform-admin add-service-config my-spark-worker spark-worker MASTER_URL="spark://»SPARK_MASTER_IP«:»SPARK_MASTER_PORT«"
```
If you have multiple masters, list them after 'spark://' as a comma-separated list of hostname:port entries (i.e. MASTER_URL=”spark://my-host-1:7077,my-host-2:7077”).

#### Redis

To register the service configuration for Redis, issue the following command:

```bash
sudo data-platform-admin add-service-config my-redis redis HOST="0.0.0.0" REDIS_PORT="»REDIS_PORT«"
```

#### Cache Proxy

To register the service configuration for Cache Proxy, issue the following command:

```bash
sudo data-platform-admin add-service-config my-cache-proxy cache-proxy  HOST="0.0.0.0" CACHE_PROXY_PORT="»CACHE_PROXY_PORT«" CACHE_PROXY_STATS_PORT="»CACHE_PROXY_STATS_PORT«" CACHE_TTL="15s" RIAK_KV_SERVERS="»RIAK_IP_1«:»RIAK_PB_PORT«,»RIAK_IP_2«:»RIAK_PB_PORT«" REDIS_SERVERS="»REDIS_IP_1«:»REDIS_PORT«,»REDIS_IP_2«:»REDIS_PORT«"
```
The IP addresses you provide should be the IP addresses of the 3 BDP nodes you started and joined earlier.


## Configuration Defaults

Each service has one or more default ports if a port has not been specified when adding a service configuration.

For example, you can add a service configuration from the command line for Redis using a specified port:

```bash
sudo data-platform-admin add-service-config my-redis redis HOST="0.0.0.0" REDIS_PORT="»REDIS_PORT«"
```

Or you can use the default port by leaving out the `REDIS_PORT` parameter:

```
sudo data-platform-admin add-service-config my-redis redis HOST="0.0.0.0"
```

In the above example, the Redis service will use the default port 6379.

### Default Ports

>If you're using AWS, be sure to check out [Installing on Amazon Web Services][aws marketplace] for instructions on configuring security group settings to work with Riak.

If you have a firewall, it should be configured to allow traffic for all network ports used by BDP. A list of default ports can be found [here][default ports].

## In the Lab

A vagrant BDP setup was developed to automate the validation of BDP packages, see [basho-labs/vagrant-bdp-setup](https://github.com/basho-labs/vagrant-bdp-cluster/).
