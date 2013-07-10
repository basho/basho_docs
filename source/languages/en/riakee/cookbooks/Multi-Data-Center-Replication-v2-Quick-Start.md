---
title: "Multi Data Center Replication: Quick Start (v2)"
project: riakee
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, bnw]
---

The Riak Multi Data Center Replication Quick Start will walk through the process of configuring Riak's version 2 Replication to perform replication between two sample Riak clusters in separate networks.  This guide will also cover bidirectional replication, which is accomplished by setting up unidirectional replication in both directions between the clusters.

### Prerequisites
This Guide assumes that you have completed the following steps:

* Install [[Riak Enterprise]]
* Perform [[System Turning|Linux Performance Tuning]]
* Reviewed [[Configuration|Multi Data Center Replication Configuration]]

### Scenario
Configure Riak MDC to perform replication, given the following two (2) three-node Riak Enterprise Clusters: 

#### Cluster 1
name  | ip          | nodename
------|-------------|-----------------
node1 | 172.16.1.11 | riak@172.16.1.11
node2 | 172.16.1.12 | riak@172.16.1.12
node3 | 172.16.1.13 | riak@172.16.1.13

#### Cluster 2
name  | ip          | nodename
------|-------------|-----------------
node4 | 192.168.1.21 | riak@192.168.1.21
node5 | 192.168.1.22 | riak@192.168.1.22
node6 | 192.168.1.23 | riak@192.168.1.23

> The addresses used in the example clusters are contrived, non-routable addresses; however, in real world applications these addresses would need to be routable over the public Internet.


### Set Up Cluster1 → Cluster2 Replication

#### Set up the Listeners on Cluster1 (Source cluster)
On a node in Cluster1, `node1` for example, identify the nodes that will be listening to connections from replication clients with `riak-repl add-listener <nodename> <listen_ip> <port>` for each node that will be listening for replication clients.

    riak-repl add-listener riak@172.16.1.11 172.16.1.11 9010
    riak-repl add-listener riak@172.16.1.12 172.16.1.12 9010
    riak-repl add-listener riak@172.16.1.13 172.16.1.13 9010
    
#### Setup the Site on Cluster2 (Site cluster)
On a node in Cluster2, `node4` for example, inform the replication clients where the Source Listeners are with `riak-repl add-site <ipaddr> <port> <sitename>`.  Use the IP address(es) and port(s) you configured in the earlier step.  For `sitename` enter **Cluster1**.

    riak-repl add-site 172.16.1.11 9010 Cluster1

> While a Listener needs to be added to each node, only a single Site needs to be added on the Site cluster.  Once connected to the Source cluster, it will get the locations of the rest of the Listeners in the Source cluster.


### Verify the Replication Configuration
Verify the replication configuration using `riak-repl status` on a Cluster1 node and a Cluster2 node.  A full description of the `riak-repl status` command's output can be found at the [riak-repl status output documentation](http://docs.basho.com/riakee/latest/cookbooks/Multi-Data-Center-Replication-Operations/#riak-repl-status-output)

On the Cluster1 node, verify that there are `listener_<nodename>`s for each listening node, and that `leader`,  and `server_stats` are populated.  They should look similar to the following:

    listener_riak@172.16.1.11: "172.16.1.11:9010"
    listener_riak@172.16.1.12: "172.16.1.12:9010"
    listener_riak@172.16.1.13: "172.16.1.13:9010"
    leader: 'riak@172.16.1.11'
    server_stats: [{<8051.3939.0>,
                    {message_queue_len,0},
                    {status,[{site,"Cluster2"},
                             {strategy,riak_repl_keylist_server},
                             {fullsync_worker,<8051.3940.0>},
                             {dropped_count,0},
                             {queue_length,0},
                             {queue_byte_size,0},
                             {state,wait_for_partition}]}}]
                         

On the Cluster2 node, verify that `Cluster1_ips`, `leader`,  and `client_stats` are populated.  They should look similar to the following:

    Cluster1_ips: "172.16.1.11:9010, 172.16.1.12:9010, 172.16.1.13:9010"
    leader: 'riak@192.168.1.21'
    client_stats: [{<8051.3902.0>,
                    {message_queue_len,0},
                    {status,[{site,"Cluster1"},
                             {strategy,riak_repl_keylist_client},
                             {fullsync_worker,<8051.3909.0>},
                             {put_pool_size,5},
                             {connected,"172.16.1.11",9010},
                             {state,wait_for_fullsync}]}}]



                             
### Testing Realtime Replication

That's all there is to it!  When PUT requests are coordinated by Cluster1, these operations will be replicated to Cluster2.  

You can use the following script to verify that PUT operations sent to Cluster1 are being replicated to Cluster2:

```bash
#!/bin/bash

VALUE=`date`
CLUSTER_1_IP=172.16.1.11
CLUSTER_2_IP=192.168.1.21
 
curl -s -X PUT -d "${VALUE}" http://${CLUSTER_1_IP}:8098/riak/replCheck/c1

CHECKPUT_C1=`curl -s http://${CLUSTER_1_IP}:8098/riak/replCheck/c1`

if [ "${VALUE}" = "${CHECKPUT_C1}" ]; then
  echo "C1 PUT Successful"
else
	echo "C1 PUT Failed"
	exit 1
fi

CHECKREPL_C1_TO_C2=`curl -s http://${CLUSTER_2_IP}:8098/riak/replCheck/c1`

if [ "${VALUE}" = "${CHECKREPL_C1_TO_C2}" ]; then
        echo "C1 to C2 consistent"
else
        echo "
C1 to C2 inconsistent
        C1:${CHECKPUT_C1}
        C2:${CHECKREPL_C1_TO_C2}
"
	exit 1
fi

exit 0

```

You will have to change some of the above variables for your own environment, such as IP addresses or ports.

If you run this script and things are working as expected, you will get the following output:

    C1 PUT Successful
    C1 to C2 consistent


### Set Up Cluster2 → Cluster1 Replication

#### About Bidirectional Replication
Multi Data Center support can also be configured to replicate in both directions, ensuring eventual consistency between your two data centers.  Setting up bidirectional replication is as simple as repeating the steps above in the other direction, i.e., from Cluster2 to Cluster1.

#### Set up the Listeners on Cluster2 (Source cluster)
On a node in Cluster2, `node4` for example, identify the nodes that will be listening to connections from replication clients with `riak-repl add-listener <nodename> <listen_ip> <port>` for each node that will be listening for replication clients.

    riak-repl add-listener riak@192.168.1.21 192.168.1.21 9010
    riak-repl add-listener riak@192.168.1.22 192.168.1.22 9010
    riak-repl add-listener riak@192.168.1.23 192.168.1.23 9010

#### Setup the Site on Cluster1 (Site cluster)
On a node in Cluster1, `node1` for example, inform the replication clients where the Source Listeners are with `riak-repl add-site <ipaddr> <port> <sitename>`.  Use the IP address(es) and port(s) you configured in the earlier step.  For `sitename` enter **Cluster2**.

    riak-repl add-site 192.168.1.21 9010 Cluster2


### Verify the Replication Configuration
Verify the replication configuration using `riak-repl status` on a Cluster1 node and a Cluster2 node.  A full description of the `riak-repl status` command's output can be found at the [riak-repl status output documentation](http://docs.basho.com/riakee/latest/cookbooks/Multi-Data-Center-Replication-Operations/#riak-repl-status-output)

On the Cluster1 node, verify that `Cluster2_ips`. `leader`,  and `client_stats` are populated.  They should look similar to the following:

    Cluster2_ips: "192.168.1.21:9010, 192.168.1.22:9010, 192.168.1.23:9010"
    leader: 'riak@172.16.1.11'
    client_stats: [{<8051.3902.0>,
                    {message_queue_len,0},
                    {status,[{site,"Cluster2"},
                             {strategy,riak_repl_keylist_client},
                             {fullsync_worker,<8051.3909.0>},
                             {put_pool_size,5},
                             {connected,"192.168.1.21",9010},
                             {state,wait_for_fullsync}]}}]

On the Cluster2 node, verify that there are listener entries for each listening node, and that `leader`,  and `server_stats` are populated.  They should look similar to the following:

    listener_riak@192.168.1.21: "192.168.1.21:9010"
    listener_riak@192.168.1.22: "192.168.1.22:9010"
    listener_riak@192.168.1.23: "192.168.1.23:9010"
    leader: 'riak@192.168.1.21'
    server_stats: [{<8051.3939.0>,
                    {message_queue_len,0},
                    {status,[{site,"Cluster1"},
                             {strategy,riak_repl_keylist_server},
                             {fullsync_worker,<8051.3940.0>},
                             {dropped_count,0},
                             {queue_length,0},
                             {queue_byte_size,0},
                             {state,wait_for_partition}]}}]
                         

### Testing Realtime Replication
You can use the following script to perform PUTs and GETs on both sides of the replication and verify that those changes are replicated to the other side.

```bash
#!/bin/bash

VALUE=`date`
CLUSTER_1_IP=172.16.1.11
CLUSTER_2_IP=192.168.1.21
 
curl -s -X PUT -d "${VALUE}" http://${CLUSTER_1_IP}:8098/riak/replCheck/c1

CHECKPUT_C1=`curl -s http://${CLUSTER_1_IP}:8098/riak/replCheck/c1`

if [ "${VALUE}" = "${CHECKPUT_C1}" ]; then
	echo "C1 PUT Successful"
else
	echo "C1 PUT Failed"
	exit 1
fi

curl -s -X PUT -d "${VALUE}" http://${CLUSTER_2_IP}:8098/riak/replCheck/c2
CHECKPUT_C2=`curl -s http://${CLUSTER_2_IP}:8098/riak/replCheck/c2`

if [ "${VALUE}" = "${CHECKPUT_C2}" ]; then
        echo "C2 PUT Successful"
else
        echo "C2 PUT Failed"
	exit 1
fi

CHECKREPL_C1_TO_C2=`curl -s http://${CLUSTER_2_IP}:8098/riak/replCheck/c1`
CHECKREPL_C2_TO_C1=`curl -s http://${CLUSTER_1_IP}:8098/riak/replCheck/c2`

if [ "${VALUE}" = "${CHECKREPL_C1_TO_C2}" ]; then
        echo "C1 to C2 consistent"
else
        echo "
C1 to C2 inconsistent
        C1:${CHECKPUT_C1}
        C2:${CHECKREPL_C1_TO_C2}
"
	exit 1
fi

if [ "${VALUE}" = "${CHECKREPL_C2_TO_C1}" ]; then
        echo "C2 to C1 consistent"
else
        echo "
C2 to C1 inconsistent
	C2:${CHECKPUT_C2}
	C1:${CHECKREPL_C2_TO_C1}
"
	exit 1
fi

exit 0

```

You will have to change some of the above variables for your own environment, such as IP addresses or ports.

If you run this script and things are working as expected, you will get the following output:

    C1 PUT Successful
    C2 PUT Successful
    C1 to C2 consistent
    C2 to C1 consistent


### Full Sync

#### About the fullsync operation
During real-time replication, operations coordinated by the Source cluster will be replicated to the Site cluster.  Riak Objects are placed in a queue on the Source cluster and streamed to the Site cluster. When the queue is full due to high traffic or a bulk loading operation, some objects will be dropped from replication. These dropped objects can be sent to the Site cluster by running a fullsync operation. The settings for the realtime replication queue and their explanations are available in the [[Multi Data Center Replication Configuration]] documentation.

#### Initiating a fullsync
To start a fullsync operation, issue the following command on your leader node:

	riak-repl start-fullsync
	
A fullsync operation may also be cancelled.  If a partition is in progress, synchronization will stop after that partition completes.  During cancellation, `riak-repl status` will show 'cancelled' in the status.

	riak-repl cancel-fullsync
	
Fullsync operations may also be paused, resumed, or scheduled for certain times using cron jobs.  A complete list of fullsync commands is available in the [[MDC Operations|Multi Data Center Replication Operations#Operations]] documentation.
