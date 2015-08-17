---
title: Installing Basho Data Platform on AWS
project: dataplatform
version: 1.0.0+
document: guide
audience: beginner
[bdp install]:
---

Whether you running a virtualized infrastructure or you just want to quickly test out Basho Data Platform (BDP), this page will walk you through configuring your AWS environment to run BDP.

If you were looking for the general install page, go [here][bdp install].

<div class="note">
AWS security profile must allow incoming and outgoing traffic from ip/ports used by Riak, Spark, and BDP.  A [list of default ports](LINK) is at the end of this document. 
</div>

##Setting Open-Files Limit

During normal operation Riak can consume a large number of open-file handles. You can increase the total limit for open files by running the following:

```
if [[ $(sysctl fs.file-max |grep 65536) == "" ]]; then
  sudo sysctl fs.file-max=65536
  sudo sysctl -p

  sudo bash -c "cat <<EOF_LIMITS >> /etc/security/limits.conf
*                soft    nofile          65536
*                hard    nofile          65536
EOF_LIMITS
"
fi
```

##Installing Java

```
sudo apt-get install python-software-properties
sudo add-apt-repository ppa:webupd8team/java
sudo apt-get update
sudo echo -e oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | sudo debconf-set-selections
sudo apt-get install -y oracle-java8-installer
```


##Setting Environment Variables

```
JAVA_HOME=$(dirname $(dirname $(readlink -f $(which javac))))
if [[ "$JAVA_HOME" == "" ]]; then
echo "failed to install jdk 8"
exit 1
fi
grep JAVA_HOME /etc/environment >/dev/null 2>&1 || test $? -ne 0 && sudo bash -c "echo JAVA_HOME=$JAVA_HOME >>/etc/environment"
```



##Ensure the Environment is preserved




## Download Basho Data Platform Enterprise Edition and Extras

```
wget http://private.downloads.basho.com/data-platform-ee/1.0/1.0.0/ubuntu/precise/data-platform-extras_1.0.0-bdb08f01-1_amd64.deb
wget http://private.downloads.basho.com/data-platform-ee/1.0/1.0.0/ubuntu/precise/data-platform-ee_1.0.0-bdb08f01-1_amd64.deb
```

## Installing Basho Data Platform

```
sudo dpkg -i data-platform-ee_1.0.0-bdb08f01-1_amd64.deb
sudo dpkg -i data-platform-extras_1.0.0-bdb08f01-1_amd64.deb
```

2: Setup BDP

run this on all nodes 

## Set Riak’s nodename, HTTP, PB  and Leader Latch listeners to be as box IP address

sudo riak stop
sudo sed -i -e "s/127\.0\.0\.1/`hostname -I | tr -d \'[[:space:]]\'`/" /etc/riak/riak.conf

## Enable Riak Leader Latch listener

sudo sed -i -e "s/## listener.leader_latch.internal =/listener.leader_latch.internal =/g" /etc/riak/riak.conf

sudo riak-admin reip riak@127.0.0.1 riak@`hostname -I`
sudo riak start

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



Appendix: Default Ports

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

BDP
Redis service
REDIS_PORT = 6379

Cache-Proxy service
CACHE_PROXY_PORT = 22122
CACHE_PROXY_STATS_PORT = 22123

Spark Service
SPARK_MASTER_PORT = 7077
SPARK_MASTER_WEBUI_PORT = 8080
SPARK_WORKER_PORT = 7078
SPARK_WORKER_WEBUI_PORT = 8081
