---
title: "Downgrading EE to OSS"
description: ""
project: "riak_kv"
project_version: "2.2.3"
menu:
  riak_kv-2.2.3:
    name: "Downgrading EE to OSS"
    identifier: "downgrading_package"
    weight: 100
    parent: "downgrade"
toc: true
---

[cs downgrade]: /riak/cs/2.1.1/cookbooks/downgrading/
[downloads]: /riak/kv/2.2.3/downloads/

On this page, you will find step-by-step instructions for downgrading Riak KV Enterprise Edition to Riak KV open-source. Downgrades from EE to OSS must be performed on each node in your cluster in a rolling fashion.

> **Note**: If this KV cluster is a part of a Riak CS installation, please see [Downgrading Riak CS from EE to Open-source][cs downgrade].

To downgrade your nodes from Riak KV EE to Riak KV OSS, you will need to follow all of these steps on each node in your KV cluster:

* [Preliminary Steps](#preliminary-steps)
* [Downgrade KV installation](#downgrade-riak-kv-installation)
* [Post-downgrade](#postdowngrade)
* [Repeat](#repeat)


## Preliminary Steps

Downgrading from KV EE to KV OSS, you will need to disable Multi-Datacenter (MDC) replication to and from the cluster (1), remove the node from any SNMP monitoring infrastructures (2), and remove the node from any JMX monitoring infrastructures (3).


### 1. Disable Multi-Datacenter replication

You will need to disable MDC on your cluster as a replication source and sink.

> **Note:** In complex replication topologies, this will take coordinated commands across your infrastructure.

1\. Run `riak-repl connections`, which lists the clusters you are connected to via the MDC V3 protocol. You will see output similar to the following if this cluster is a replication source:

```
Connection           Cluster Name         <Ctrl-Pid>      [Members]
----------           ------------         ----------      ---------
Cluster3             Cluster3             <0.8901.0>      ["127.0.0.1:10036"] (via 127.0.0.1:10036)
Cluster2             Cluster2             <0.8882.0>      ["127.0.0.1:10026"] (via 127.0.0.1:10026)
```

This output indicates that the current cluster is connected as a source cluster to two other clusters named Cluster2 and Cluster3.

2\. For each listed cluster, run:

```
riak-repl disconnect »cluster name« 
```

For instance, given the example output above, we would run:

```
riak-repl realtime disable Cluster2
riak-repl realtime disable Cluster3

riak-repl fullsync disable Cluster2
riak-repl fullsync disable Cluster3

riak-repl disconnect Cluster2
riak-repl disconnect Cluster3
```

3\. Run `riak-repl connections` again to verify that the clusters have been successfully disconnected.  You should see the following output:

```
Connection           Cluster Name         <Ctrl-Pid>      [Members]
----------           ------------         ----------      ---------
```


## Downgrade Riak KV Installation

Once you have completed the [preliminary steps](#preliminary-steps), you can downgrade your Riak KV package.

1\. Shut down the node you are going to upgrade by running `riak stop`.

2\. Back up your configuration and data directories. Your configuration directory is usually in /etc/riak. The location of your data directory will be listed in your configuration files.

3\. Uninstall your KV EE package.

4\. Install the Riak KV OSS package, which you can find [here][downloads].

5\. While a standard package uninstall should not have removed your data directories, if it did, take a moment to move your backup to where the data directory should be.

6\. Some package management tools (such as yum and apt-get) will leave your original configuration, but rename them. If this occurred, overwrite the default package configuration files (riak.conf and advanced.config) with the renamed files or with your backup.


## 3. Post-Downgrade

Now that you've [downgraded your KV installation](#downgrade-riak-kv-installation), you are almost done! You just need to perform a few verifications before you're ready to move on to the next node in your cluster.


1\. Verify that your configuration is correct by running `riak config generate` and reviewing the results.  Address any issues by commenting out or deleting the configuration items displayed. 

In some cases, Basho's Riak Enterprise Edition(EE) products have more configuration elements than their open-source counterparts. This will cause the node to not start after downgrades since riak.conf has strict checking against the configuration schema.

For example, if we run `riak config generate` and see:
 
```
[vagrant@node1 riak]$ sudo riak config generate
13:28:18.748 [error] You've tried to set jmx, but there is no setting with that name.
13:28:18.748 [error]   Did you mean one of these?
13:28:18.755 [error]     sasl
13:28:18.755 [error]     dtrace
13:28:18.755 [error]     search
13:28:18.755 [error] Error generating configuration in phase transform_datatypes
13:28:18.755 [error] Conf file attempted to set unknown variable: jmx
```

We would know that we needed to comment out or remove the line trying to set `jmx`. Using a text editor, we comment out the line, and then we run `riak config generate` again. We should see:

```
[vagrant@node1 riak]$ sudo riak config generate
 -config /var/lib/riak/generated.configs/app.2017.04.21.13.37.10.config -args_file /var/lib/riak/generated.configs/vm.2017.04.21.13.37.10.args -vm_args /var/lib/riak/generated.configs/vm.2017.04.21.13.37.10.args  
 ```

This output tells us that the configuration file was parsed properly and that there should be no issue starting the node.

2\. Start Riak KV on the downgraded node by running `riak start`.

3\. Finally, delete the /etc/riak/snmp directory. Once the node has been downgraded, there is no further need for it.


## Repeat

Once you've downgraded to KV OSS on one node, you will need to follow these instructions for node in your KV cluster one at a time in rolling fashion.