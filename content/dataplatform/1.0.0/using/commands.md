---
title: "Basho Data Platform Commands"
description: ""
project: "dataplatform"
project_version: "1.0.0"
menu:
  dataplatform-1.0.0:
    name: "Data Platform Commands"
    identifier: "using_cluster_commands"
    weight: 102
    parent: "using"
toc: true
aliases:
  - /dataplatform/1.0.0/using-dataplatform/dataplatform-commands/
  - /dataplatform/latest/using/commands/
---

[bdp configure]: {{<baseurl>}}dataplatform/1.0.0/configuring/setup-a-cluster/
[bdp configure add services]: {{<baseurl>}}dataplatform/1.0.0/configuring/setup-a-cluster/#add-services
[bdp install]: {{<baseurl>}}dataplatform/1.0.0/installing/
[bdp reference]: {{<baseurl>}}dataplatform/1.0.0/learn/service-manager/

Basho Data Platform (BDP) comes with a command line tool (`data-platform-admin`) that allows you to perform various operations on your BDP cluster. The following reference outlines available commands and their uses.

```
Usage: data-platform-admin { join | add-service-config | remove-service | start-service | stop-service | services | node-services | service-nodes }
```

Use `--help` after a sub-command for more details. For example:

```bash
data-platform-admin join --help
```


## join

Join a node to the Basho Data Platform cluster.

```bash
data-platform-admin join »node«
```

### Parameters

| Parameters | Description |
| ---------- | ----------- |
| `»node«`   | The name of the Riak node. |


## leave

Remove a node from the Basho Data Platform cluster.

```bash
data-platform-admin leave
```


## cluster-status

Display a summary of the status of nodes in the cluster.

```bash
data-platform-admin cluster-status
```


## add-service-config

Add a new service configuration to cluster.

```bash
data-platform-admin add-service-config »service-name« »service« [»service-configuration«]
```

### Parameters

| Parameters       | Description |
| -----------------| ----------- |
| `»service-name«` | The name of the service. |
| `»service«`      | Valid services include: redis, cache-proxy, spark-worker, spark-master |
| `»service-configuration` | Environment variables set on service start, Specified as Key/Value pairs (space-delimited): `HOST="0.0.0.0" THE_ANSWER="42"` |

### Options

| Options/Flags | Description |
| ------------- | ----------- |
| `-f` / `--force`   | The `--force` (`-f`) flag enables an overwrite of an existing service configuration. Without `--force`, attempts to add a service configuration that already exists will result in an error. |


## remove-service-config

Remove an existing service configuration from the cluster.

```bash
data-platform-admin remove-service »service«
```

### Parameters

| Parameters  | Description |
| ----------- | ----------- |
| `»service«` | Valid services include: redis, cache-proxy, spark-worker, spark-master |


## start-service

Start a service on the designated platform instance. The `-i/--output-ip` flag will cause the IP address of node to be printed back out on the console instead of the normal output.

```bash
data-platform-admin start-service »node« »group« »service« [-i | --output-ip]
```

### Parameters

| Parameters       | Description |
| -----------------| ----------- |
| `»node«`   | The name of the Riak node. |
| `»group«`        | The name of the group. |
| `»service«`      | Valid services include: redis, cache-proxy, spark-worker, spark-master |

### Options

| Options/Flags | Description |
| ------------- | ----------- |
| `-i` / ` --output-ip ` | Print IP address of node to console instead of normal output. |


## stop-service

Stop a service on the designated instance.

```bash
data-platform-admin stop-service »node« »group« »service«
```

### Parameters

| Parameters       | Description |
| -----------------| ----------- |
| `»node«`   | The name of the Riak node. |
| `»group«` | The name of the group. |
| `»service«` | Valid services include: redis, cache-proxy, spark-worker, spark-master |


## services

Display available services on the cluster.

```bash
data-platform-admin services
```


## node-services

Display all running services for the given node.

```bash
data-platform-admin node-services »node«
```

### Parameters

| Parameters       | Description |
| -----------------| ----------- |
| `»node«`   | The name of the Riak node. |


## service-nodes

Display all nodes running the designated service.

```bash
data-platform-admin service-nodes »service«
```

### Parameters

| Parameters       | Description |
| -----------------| ----------- |
| `»service«`      | Valid services include: redis, cache-proxy, spark-worker, spark-master |
