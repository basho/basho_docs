---
title: data-platform-admin Command Line Interface
project: dataplatform
version: 1.0.0+
document: guide
audience: beginner
[bdp configure]:
[bdp configure add services]:
[bdp install]: 
[bdp reference]: 
---

The `data-platform-admin` command line tool allows you to perform various operations on your Basho Data Platform cluster. The following reference outlines available commands and their uses.

```
Usage: data-platform-admin { join | add-service-config | remove-service | start-service | stop-service | services | node-services | service-nodes }
```

Use `--help` after a sub-command for more details. For example:

```
data-platform-admin join --help
```


## join

Join a node to the Basho Data Platform cluster.

```
data-platform-admin join <node>
```

### Parameters

| Parameters | Description |
| ---------- | ----------- |
| `<node>`   | The name of the Riak node. |


## leave

Remove a node from the Basho Data Platform cluster.

```
data-platform-admin leave
```


## cluster-status

Display a summary of the status of nodes in the cluster.

```
data-platform-admin cluster-status
```


## add-service-config

Add a new service configuration to cluster.

```
data-platform-admin add-service-config <service-name> <service> [<service-configuration>]
```

### Parameters

| Parameters       | Description |
| -----------------| ----------- |
| `<service-name>` | The name of the service. |
| `<service>`      | Valid services include: redis, cache-proxy, spark-worker, spark-master |
| `[<service-configuration | Specified as Key/Value pairs: `my_config=42` |

### Options

| Options/Flags | Description |
| ------------- | ----------- |
| `-f` / `--force`   | The `--force` (`-f`) flag enables an overwrite of an existing service configuration. Without `--force`, attempts to add a service configuration that already exists will result in an error. |


## remove-service-config

Remove an existing service configuration from the cluster.

```
data-platform-admin remove-service <service>
```

### Parameters

| Parameters  | Description |
| ----------- | ----------- |
| `<service>` | Valid services include: redis, cache-proxy, spark-worker, spark-master |


## start-service

Start a service on the designated platform instance. The `-i/--output-ip` flag will cause the IP address of node to be printed back out on the console instead of the normal output.

```
data-platform-admin start-service <node> <group> <service> [-i | --output-ip]
```

### Parameters

| Parameters       | Description |
| -----------------| ----------- |
| `<node>`   | The name of the Riak node. |
| `<group>`        | The name of the group. |
| `<service>`      | Valid services include: redis, cache-proxy, spark-worker, spark-master |

### Options

| Options/Flags | Description |
| ------------- | ----------- |
| `-i` / ` --output-ip ` | Print IP address of node to console instead of normal output. |


## stop-service

Stop a service on the designated instance.

```
data-platform-admin stop-service <node> <group> <service>
```

### Parameters

| Parameters       | Description |
| -----------------| ----------- |
| `<node>`   | The name of the Riak node. |
| `<group>` | The name of the group. |
| `<service>` | Valid services include: redis, cache-proxy, spark-worker, spark-master |


## services

Display available services on the cluster.

```
data-platform-admin services
```


## node-services

Display all running services for the given node.

```
data-platform-admin node-services <node>
```

### Parameters

| Parameters       | Description |
| -----------------| ----------- |
| `<node>`   | The name of the Riak node. |


## service-nodes

Display all nodes running the designated service.

```
data-platform-admin service-nodes <service>
```

### Parameters

| Parameters       | Description |
| -----------------| ----------- |
| `<service>`      | Valid services include: redis, cache-proxy, spark-worker, spark-master |




