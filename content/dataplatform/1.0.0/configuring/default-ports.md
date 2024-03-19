---
title: "Default Ports for Basho Data Platform"
description: ""
project: "dataplatform"
project_version: "1.0.0"
menu:
  dataplatform-1.0.0:
    name: "Default Ports"
    identifier: "configuring_default_ports"
    weight: 103
    parent: "configuring"
toc: true
aliases:
  - /dataplatform/1.0.0/using-dataplatform/configuration/reference/default-ports/
  - /dataplatform/latest/configuring/default-ports/
---

The following is a list of default network ports used by Basho Data Platform. Your environment should be configured to allow traffic for these network ports.

| Service | Default |
| ---------- | ---------- |
| Secure Shell (SSH) | 22/TCP |
| EPMD Listener | 4369/TCP |
| Leader Election (Enterprise Edition only) | 5323 |
| Riak Inter-Nodes Communication | 6000-7999 (port range) |
| Redis | 6379 |
| Cache-Proxy | 22122 |
| Cache-Proxy Stats | 22123 |
| Spark Master | 7077 |
| Spark Master Web UI  | 8080 |
| Spark Worker | 7078 |
| Spark Worker Web UI | 8081 |
| Protocol Buffer | 8087/TCP |
| HTTP | 8098/TCP |
| Riak Handof | 8099/TCP |
| Search (Solr) | 8093 |
| Search (Solr JMX) | 8985 |
| Cluster Manager | 9080 |
| Riak EE JMX (Enterprise Edition only) | 41110 |
