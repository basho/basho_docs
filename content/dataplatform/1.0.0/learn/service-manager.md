---
title: "Service Manager Features"
description: ""
project: "dataplatform"
project_version: "1.0.0"
menu:
  dataplatform-1.0.0:
    name: "Service Manager"
    identifier: "learn_service_manager"
    weight: 100
    parent: "learn"
toc: true
aliases:
  - /dataplatform/1.0.0/learn-about-dataplatform/service-manager-features/
  - /dataplatform/latest/learn/service-manager/
---

[bdp cli]: {{<baseurl>}}dataplatform/1.0.0/using/commands/

## Overview

The service manager is the foundation of the Basho Data Platform (BDP). It provides a means for building a cluster of nodes that can deploy, run, and manage platform services. Note that Riak KV is included with BDP, and use of BDP assumes familiarity with Riak KV.

For information on usage check out [data-platform-admin command-line interface](LINK).

## Data Replication and Synchronization

Replicate and synchronize data across and between Riak and Spark, Redis, and Solr service instances to ensure data accuracy with no data loss and high availability.

## Cluster Management & Monitoring
Integrated cluster management automates deployment and configuration of Riak KV, Riak S2, Spark, and Redis. Once deployed in production, auto-detect issues and restart Redis instances or Spark clusters. Cluster management eliminates the need for Zookeeper.

## Internal Data Store

A built-in, distributed data store for ensuring speed, fault-tolerance, and ease-of-operations is used to persist static and dynamic configuration data (port number and IP address) across the Basho Data Platform.

## Logging and Troubleshooting
Event logs provide valuable information that can facilitate the enhanced tuning of clusters and accurately analyze dataflow across the cluster.
