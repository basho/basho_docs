---
title: "Riak Redis Add-on Deployment Models"
description: "Explore the various models for deploying Riak Redis Add-on"
project: "riak_kv"
project_version: "2.2.3"
menu:
  riak_kv-2.2.3:
    name: "Redis Add-on Deployment Models"
    identifier: "add-ons_redis_deployment"
    weight: 201
    parent: "add-ons_redis_setup"
toc: true
commercial_offering: true
---

[Local-deployment]: {{<baseurl>}}images/redis/rra_deployment_local.png
[Colocated-deployment]: {{<baseurl>}}images/redis/rra_deployment_colocated.png
[Distributed-deployment]: {{<baseurl>}}images/redis/rra_deployment_distributed.png

## Deployment Models

### Local Cache Deployment

In a local cache deployment, the RRA and Redis are deployed to the application
server.

![Local-deployment]({{<baseurl>}}images/redis/rra_deployment_local.png)

Connections:

* RRA: The connections between Application Service instances to RRA Service
  instance are local.
* Redis: The connection between the RRA Service instance and Redis Service
  instance is local.
* Riak: The connections between Application Servers to Riak Nodes is distributed
  and bounded to equal the number of Riak nodes _multiplied_ by the number of
  Application Servers since they are aggregated at the RRA Service instance.

Advantages:

* Cache hits are extremely fast

Disadvantages:

* Cache writes on one application server are *not* observed on other application
  servers, so cache hit rates are likely lower unless some form of consistent
  routing to the application server exists within the solution.
* Redis competing for RAM with the application service may be problematic

### Colocated Cache Deployment

In a colocated cache deployment, the RRA may be deployed either to the
application server (suggested) or to the Riak servers and Redis is deployed to
the Riak servers.

In the case of deploying the RRA to the application servers, the RRA features
of reducing connections from the relatively high number of application service
instances to the fewer Redis (cache) and Riak (persistent) data service
instances allows for the greatest scale at the expense of the deployment cost
of pushing a service and its configuration.

In the case of deploying the RRA to the colocated Redis and Riak data servers,
the maximum scale for the solution is contrained by the number of network
connections from the application services while deployment costs remain a matter
of pushing a service and its configuration. In either case, deployment should
be automated, so are not multiplied by the number of servers.

![Colocated-deployment]({{<baseurl>}}images/redis/rra_deployment_colocated.png)

Connections:

* RRA: The connections between Application Service instances to RRA Service
  instance are distributed and bounded to equal the number of Riak nodes
  _multiplied_ by the number of Application Service instances.
* Redis: The connection between the RRA Service instance and Redis Service
  instance is local.
* Riak: The connections between RRA to Riak Nodes is distributed and bounded to
  equal the number of Riak nodes _squared_.

Advantages:

* Increases the cache hit rate as a cache write from one application server
  will lead to a cache hit by all other application servers.

Disadvantages:

* Typically increased distance between the application service and Redis and
  Riak services, so slightly increased latency compared to local.
* Redis competing for RAM with Riak will likely be problematic. Redis should
  be configured to ensure `maxmemory` and `maxmemory-policy` constrain Redis
  to ensure Riak is allotted sufficient RAM to serve the more important
  persistent data storage and retrieval services. See http://redis.io/topics/config
* This model may seem to provide data locality, but in the case of faults in
  either Redis or Riak services, the fault tolerance mechanisms of RRA and
  Riak will not match exactly as communicating the necessary information to
  support such a lock-step fault tolerance would lead to greater mean latencies
  and Riak provides superior 99th percentile latency performance in the face
  of faults.


### Distributed Cache Deployment

In a distributed cache deployment, the RRA is deployed to the application server
and Redis is deployed to standalone servers, separate from Riak cluster nodes.

![Distributed-deployment]({{<baseurl>}}images/redis/rra_deployment_distributed.png)

Connections:

* RRA: The connections between Application Service instances to RRA Service
  instance are local.
* Redis: The connection between the RRA Service instance and Redis Service
  instance are distributed and bounded to equal the number of Application
  Servers _multipled_ by the number of Redis Servers.
* Riak: The connections between RRA to Riak Nodes is distributed and bounded to
  equal the number of Riak nodes _multiplied_ by the number of Application
  Servers since they are aggregated at the RRA Service instance.

Advantages:

* Increases the cache hit rate as a cache write from one application server
  will lead to a cache hit by all other application servers.
* Keeps RRA near the application, reducing network connections.
* Moves Redis to distinct servers, allowing the cache more RAM and not
  constraining the RAM of either application or persistent data services.

Disadvantages:

* Typically increased distance between the application service and Redis and
  Riak services, so increased latency compared to local.

### Recommendation

The relative advantages and disadvantages of the Distributed Cache Deployment,
most notably the increased cache hit rate and reduced connection overhead,
should make it the standout choice for applications requiring the scale and
operational simplicity of Riak. For this reason, we recommend the Distributed
Cache Deployment.
