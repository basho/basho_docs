---
title: "Security Overview"
description: "Overview of security in Riak TS."
menu:
  riak_ts-1.4.0:
    name: "Security"
    identifier: "security"
    weight: 330
    parent: "using"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/using/security/
canonical_link: "https://docs.basho.com/riak/ts/latest/using/security/"
---

[JMX]: http://www.oracle.com/technetwork/java/javase/tech/javamanagement-140525.html
[Solr]: http://lucene.apache.org/solr/

As of Riak TS 1.4, security is supported for the Protocol Buffers Client (PBC) interface (like in [Riak KV](/riak/kv/)).

The following is an overview of standard configurations and port settings to use for network security in a Riak TS Cluster. For more on enabling security and managing users & security sources see the [best practices](#best-practices) section below.

## Access Control

There are two classes of access control for Riak TS:

* Other Riak TS nodes participating in the cluster
* Clients making use of the Riak TS cluster

The settings for both access groups are located in your cluster's
configuration settings.

For `riak.conf`-based configuration, you can set a host and port for each node in that node's `riak.conf` file with the `listener.protobuf` setting.

If you are using the `app.config`-based configuration system, adjust the settings of `pb`.

Make note of these configurations and set up your firewall to allow
incoming TCP access to those ports or IP address/port combinations.
Exceptions to this are the `handoff_ip` and `handoff_port` directives.
Those are for communication between Riak TS nodes only.

## Inter-node Communication

Riak TS uses the Erlang distribution mechanism for most inter-node
communication. Riak TS identifies other machines in the ring using Erlang
identifiers (`<hostname or IP>`, e.g. `riak@10.9.8.7`). Erlang resolves
these node identifiers to a TCP port on a given machine via the Erlang
Port Mapper daemon (epmd) running on each cluster node.

By default, epmd binds to TCP port 4369 and listens on the wildcard
interface. For inter-node communication, Erlang uses an unpredictable
port by default; it binds to port 0, which means the first available
port.

For ease of firewall configuration, Riak can be configured
to instruct the Erlang interpreter to use a limited range
of ports. For example, to restrict the range of ports that Erlang will
use for inter-Erlang node communication to 6000-7999, add the following
lines to the configuration file on each Riak node:

```riakconf
erlang.distribution.port_range.minimum = 6000
erlang.distribution.port_range.maximum = 7999
```

```appconfig
{ kernel, [
            {inet_dist_listen_min, 6000},
            {inet_dist_listen_max, 7999}
          ]},
```

The above lines should be added into the top level list in app.config,
at the same level as all the other applications (e.g. `riak_core`).
Then configure your firewall to allow incoming access to TCP ports 6000
through 7999 from whichever network(s) contain your Riak TS nodes.

## Riak TS Node Ports

Riak TS nodes in a cluster need to be able to communicate freely with one
another on the following ports:

* epmd listener: TCP:4369
* handoff_port listener: TCP:8099
* range of ports specified in `app.config` or `riak.conf`

## Riak TS Client Ports

Riak TS clients must be able to contact at least one machine in a Riak
cluster on the following TCP ports:

Protocol | Port
:--------|:----
<a href="http://docs.basho.com/riak/kv/2.1.4/developing/api/protocol-buffers/">Protocol Buffers</a> | TCP port 8087

## Best Practices

### Security Checklist

Before enabling security on your Riak TS cluster be sure to check out the [Security Checklist](./checklist).

### Enabling, Disabling, and Checking Security

For a brief tutorial on how to enable, disable, or check the security status of a cluster, visit the [Riak TS: Security Basics](./basics) page.

### Managing User Authentication and Permissions

For instructions on how to apply permissions and require client authentication in Riak TS, please see our [Security: User Management](./user-management)documentation.

### Managing Security Sources

For more on creating and managing security sources for authentication in Riak TS, check out [Security: Sources Management](./sources-management).

### Notifying Basho

If you discover a potential security issue with Riak TS or want to know more about Basho's approach to security, visit the [Security: Notifying Basho](./notify-basho) page.
