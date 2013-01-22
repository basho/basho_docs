---
title: Network Security and Firewall Configurations
project: riak
version: 0.10.0+
document: cookbook
toc: true
audience: advanced
keywords: [troubleshooting, security]
---

The following article discusses standard configurations and port
settings to use when thinking about how to secure a Riak Cluster.

There are two classes of access control for Riak:

* Other Riak nodes participating in the cluster
* Clients making use of the Riak cluster

The settings for both access groups are located in `app.config`. The
configuration directives for client access all end in *ip* and *port*:
`web_ip`, `web_port`, `pb_ip`, and `pb_port`.

Make note of those and configure your firewall to allow incoming TCP
access to those ports or IP address and port combinations. Exceptions to
this are the `handoff_ip` and `handoff_port` directives. Those are for
communication between Riak nodes only.

Riak uses the Erlang distribution mechanism for most inter-node
communication. Riak identifies other machines in the ring using Erlang
identifiers (`<hostname or IP>`, ex: `riak@10.9.8.7`). Erlang resolves
these node identifiers to a TCP port on a given machine via the Erlang
Port Mapper daemon (epmd) running on each cluster node.

By default, epmd binds to TCP port 4369 and listens on the wildcard interface. For inter-node communication, Erlang uses an unpredictable port by default; it binds to port 0, which means the first available port.

For ease of firewall configuration, Riak can be configured via
`app.config` to instruct the Erlang interpreter to use a limited range
of ports. For example, to restrict the range of ports that Erlang will
use for inter-Erlang node communication to 6000-7999, add the following
lines to the `app.config` file on each Riak node:

```erlang
{ kernel, [
            {inet_dist_listen_min, 6000},
            {inet_dist_listen_max, 7999}
          ]},
```

The above lines should be added into the top level list in app.config,
at the same level as all the other applications (eg. **riak\_core**).

Then configure your firewall to allow incoming access to TCP ports 6000
through 7999 from whichever network(s) contain your Riak nodes.

**Riak nodes in a cluster need to be able to communicate freely with one
another on the following ports:**

* epmd listener: TCP:4369
* handoff_port listener: TCP:8099
* range of ports specified in `app.config`

**Riak clients must be able to contact at least one machine in a Riak
cluster on the following ports:**

* web_port: TCP:8098
* pb_port: TCP:8087

<div class="info"><div class="title">Important note</div>The epmd process will continue to run on a given node even after all Erlang interpreters have exited. If <tt>inet_dist_listen_min</tt> and <tt>inet_dist_listen_max</tt> are added to <tt>app.config</tt>, epmd must be killed so that it will pick up the new settings.</div>
