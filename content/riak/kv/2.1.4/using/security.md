---
title: "Security & Firewalls"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Security"
    identifier: "managing_security"
    weight: 205
    parent: "managing"
toc: true
aliases:
  - /riak/2.1.4/ops/advanced/security
  - /riak/kv/2.1.4/ops/advanced/security
canonical_link: "https://docs.basho.com/riak/kv/latest/using/security"
---

[config reference search]: /riak/kv/2.1.4/configuring/reference/#search
[config search enabling]: /riak/kv/2.1.4/configuring/search/#enabling-riak-search
[config v3 ssl]: /riak/kv/2.1.4/configuring/v3-multi-datacenter/ssl
[JMX]: http://www.oracle.com/technetwork/java/javase/tech/javamanagement-140525.html
[security basics]: /riak/kv/2.1.4/using/security/basics
[security managing]: /riak/kv/2.1.4/using/security/managing-sources/
[Solr]: http://lucene.apache.org/solr/
[usage search]: /riak/kv/2.1.4/developing/usage/search

> **Internal security**
>
> This document covers network-level security. For documentation on the
authentication and authorization features introduced in Riak 2.0, see
[Authentication and Authorization][security basics] and [Managing Security Sources][security managing]

This article discusses standard configurations and port settings to use
when providing network security for a Riak Cluster. There are two
classes of access control for Riak:

* Other Riak nodes participating in the cluster
* Clients making use of the Riak cluster

The settings for both access groups are located in your cluster's
configuration settings. If you are using the newer configuration system,
you can set a host and port for each node in that node's `riak.conf`
file, setting `listener.protobuf` if you are using Riak's Protocol
Buffers interface or `listener.http` if you are using HTTP (or
`listener.https` if you are using SSL). If you are using the older
configuration system, adjust the settings of `pb`, `http`, or `https`,
depending on which client interface you are using.

Make note of these configurations and set up your firewall to allow
incoming TCP access to those ports or IP address/port combinations.
Exceptions to this are the `handoff_ip` and `handoff_port` directives.
Those are for communication between Riak nodes only.

## Inter-node Communication

Riak uses the Erlang distribution mechanism for most inter-node
communication. Riak identifies other machines in the ring using Erlang
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
through 7999 from whichever network(s) contain your Riak nodes.

### Riak Node Ports

Riak nodes in a cluster need to be able to communicate freely with one
another on the following ports:

* epmd listener: TCP:4369
* handoff_port listener: TCP:8099
* range of ports specified in `app.config` or `riak.conf`

### Riak Client Ports

Riak clients must be able to contact at least one machine in a Riak
cluster on the following TCP ports:

Protocol | Port
:--------|:----
<a href="http://docs.basho.com/riak/kv/2.1.4/developing/api/http/">HTTP</a> | TCP port 8098
<a href="http://docs.basho.com/riak/kv/2.1.4/developing/api/protocol-buffers/">Protocol Buffers</a> | TCP port 8087

### Riak Search Ports

Riak's [search][usage search] feature relies on [Apache Solr][Solr], which runs on each Riak node if security has been [enabled][config search enabling]. When Riak's Search subsystem starts up, [JMX][JMX] opens a well-known port as well as some ephemeral ports. The well-known port is determined by the value of the `search.solr.jmx_port` in each node's [Search configuration][config reference search]. The default is 8985.

In addition to JMX ports, Solr also binds to a well-known port of its
own, as determined by each node's `search.solr.port` setting, which is
also located in each node's Search configuration. The default is 8093.

# Riak Security Community

## Riak

Riak is a powerful open-source distributed database focused on scaling
predictably and easily, while remaining highly available in the face of
server crashes, network partitions or other (inevitable) disasters.

## Commitment

Data security is an important and sensitive issue to many of our users.
A real-world approach to security allows us to balance appropriate
levels of security and related overhead while creating a fast, scalable,
and operationally straightforward database.

### Continuous Improvement

Though we make every effort to thwart security vulnerabilities whenever
possible (including through independent reviews), no system is
completely secure. We will never claim that Riak is 100% secure (and you
should seriously doubt anyone who claims their solution is). What we can
promise is that we openly accept all vulnerabilities from the community.
When appropriate, we'll publish and make every attempt to quickly
address these concerns.

### Balance

More layers of security increase operational and administrative costs.
Sometimes those costs are warranted, sometimes they are not. Our
approach is to strike an appropriate balance between effort, cost, and
security.

For example, Riak does not have fine-grained role-base security. Though
it can be an attractive bullet-point in a database comparison chart,
you're usually better off finely controlling data access through your
application or a service layer.

### Notifying Basho

If you discover a potential security issue, please email us at
**security@basho.com**, and allow us 48 hours to reply.

We prefer to be contacted first, rather than searching for blog posts
over the Internet. This allows us to open a dialogue with the security
community on how best to handle a possible exploit without putting any
users at risk.

For sensitive topics, you may send a secure message. The security team
has the following GPG key:

<pre><tt>-----BEGIN PGP PUBLIC KEY BLOCK-----
Version: GnuPG v1.4.12 (Darwin)

mQENBFAQM40BCADGjCmwn9Q9xpWfJ4HpKGwt5kGyf4Oq4PglC28MhtscT9cGwtJv
gRK1ckzkwhCdw6uQKRN3o3iVFHFp+uD8G28zs1fGNfpUZls7WV29WyxfIgB3f01Q
Ll6tiZ2fLG69lSlLTPn7JlzZz1sRVrAKdwUVEYRKCidF0bqaztBCkKbcNAmIvV1E
TboEGMPLXqOnK2134NP+tp0B15oNwSQd9jmOrClvhCF5NR4ATQguS5ecp05/GldZ
8vQQ1XOBc2uiuWpzvhD2CAXQ/Spxir8JjbqpzjPo6d4yte7pYvx6wfnJ9b2KC+sn
AtdqqQslZ3saceXAFXFOIGk7NOq8LSattmRbABEBAAG0GkJhc2hvIDxzZWN1cml0
eUBiYXNoby5jb20+iQE4BBMBAgAiBQJQEDONAhsDBgsJCAcDAgYVCAIJCgsEFgID
AQIeAQIXgAAKCRDEq056TdGVhHl7B/9rXnzZOdC7M8NN+BAEO8kucw0dXGhgcahs
zS81WDRpRJD1fi+QBinfohGg2phIq5TlrXNmduFwCpvyujNkeiCr+Nh00mp6SdU2
m7XFzfPIz3ZWR0YNdvruaf0W5K6jAaHcJkkc3Xwpgk6rxTcNwWUqYRGD7zie4Iad
At0WLJXMUvJH2XoMf8MGO5mHspkqC5M/HvNvH3ZG5CldIHPqgZdg4NXMcGtFAr8z
72wFamick31oCpJyWq+AloOxh3mJpfhp94EBrc/lGbbOD/Sg4oyT+B/4Ee0zWqN5
hDBefi3FCyjo2NuhM1YyRrrvWe7Kwaj8iuItYPIpEwGUqEJzZ7kYuQENBFAQM40B
CAC4J0Pb1WXjGpsQnfOdzZUq57x63RaVA74IIuLSU7v//04wNgNGiLdMbz4isr6K
5NfXTu0i+GqQdcj7UnajwxYCUEnXYpKQBLfT82tTgdw/DPXYgSnxIC02POrwCnhr
wSDbUryuTdbZFS13HPrQPdOXZlmG8oHOgu04a9vPUlkshYmUZm+zRY2FIuW8fJ44
ysJBm49hxkF9WuyGnNiU8UJEvw0sS63x4EUkYdJXLzzdC9T8/t8HGV3aKFEZ3km0
GgYUlt04FdWtFjYcMQnrhJSf7atxwQLpfH78sFCyEH+PFIRfnkirVx9TbN0QSw/z
VaRNxJQde2SHfEft66mf0RJ5ABEBAAGJAR8EGAECAAkFAlAQM40CGwwACgkQxKtO
ek3RlYRPFwf+LiHlf9tCqRLwmI2X8bBmoQTV/Eb4pbPF/1WR6W/afAMp4ZiLpWtn
XeZ9UNdnQDPJIMPhaWrPHB4oLCnDBm1m6wq6FVjHcDur+s7QtWnnTuaVKBDKY42T
NkFj+WP3ZBsfDBtt49KRLm0bWqzkhK7IA+1DMKRmTUhf0tIeLb0um0hL+mXNucrE
dMk+Fdh/54IfHMMw3GwtNd+ZMLf8cht+z3Z0Y0qONe0ClfkiligYItD+P5tufhew
HtU5clY0rP8W/Nr7tC+ZGH2bjT1bmN1E9IM4wjBdyWGTosvY6ciIxuY5p5Iy/UhB
7Xk9zl4ZkKcsVnuscYQPNE2jb393XAhFEg==
=1KRp
-----END PGP PUBLIC KEY BLOCK-----
</tt></pre>

## Security Best Practices

### Authentication and Authorization

For instructions on how to apply permissions and to require client
authentication, please see our documentation on [Riak Security][security basics].

### Network Configurations

Being a distributed database means that much of Riak's security springs
from how you configure your network. We have a few recommendations for
[Security and Firewalls][security basics].

### Client Auth

Many of the Riak drivers support HTTP basic auth, though this is not a
role-based security solution. You might instead wish to connect over
HTTPS or through a VPN.

### Multi-Datacenter Replication

For those versions of Riak that support Multi Data Center (MDC)
Replication, you can configure Riak 1.2+ to communicate over SSL, to
seamlessly encrypt the message traffic.

See also: [Multi Data Center Replication: SSL][config v3 ssl] in the Enterprise
Documentation.
