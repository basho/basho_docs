---
title: Security and Firewalls
project: riak
version: 0.10.0+
document: cookbook
toc: true
audience: advanced
keywords: [troubleshooting, security]
moved: {
    '1.4.0-': '/cookbooks/Network-Security-and-Firewall-Configurations'
}
---

{{#2.0.0+}}
<div class="info"><div class="title">Internal security</div>This
document covers network-level security; for authentication and
authorization introduced with Riak 2.0, see
[[Authentication and Authorization]].</div>
{{/2.0.0+}}

The following article discusses standard configurations and port
settings to use when thinking about how to secure a Riak Cluster.

There are two classes of access control for Riak:

* Other Riak nodes participating in the cluster
* Clients making use of the Riak cluster

The settings for both access groups are located in `app.config`:

* `pb_ip` {{1.4.0-}}
* `pb_port` {{1.4.0-}}
* `pb` {{1.4.0+}}
* `http`
* `https`

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


---

# Riak Security Community

## Riak

Riak is a powerful open-source distributed database focused on scaling predictably and easily, while remaining highly available in the face of server crashes, network partitions or other (inevitable) disasters.

## Commitment

Data security is an important and sensitive issue to many of our users. A real-world approach to security allows us to balance appropriate levels of security and related overhead while creating a fast, scalable, and operationally straightforward database.

### Continuous Improvement

Though we make every effort to thwart security vulnerabilities whenever possible (including through independent reviews), no system is completely secure. We will never claim that Riak is 100% secure (and you should seriously doubt anyone who claims their solution is). What we can promise is that we openly accept all vulnerabilities from the community. When appropriate, we'll publish and make every attempt to quickly address these concerns.

### Balance

More layers of security increase operational and administrative costs. Sometimes those costs are warranted, sometimes they are not. Our approach is to strike an appropriate balance between effort, cost and security.

For example, Riak does not have fine-grained role-base security. Though it can be an attractive bullet-point in a database comparison chart, you're usually better off finely controlling data access through your application or a service layer.

### Notifying Basho

If you discover a potential security issue, please email us at security@basho.com, and allow us 48 hours to reply.

We prefer to be contacted first, rather than searching for blog posts over the Internet. This allows us to open a
dialog with the security community on how best to handle a possible exploit without putting any users at risk.

For sensitive topics, you may send a secure message. The security team's GPG key is:

```
-----BEGIN PGP PUBLIC KEY BLOCK-----
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
```

## Security Best Practices

### Network Configurations

Being a distributed database means that much of Riak's security springs from how you configure your network. We have a few recommendations for [[Security and Firewalls]].

### Client Auth

Many of the Riak drivers support HTTP basic auth, though this is not a role-based security solution. You might instead wish to connect over HTTPS or through a VPN.

### Multi Data Center Replication

For those versions of Riak that support Multi Data Center (MDC) Replication, you can configure Riak 1.2+ to communicate over SSL, to seamlessly encrypt the message traffic.

*No link here yet until the EDS docs are published*
