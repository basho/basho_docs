---
title: Erlang VM Tuning
project: riak
version: 2.0.0+
document: cookbook
audience: advanced
keywords: [erlang, operator]
---

## SMP

## Port Range

Riak uses the Erlang distribution mechanism for most inter-node
communication, identifying other nodes in the [[cluster|Clusters]]
using Erlang identifiers, e.g. `riak@10.9.8.7`. The Erlang Port Mapper
Daemon ([epmd](http://www.erlang.org/doc/man/epmd.html)) running on each
node resolves these node identifiers to a TCP port.

By default, epmd binds to TCP port 4369 and listens on the wildcard
interface. epmd uses an unpredictable port for inter-node communication
by default, binding to port 0, which means that it uses the first
available port. This can make it difficult to configure
[[firewalls|Security and Firewalls]].

To make configuring firewalls easier, you can instruct the Erlang VM to
use a limited range of TCP ports or a single TCP port. The minimum and
maximum can be set using the `erlang.distribution.port_range.minimum`
and `erlang.distribution.port.maximum` parameters, respectively. The
following would set the range to ports between 3000 and 5000:

```riakconf
erlang.distribution.port_range.minimum = 3000
erlang.distribution.port_range.maximum = 5000
```

```appconfig
%% The older, app.config-based system uses different parameter names
%% for specifying the minimum and maximum port
{kernel, [
          % ...
          {inet_dist_listen_min, 3000},
          {inet_dist_listen_min, 5000}
          % ...
         ]}
```

You can set the Erlang VM to use a single port by setting the minimum to
the desired port while setting no maximum. The following would set the
port to 5000:

```riakconf
erlang.distribution.port_range.minimum = 5000
```

```appconfig
{kernel, [
          % ...
          {inet_dist_listen_min, 5000},
          % ...
         ]}
```

If the minimum port is unset, the Erlang VM will listen on a random
high-numbered port.

