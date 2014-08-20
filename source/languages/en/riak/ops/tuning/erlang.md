---
title: Erlang VM Tuning
project: riak
version: 2.0.0+
document: cookbook
audience: advanced
keywords: [erlang, operator]
---

A table listing all available configurable parameters can be found in
our [[configuration files|Configuration Files#Erlang-VM]] documentation.

<div class="note">
<div class="title">Note on upgrading to 2.0</div>
In versions of Erlang prior to 2.0, Erlang VM-related parameters were
specified in a `vm.args` file. If you're using
</div>

## SMP

Some operating systems are equipped to provide Erlang VMs with
symmetric multiprocessing capabilities, aka
[SMP](http://en.wikipedia.org/wiki/Symmetric_multiprocessing). SMP
support can be turned on and off by setting the `erlang.smp` parameter
to `enable` or `disable`. It is enabled by default.

Riak is supported on some operating systems that do not provide SMP
support. Make sure that you ensure that your OS supports SMP before
enabling it for use by Riak's Erlang VM. If it does not, you should set
set `erlang.smp` to `disable` prior to starting up your cluster.

## Schedulers

If [[SMP support|Erlang VM Tuning#SMP]] has been enabled on your Erlang
VM, i.e. if the `erlang.smp` parameter is set to `enable`, you can
configure the number of logical processors, or [scheduler
threads](http://www.erlang.org/doc/man/erl.html#+S), that are created
when starting Riak, as well as the number of available scheduler
threads.

The total number of threads can be set using the
`erlang.schedulers.total` parameter, whereas the number of threads set
online can be determined using `erlang.schedulers.online`. These
parameters map directly onto `Schedulers` and `SchedulersOnline`, both
of which are used by `[erl](http://www.erlang.org/doc/man/erl.html#+S)`.

The maximum for both parameters is 1024. There is no universal default
for either of these parameters. Instead, the Erlang VM will attempt to
determine the number of configured processors as well as the number of
available processors. If the Erlang VM can make that determination,
`schedulers.total` will default to the number of configured processors
while `schedulers.online` will default to the number of processors
available; tf the Erlang VM can't make that determination, both values
will default to 1.

If either parameter is set to a negative integer, this value will be
subtracted from the default number of processors that are configured
or available, depending on the parameter. For example, if there are 100
configured processors and `schedulers.total` is set to `-50`, then
the value for `schedulers.total` will be 50. Setting either parameter to
0, on the other hand, will reset both values to their defaults.

If SMP support is not enabled, i.e. if `erlang.smp` is set to `disable`,
then `schedulers.total` and `schedulers.online` will be ignored.

## Port Settings

Riak uses the Erlang distribution mechanism for most inter-node
communication, identifying other nodes in the [[cluster|Clusters]]
using Erlang identifiers, e.g. `riak@10.9.8.7`. The Erlang Port Mapper
Daemon ([epmd](http://www.erlang.org/doc/man/epmd.html)) running on each
node resolves these node identifiers to a TCP port. You can specify a
port or range of ports for Riak nodes to listen on as well as the
maximum number of concurrent

### Port Range

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

### Maximum Ports

You can set the maximum number of concurrent sockets used by the Erlang
VM using the `erlang.max_ports` setting. Possible values range from 1024
to 134217727.
