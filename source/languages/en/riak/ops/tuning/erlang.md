---
title: Erlang VM Tuning
project: riak
version: 2.0.0+
document: cookbook
audience: advanced
keywords: [erlang, operator]
---

Riak was written almost exclusively in [Erlang](http://www.erlang.org)
and runs on the Erlang virtual machine (VM). Because of this, proper
Erlang VM tuning is an important part of optimizing Riak performance.

The Erlang VM itself provides a wide variety of [configurable
parameters](http://erlang.org/doc/man/erl.html) that you can use to tune
its performance; Riak enables you to tune a subset of those parameters
in each node's [[configuration files|Configuration Files#Erlang-VM]].
The table below lists some of the parameters that are available, showing
both their names as used directly in Erlang and their names as Riak
parameters.

Erlang param | Riak param
:------------|:----------
[`+A`](http://erlang.org/doc/man/erl.html#async_thread_pool_size) | `erlang.async_threads`
[`+K`](http://erlang.org/doc/man/erl.html#emu_flags) | `erlang.K`
[`+P`](http://erlang.org/doc/man/erl.html#+P) | `erlang.process_limit`
[`+Q`](http://erlang.org/doc/man/erl.html#+Q) | `erlang.max_ports`
[`+S`](http://erlang.org/doc/man/erl.html#+S) | `erlang.schedulers.total`, `erlang.schedulers.online`
[`+W`](http://erlang.org/doc/man/erl.html#emu_flags) | `erlang.W`
[`+e`](http://www.erlang.org/doc/man/ets.html#+e) | `erlang.max_ets_tables`
[`-smp`](http://erlang.org/doc/man/erl.html#smp) | `erlang.smp`
[`+sfwi`](http://www.erlang.org/doc/man/erl.html#+sfwi) | `erlang.schedulers.force_wakeup_interval`
[`+zdbbl`](http://erlang.org/doc/man/erl.html#+zdbbl) | `erlang.distribution_buffer_size`

The list above is not exhaustive of the available Erlang VM parameters.
A full listing can be found in [[Erlang VM Settings|Configuration
Files#Erlang-VM]].

<div class="note">
<div class="title">Note on upgrading to 2.0</div>
In versions of Erlang prior to 2.0, Erlang VM-related parameters were
specified in a `vm.args` file. If you're upgrading to 2.0 from an
earlier version, you can still use your old `vm.args`.
</div>

## SMP

Some operating systems are equipped to provide Erlang VMs with
Symmetric Multiprocessing Capabilities, aka
[SMP](http://en.wikipedia.org/wiki/Symmetric_multiprocessing). SMP
support can be turned on and off by setting the `erlang.smp` parameter
to `enable` or `disable`. It is enabled by default. Setting this to
`auto` will instruct the Erlang VM to start with SMP support enabled
if it is available _and_ more than one logical processor is detected.

Riak is supported on some operating systems that do not provide SMP
support. Make sure that you ensure that your OS supports SMP before
enabling it for use by Riak's Erlang VM. If it does not, you should set
`erlang.smp` to `disable` prior to starting up your cluster.

## Schedulers

If [[SMP support|Erlang VM Tuning#SMP]] has been enabled on your Erlang
VM, i.e. if `erlang.smp` is set to `enable`, you can configure the
number of logical processors, or [scheduler
threads](http://www.erlang.org/doc/man/erl.html#+S), that are created
when starting Riak, as well as the number of threads that are set
online.

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

Riak uses [epmd](http://www.erlang.org/doc/man/epmd.html), the Erlang
Port Mapper Daemon, for most inter-node communication, identifying other
nodes in the [[cluster|Clusters]] using the Erlang identifiers specified
by the `nodename` parameter, e.g.  `riak@10.9.8.7`. The Erlang Port
Mapper Daemon ([epmd](http://www.erlang.org/doc/man/epmd.html)) running
on each node resolves these node identifiers to a TCP port. You can
specify a port or range of ports for Riak nodes to listen on as well as
the maximum number of concurrent ports/sockets.

### Port Range

By default, epmd binds to TCP port 4369 and listens on the wildcard
interface. epmd uses an unpredictable port for inter-node communication
by default, binding to port 0, which means that it uses the first
available port. This can make it difficult to configure
[[firewalls|Security and Firewalls]].

To make configuring firewalls easier, you can instruct the Erlang VM to
use either a limited range of TCP ports or a single TCP port. The
minimum and maximum can be set using the
`erlang.distribution.port_range.minimum` and
`erlang.distribution.port.maximum` parameters, respectively. The
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

You can set the maximum number of concurrent ports/sockets used by the Erlang
VM using the `erlang.max_ports` setting. Possible values range from 1024
to 134217727. The default is 65536.

## Asynchronous Thread Pool

You can set the number of asynchronous threads in the Erlang VM's
asynchronous thread pool using `erlang.async_threads` (`+A` in Erlang).
The valid range is 0 to 1024. If thread support is available on your
OS, the default is 64.

```riakconf
erlang.async_threads = xxx
```

```vmargs
+A xxx
```

## Kernel Polling

Kernel poll is a means of checking for data . The more file descriptors
are in use, the larger an effect kernel polling can have on performance.
...

## Warning Messages

Erlang's
[`error_logger`](http://www.erlang.org/doc/man/error_logger.html) is an
event manager that registers error, warning, and info events from the
Erlang runtime. By default, events from the `error_logger` are mapped as
warnings, but you can also set messages to be mapped as errors or info
reports. The possible values are `w` (warnings), `errors`, or `i` (info
reports).

## Process Limit

The `erlang.process_limit` parameter can be used to set the maximum
number of simultaneously existing system processes. The valid range is
1024 to 134217727. The default is 256000.

## Distribution Buffer

`erlang.distribution_buffer_size`
Useful on nodes with many `busy_dist_port` events
The default is 32 MB, but this may be insufficient for some workloads
A larger buffer limit will allow processes to buffer more outgoing
messages; when the limit is reached, sending processes will be suspended
until the buffer size has shrunk below the "buffer size" threshold; set
on a per-disribution-channel basis; higher => lower latency and higher
throughput, but higher memory usage; depends on available RAM
Corresponds to `+zdbbl`; set in KB

## Erlang Built-in Storage

Erlang Term Storage; in a part of the VM where destructive updates are
allowed and there is no GC; limited concurrency in reads/writes; avoid
having more than one table per process (notice that the defaults for
`erlang.max_ets_tables` and `erlang.distribution_size` are the same)

Constant access time vs. logarithmic access time; ETS tables stored
tuples, whatever you want; more ETS tables mean more quick access data
storage, but at the cost of more RAM

Erlang uses a built-in database called
[ets](http://www.erlang.org/doc/man/ets.html)
Enables fast access from memory
`erlang.max_ets_tables` raises the ETS table limit; default is 256000
The default limit on the Erlang VM is 1400

## Crash Dumps

By default, crash dumps deposited in `./log/erl_crash.dump`. You can
change this location using `erlang.crash_dump`. This is the equivalent
of setting the
[`ERL_CRASH_DUMP`](http://www.erlang.org/doc/man/erl.html#environment_variables)
environment variable.
