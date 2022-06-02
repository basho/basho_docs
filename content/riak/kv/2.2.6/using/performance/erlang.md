---
title: "Erlang VM Tuning"
description: ""
project: "riak_kv"
project_version: "2.2.6"
menu:
  riak_kv-2.2.6:
    name: "Erlang VM"
    identifier: "performance_erlang"
    weight: 105
    parent: "managing_performance"
toc: true
aliases:
  - /riak/2.2.6/ops/tuning/erlang
  - /riak/kv/2.2.6/ops/tuning/erlang
---

Riak was written almost exclusively in [Erlang](http://www.erlang.org)
and runs on an Erlang virtual machine (VM), which makes proper Erlang VM
tuning an important part of optimizing Riak performance. The Erlang VM
itself provides a wide variety of [configurable parameters](http://erlang.org/doc/man/erl.html) that you can use to tune its performance; Riak enables you to tune a subset of those parameters in each node's [configuration files](../../../configuring/reference/#erlang-vm).

The table below lists some of the parameters that are available, showing
both their names as used in Erlang and their names as Riak parameters.

Erlang parameter | Riak parameter
:----------------|:--------------
[`+A`](http://erlang.org/doc/man/erl.html#async_thread_pool_size) | `erlang.async_threads`
[`+K`](http://erlang.org/doc/man/erl.html#emu_flags) | `erlang.K`
[`+P`](http://erlang.org/doc/man/erl.html#+P) | `erlang.process_limit`
[`+Q`](http://erlang.org/doc/man/erl.html#+Q) | `erlang.max_ports`
[`+S`](http://erlang.org/doc/man/erl.html#+S) | `erlang.schedulers.total`, `erlang.schedulers.online`
[`+W`](http://erlang.org/doc/man/erl.html#emu_flags) | `erlang.W`
[`+a`](http://erlang.org/doc/man/erl.html#async_thread_stack_size) | `erlang.async_threads.stack_size`
[`+e`](http://www.erlang.org/doc/man/ets.html#+e) | `erlang.max_ets_tables`
[`+scl`](http://www.erlang.org/doc/main/erl.html#+scl) | `erlang.schedulers.compaction_of_load`
[`+sfwi`](http://www.erlang.org/doc/man/erl.html#+sfwi) | `erlang.schedulers.force_wakeup_interval`
[`-smp`](http://erlang.org/doc/man/erl.html#smp) | `erlang.smp`
[`+sub`](http://www.erlang.org/doc/man/erl.html#+sub) | `erlang.schedulers.utilization_balancing`
[`+zdbbl`](http://erlang.org/doc/man/erl.html#+zdbbl) | `erlang.distribution_buffer_size`
[`-kernel net_ticktime`](http://www.erlang.org/doc/man/kernel_app.html#net_ticktime) | `erlang.distribution.net_ticktime`
[`-env FULLSWEEP_AFTER`](http://www.erlang.org/doc/man/erlang.html#system_flag-2) | `erlang.fullsweep_after`
[`-env ERL_CRASH_DUMP`](http://www.erlang.org/doc/apps/erts/crash_dump.html) | `erlang.crash_dump`
[`-env ERL_MAX_ETS_TABLES`](http://learnyousomeerlang.com/ets) | `erlang.max_ets_tables`
`-name` | `nodename`

{{% note title="Note on upgrading to 2.0" %}}
In versions of Riak prior to 2.0, Erlang VM-related parameters were specified
in a `vm.args` configuration file; in versions 2.0 and later, all
Erlang-VM-specific parameters are set in the `riak.conf` file. If you're
upgrading to 2.0 from an earlier version, you can still use your old `vm.args`
if you wish.  Please note, however, that if you set one or more parameters in
both `vm.args` and in `riak.conf`, the settings in `vm.args` will override
those in `riak.conf`.
{{% /note %}}

## SMP

Some operating systems provide Erlang VMs with Symmetric Multiprocessing
capabilities
([SMP](http://en.wikipedia.org/wiki/Symmetric_multiprocessing)) for
taking advantage of multi-processor hardware architectures. SMP support
can be turned on or off by setting the `erlang.smp` parameter to
`enable` or `disable`. It is enabled by default. The following would
disable SMP support:

```riakconf
erlang.smp = disable
```

Because Riak is supported on some operating systems that do not provide
SMP support. Make sure that your OS supports SMP before enabling it for
use by Riak's Erlang VM. If it does not, you should set `erlang.smp` to
`disable` prior to starting up your cluster.

Another safe option is to set `erlang.smp` to `auto`. This will instruct
the Erlang VM to start up with SMP support enabled if (a) SMP support is
available on the current OS and (b) more than one logical processor is
detected. If neither of these conditions is met, the Erlang VM will
start up with SMP disabled.

## Schedulers

> **Note on missing scheduler flags**
>
> We recommend that _all_ users set the `+sfwi` to `500` (milliseconds)
and the `+scl` flag to `false` if using the older, `vm.args`-based
configuration system. If you are using the new, `riak.conf`-based
configuration system, the corresponding parameters are
`erlang.schedulers.force_wakeup_interval` and
`erlang.schedulers.compaction_of_load`.
>
> Please note that you will need to uncomment the appropriate lines in
your `riak.conf` for this configuration to take effect.

If [SMP support](#smp) has been enabled on your Erlang
VM, i.e. if `erlang.smp` is set to `enable` or `auto` on a machine
providing SMP support _and_ more than one logical processor, you can
configure the number of logical processors, or [scheduler
threads](http://www.erlang.org/doc/man/erl.html#+S), that are created
when starting Riak, as well as the number of threads that are set
online.

The total number of threads can be set using the
`erlang.schedulers.total` parameter, whereas the number of threads set
online can be set using `erlang.schedulers.online`. These parameters map
directly onto `Schedulers` and `SchedulersOnline`, both of which are
used by [`erl`](http://www.erlang.org/doc/man/erl.html#+S).

While the maximum for both parameters is 1024, there is no universal
default for either. Instead, the Erlang VM will attempt to determine the
number of configured processors, as well as the number of available
processors, on its own. If the Erlang VM _can_ make that determination,
`schedulers.total` will default to the total number of configured
processors while `schedulers.online` will default to the number of
processors available; if the Erlang VM can't make that determination,
both values will default to 1.

If either parameter is set to a negative integer, that value will be
subtracted from the default number of processors that are configured or
available, depending on the parameter. For example, if there are 100
configured processors and `schedulers.total` is set to `-50`, then the
calculated value for `schedulers.total` will be 50. Setting either
parameter to 0, on the other hand, will reset both values to their
defaults.

If SMP support is not enabled, i.e. if `erlang.smp` is set to `disable`
(or set to `auto` on a machine without SMP support or with only one
logical processor), then the values of `schedulers.total` and
`schedulers.online` will be ignored.

### Scheduler Wakeup Interval

Scheduler wakeup is an optional process whereby Erlang VM schedulers are
periodically scanned to determine whether they have "fallen asleep,"
i.e. whether they have an empty [run
queue](http://en.wikipedia.org/wiki/Run_queue). The interval at which
this process occurs can be set, in milliseconds, using the
`erlang.schedulers.force_wakeup_interval` parameter, which corresponds
to the Erlang VM's `+sfwi` flag. This parameter is set to `0` by
default, which disables scheduler wakeup.

Erlang distributions like R15Bx have a tendency to put schedulers to
sleep too often. If you are using a more recent distribution, i.e. a if
you are running Riak 2.0 or later, you most likely won't need to enable
scheduler wakeup.

### Scheduler Compaction and Balancing

The Erlang scheduler offers two methods of distributing load across
schedulers: **compaction of load** and **utilization balancing** of
load.

Compaction of load is used by default. When enabled, the Erlang VM will
attempt to fully load as many scheduler threads as possible, i.e. it
will attempt to ensure that scheduler threads do not run out of work. To
that end, the VM will take into account the frequency with which
schedulers run out of work when making decisions about which schedulers
should be assigned work. You can disable compaction of load by setting
the `erlang.schedulers.compaction_of_load` setting to `false` (in the
older configuration system, set `+scl` to `true`).

The other option, utilization balancing, is disabled by default in favor
of load balancing. When utilization balancing is enabled instead, the
Erlang VM will strive to balance scheduler utilization as equally as
possible between schedulers, without taking into account the frequency
at which schedulers run out of work. You can enable utilization
balancing by setting the `erlang.schedulers.utilization_balancing`
setting to `true` (or the `+scl` parameter to `false` in the older
configuration system).

At any given time, only compaction of load _or_ utilization balancing
can be used. If you set both parameters to `false`, Riak will default to
using compaction of load; if both are set to `true`, Riak will enable
whichever setting is listed first in `riak.conf` (or `vm.args` if you're
using the older configuration system).

## Port Settings

Riak uses [epmd](http://www.erlang.org/doc/man/epmd.html), the Erlang
Port Mapper Daemon, for most inter-node communication. In this system,
other nodes in the [cluster](../../../learn/concepts/clusters) use the Erlang identifiers specified by the `nodename` parameter (or `-name` in `vm.args`), for example `riak@10.9.8.7`. On each node, the daemon resolves these node
identifiers to a TCP port. You can specify a port or range of ports for
Riak nodes to listen on as well as the maximum number of concurrent
ports/sockets.

### Port Range

By default, epmd binds to TCP port 4369 and listens on the wildcard
interface. epmd uses an unpredictable port for inter-node communication
by default, binding to port 0, which means that it uses the first
available port. This can make it difficult to configure [firewalls](../../security).

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
          {inet_dist_listen_max, 5000}
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

You can set the maximum number of concurrent ports/sockets used by the
Erlang VM using the `erlang.max_ports` setting. Possible values range
from 1024 to 134217727. The default is 65536. In `vm.args` you can use
either `+Q` or `-env ERL_MAX_PORTS`.

## Asynchronous Thread Pool

If thread support is available in your Erlang VM, you can set the number
of asynchronous threads in the Erlang VM's asynchronous thread pool
using `erlang.async_threads` (`+A` in `vm.args`).  The valid range is 0
to 1024. If thread support is available on your OS, the default is 64.
Below is an example setting the number of async threads to 600:

```riakconf
erlang.async_threads = 600
```

```vmargs
+A 600
```

### Stack Size

In addition to the number of asynchronous threads, you can determine the
memory allocated to each thread using the
`erlang.async_threads.stack_size` parameter, which corresponds to the
`+a` Erlang flag. You can determine that size in Riak using KB, MB, GB,
etc. The valid range is 16-8192 kilowords, which translates to 64-32768
KB on 32-bit architectures. While there is no default, we suggest a
stack size of 16 kilowords, which translates to 64 KB. We suggest such a
small size because the number of asynchronous threads, as determined by
`erlang.async_threads` might be quite large in your Erlang VM. The 64 KB
default is enough for drivers delivered with Erlang/OTP but might not be
large enough to accommodate drivers that use the `driver_async()`
functionality, documented
[here](http://www.erlang.org/doc/man/erl_driver.html). We recommend
setting higher values with caution, always keeping the number of
available threads in mind.

## Kernel Polling

You can utilize kernel polling in your Erlang distribution if your OS
supports it. Kernel polling can improve performance if many file
descriptors are in use; the more file descriptors, the larger an effect
kernel polling may have on performance. Kernel polling is enabled by
default on Riak's Erlang VM, i.e. the default for `erlang.K` is `on`.
This corresponds to the
[`+K`](http://erlang.org/doc/man/erl.html#emu_flags) setting on the
Erlang VM. You can disable it by setting `erlang.K` to `off`.

## Warning Messages

Erlang's
[`error_logger`](http://www.erlang.org/doc/man/error_logger.html) is an
event manager that registers error, warning, and info events from the
Erlang runtime. By default, events from the `error_logger` are mapped as
warnings, but you can also set messages to be mapped as errors or info
reports using the `erlang.W` parameter (or `+W` in `vm.args`). The
possible values are `w` (warnings), `errors`, or `i` (info reports).

## Process Limit

The `erlang.process_limit` parameter can be used to set the maximum
number of simultaneously existing system processes (corresponding to
Erlang's `+P` parameter). The valid range is 1024 to 134217727. The
default is 256000.

## Distribution Buffer

You can set the size of the Erlang VM's distribution buffer busy limit
(denoted by `+zdbbl` on the VM and in `vm.args`) by adding
`erlang.distribution_buffer_size` to `riak.conf`.  Modifying this setting can be useful
on nodes with many `busy_dist_port` events, i.e. instances when the
Erlang distribution is overloaded. The default is 32 MB (i.e. `32MB`),
but this may be insufficient for some workloads. The maximum value is
2097151 KB.

A larger buffer limit will allow processes to buffer more outgoing
messages. When the limit is reached, sending processes will be suspended
until the the buffer size has shrunk below the limit specified by
`erlang.distribution_buffer_size`. Higher values will tend to produce
lower latency and higher throughput but at the expense of higher RAM
usage. You should evaluate your RAM resources prior to increasing this
setting.

## Erlang Built-in Storage

Erlang uses a built-in database called
[ets](http://www.erlang.org/doc/man/ets.html) \(Erlang Term Storage)
for some processes that require fast access from memory in constant
access time (rather than logarithmic access time).  The maximum number
of tables can be set using the `erlang.max_ets_tables` setting. The
default is 256000, which is higher than the default limit of 1400 on the
Erlang VM. The corresponding setting in `vm.args` is `+e`.

Higher values for `erlang.max_ets_tables` will tend to provide more
quick-access data storage but at the cost of higher RAM usage. Please
note that the default values for `erlang.max_ets_tables` and
`erlang.distribution_size` (explained in the section [above](#distribution-buffer)) are the same.

## Crash Dumps

By default, crash dumps from Riak's Erlang distribution are deposited in
`./log/erl_crash.dump`. You can change this location using
`erlang.crash_dump`. This is the equivalent of setting the
[`ERL_CRASH_DUMP`](http://www.erlang.org/doc/man/erl.html#environment_variables)
environment variable for the Erlang VM.

## Net Kernel Tick Time

The [net kernel](http://erlang.org/doc/man/net_kernel.html) is an Erlang
system process that provides various forms of network monitoring. In a
Riak cluster, one of the functions of the net kernel is to periodically
check node liveness. **Tick time** is the frequency with which those
checks happen. You can determine that frequency using the
`erlang.distribution.net_ticktime`. The tick will occur every N seconds,
where N is the value set. Thus, setting
`erlang.distribution.net_ticktime` to `60` will make the tick occur once
every minute. The corresponding flag in `vm.args` is `-kernel
net_ticktime`.

## Shutdown Time

You can determine how long the Erlang VM spends shutting down using the
`erlang.shutdown_time` parameter. The default is `10s` (10 seconds).
Once this duration elapses, all existing processes are killed.
Decreasing shutdown time can be useful in situations in which you are
frequently starting and stopping a cluster, e.g. in test clusters. In
`vm.args` you can set the `-shutdown_time` flag in milliseconds.
