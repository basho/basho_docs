---
title: "Logging Reference"
description: ""
project: "riak_kv"
project_version: "2.2.0"
menu:
  riak_kv-2.2.0:
    name: "Logging"
    identifier: "managing_ref_logging"
    weight: 100
    parent: "managing_ref"
toc: true
aliases:
  - /riak/2.2.0/ops/running/logging
  - /riak/kv/2.2.0/ops/running/logging
---

[cluster ops log]: {{<baseurl>}}riak/kv/2.2.0/using/cluster-operations/logging

Logging in Riak KV is handled by a Basho-produced logging framework for
[Erlang](http://www.erlang.org) called
[lager](https://github.com/basho/lager).

lager provides a number of configuration options that you can use to fine-tune your Riak cluster's logging output. A compact listing of parameters can be found in our [configuration files]({{<baseurl>}}riak/kv/2.2.0/configuring/reference/#logging) documentation. A more thorough explanation of these options can be found in this document.

## Log Directory

Riak's log files are stored in a `/log` directory on each node. The
location of that directory differs from platform to platform. The table
below shows you where log files are stored on all supported operating
systems.

OS | Directory
:--|:---------
Ubuntu, Debian, CentOS, RHEL | `/var/log/riak`
Solaris, OpenSolaris | `/opt/riak/log`
Source install and Mac OS X | `./log` (where the `.` represents the root installation directory)

## Log Files

Below is a list of files that can be found in each node's `/log`
directory:

File | Significance
:----|:------------
`console.log` | Console log output
`crash.log` | Crash logs
`erlang.log` | Logs emitted by the [Erlang VM](../../performance/erlang) on which Riak runs.
`error.log` | [Common errors](../../repair-recovery/errors) emitted by Riak.
`run_erl.log` | The log file for an Erlang process called `run_erl`. This file can typically be ignored.

## Log Syntax

Riak logs tend to be structured like this:

```log
<date> <time> [<level>] <PID> <prefix>: <message>
```

The `date` segment is structured `YYYY-MM-DD`, `time` is structured
`hh:mm:ss.sss`, `level` depends on which log levels are available in the
file you are looking at (consult the sections below), the `PID` is the
Erlang process identifier for the process in which the event occurred,
and the message `prefix` will often identify the Riak subsystem
involved, e.g. `riak_ensemble_peer` or `alarm_handler` (amongst many
other possibilities).

The exception to this syntax is in crash logs (stored in `crash.log`
files). For crash logs, the syntax tends to be along the following
lines:

```log
<date> <time> =<report title>====
<message>
```

Here is an example crash report:

```log
2014-10-17 15:56:38 =ERROR REPORT====
Error in process <0.4330.323> on node 'dev1@127.0.0.1' with exit value: ...
```

## Log Files

In each node's `/log` directory, you will see at least one of each of
the following:

File | Contents
:----|:--------
`console.log` | General messages from all Riak subsystems
`crash.log` | Catastrophic events, such as node failures, running out of disk space, etc.
`erlang.log` | Events from the Erlang VM on which Riak runs
`run_erl.log` | The command-line arguments used when starting Riak

### Log File Rotation

Riak maintains multiple separate files for `console.log`, `crash.log`,
`erlang.log`, and `error.log`, which are rotated as each file reaches
its maximum capacity of 100 KB. In each node's `/log` directory, you may
see, for example, files name `console.log`, `console.log.0`,
`console.log.1`, and so on. Riak's log rotation is somewhat non
traditional, as it does not always log to `*.1` (e.g. `erlang.log.1`)
but rather to the oldest log file.

After, say, `erlang.log.1` is filled up, the logging system will begin
writing to `erlang.log.2`, then `erlang.log.3`, and so on. When
`erlang.log.5` is filled up, it will loop back to `erlang.log.1`.

## SASL

[SASL](http://www.erlang.org/doc/man/sasl_app.html) (System Architecture
Support Libraries) is Erlang's built-in error logger. You can enable it
and disable it using the `sasl` parameter (which can be set to `on` or
`off`). It is disabled by default. The following would enable it:

```riakconf
sasl = on
```

## Error Messages

By default, Riak stores error messages in `./log/error.log` by default.
You can change this using the `log.error.file` parameter. Here is an
example, which uses the default:

```riakconf
log.error.file = ./log/error.log
```

By default, error messages are redirected into lager, i.e. the
`log.error.redirect` parameter is set to `on`. The following would
disable the redirect:

```riakconf
log.error.redirect = off
```

You can also throttle the number of error messages that are handled per
second. The default is 100.

```riakconf
log.error.messages_per_second = 100
```

## Crash Logs

Riak crash logs are stored in `./log/crash.log` by default. You can
change this using the `log.crash.file` parameter. This example uses the
default:

```riakconf
log.crash.file = ./log/crash.log
```

While crash logs are kept by default, i.e. the `log.crash` parameter is
set to `on`, you can disable crash logs like this:

```riakconf
log.crash = off
```

### Crash Log Rotation

Like other Riak logs, crash logs are rotated. You can set the crash logs
to be rotated either when a certain size threshold is reached and/or at
designated times.

You can set the rotation time using the `log.crash.rotation` parameter.
The default is `$D0`, which rotates the logs every day at midnight. You
can also set the rotation to occur weekly, on specific days of the
month, etc. Complete documentation of the syntax can be found
[here](https://github.com/basho/lager/blob/master/README.md#internal-log-rotation).
Below are some examples:

* `$D0` --- Every night at midnight
* `$D23` --- Every day at 23:00 (11 pm)
* `$W0D20` --- Every week on Sunday at 20:00 (8 pm)
* `$M1D0` --- On the first day of every month at midnight
* `$M5D6` --- On the fifth day of the month at 6:00 (6 am)

To set the maximum size of the crash log before it is rotated, use the
`log.crash.size` parameter. You can specify the size in KB, MB, etc. The
default is `10MB`.


### Other Crash Log Settings

The maximum size of individual crash log messages can be set using the
`log.crash.maximum_message_size`, using any size denomination you wish,
e.g. `KB` or `MB`  The default is 64 KB. The following would set that
maximum message size to 1 MB:

```riakconf
log.crash.maximum_message_size = 1MB
```

## Syslog

Riak log output does not go to syslog by default, i.e. the `log.syslog`
setting is set to `off` by default. To enable syslog output:

```riakconf
log.syslog = on
```

If syslog output is enabled, you can choose a prefix to be appended to
each syslog message. The prefix is `riak` by default.

```riakconf
log.syslog.ident = riak
```

### Syslog Level and Facility Level

If syslog is enabled, i.e. if `log.syslog` is set to `on`, you can
select the log level of syslog output from amongst the available levels,
which are listed in the table below. The default is `info`.

* `alert`
* `critical`
* `debug`
* `emergency`
* `error`
* `info`
* `none`
* `notice`
* `warning`

In addition to a log level, you must also select a [facility
level](http://en.wikipedia.org/wiki/Syslog#Facility_levels) for syslog
messages amongst the available levels, which are listed in the table
below. The default is `daemon`.

* `auth`
* `authpriv`
* `clock`
* `cron`
* `daemon`
* `ftp`
* `kern`
* `lpr`
* `mail`
* `news`
* `syslog`
* `user`
* `uucp`

In addition to these options, you may also choose one of `local0`
through `local7`.

## Console Logs

Riak console logs can be emitted to one of three places: to a log file
(you can choose the name and location of that file), to standard output,
or to neither. This is determined by the value that you give to the
`log.console` parameter, which gives you one of four options:

* `file` --- Console logs will be emitted to a file. This is Riak's
    default behavior. The location of that file is determined by the
    `log.console.file` parameter. The default location is
    `./log/console.log` on an installation from [source]({{<baseurl>}}riak/kv/2.2.0/setup/installing/source), but will differ on platform-specific installation,
    e.g.  `/var/log/riak` on Ubuntu, Debian, CentOS, and RHEL or
    `/opt/riak/log` on Solaris-based platforms.
* `console` --- Console logs will be emitted to standard output, which
    can be viewed by running the [`riak attach-direct`]({{<baseurl>}}riak/kv/2.2.0/using/admin/riak-cli/#attach-direct) command
* `both` --- Console logs will be emitted both to a file and to standard
    output
* `off` --- Console log messages will be disabled

In addition to the the placement of console logs, you can also choose
the severity of those messages using the `log.console.level` parameter.
The following four options are available:

* `info` (the default)
* `debug`
* `warning`
* `error`

## Enabling and Disabling Debug Logging

Checkout [Cluster Operations: Enabling and Disabling Debug Logging][cluster ops log]
