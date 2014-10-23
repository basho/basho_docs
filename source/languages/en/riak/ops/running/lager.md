---
title: lager
project: riak
version: 2.0.0+
document: reference
audience: intermediate
keywords: [operator, logging, lager]
---

[lager](https://github.com/basho/lager) is a logging framework for
[Erlang](http://www.erlang.org/) that is used as the logging
infrastructure for Riak. lager provides a number of configuration
options that you can use to fine-tune your Riak cluster's logging
output. A compact listing of parameters can be found in our
[[configuration files|Configuration Files#lager]] documentation. A more
thorough explanation of these options can be found in this document.

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
`erlang.log` | Logs emitted by the [[Erlang VM|Erlang VM Tuning]] on which Riak runs
`error.log` | [[Error messages]] emitted by Riak.
`run_erl.log` | The log file for an Erlang process called `run_erl`. This file can typically be ignored.

## Log Syntax

Riak logs tend to be structured in one of the following ways:

```log
<date> <time> [<level>] <PID> <prefix>: 
```

### Log File Rotation

Riak maintains multiple separate files for `console.log`, `crash.log`,
`erlang.log`, and `error.log`, which are rotated as each file reaches
its maximum capacity of 100 KB. In each node's `/log` directory, you may
see, for example, files name `console.log`, `console.log.0`,
`console.log.1`, and so on. Riak's log rotation is somewhat non
traditional, as it does not always log to `*.1` (e.g. `erlang.log.1`)
but rather to the oldest log file.

After, say, `erlang.log.1` is filled up, the logging system will begin
writing to `erlang.log.2`, and so on. When `erlang.log.5` is filled up,
it will loop back to `erlang.log.1`.

## Format

`YYYY-MM-DD HH:MM:SS.SSS [<level>] <PID>@<system>:<function> Message`

or

`YYYY-MM-DD HH:MM:SS.SSS [<level>] <PID> <message>`

## Log Files

File | Contents
:----|:--------
`console.log`
`crash.log`
`erlang.log`
`error.log`
`run_erl.log`

## SASL

[SASL](http://www.erlang.org/doc/man/sasl_app.html) (System Architecture
Support Libraries) is Erlang's built-in error logger.

## Error Messages



## Crash Logs

## Syslog

Facility levels:

Level | Meaning
:-----|:-------
`auth` |
`authpriv`
`clock`
`cron`
`daemon`
`ftp`
`kern`
`lpr`
`mail`
`news`
`syslog`
`user`
`uucp`


## Console Logs

Riak console logs can be emitted to one of three places: to a log file
(you can choose the name and location of that file), to standard output,
or to neither. This is determined by the value that you give to the
`log.console` parameter, which gives you one of four options:

* `file` --- Console logs will be emitted to a file. This is Riak's
    default behavior. The location of that file is determined by the
    `log.console.file` parameter. The default location is
    `./log/console.log` on an installation from [[source|Installing Riak
    from Source]], but will differ on platform-specific installation,
    e.g.  `/var/log/riak` on Ubuntu, Debian, CentOS, and RHEL or
    `/opt/riak/log` on Solaris-based platforms.
* `console` --- Console logs will be emitted to standard output, which
    can be viewed by running the `[[riak attach-direct|riak Command
    Line#attach-direct]]` command
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

If you'd like to enable debug logging on the current node, i.e. set the
console log level to `debug`, you can do so without restarting the node
by accessing the Erlang console directly using the `[[riak attach|riak
Command Line#attach]]` command. Once you run this command and drop into
the console, enter the following:

```erlang
lager:set_logleve(lager_file_backend, "/var/log/riak/console.log", debug).
```

You should replace the file location above (`/var/log/riak/console.log`)
with your platform-specific location, e.g. `./log/console.log` for a
source installation. This location is specified by the
`log.console.file` parameter explained [[above|lager#Console-Logs]].

If you'd like to enable debug logging on _all_ nodes instead of just one
node, you can enter the Erlang console of any running by running `riak
attach` and enter the following:

```erlang
rp(rpc:multicall(lager, set_loglevel, [lager_file_backend, "/var/log/riak/console.log", debug])).
```

As before, use the appropriate log file location for your cluster.

At any time, you can set the log level back to `info`:

```erlang
rp(rpc:multicall(lager, set_loglevel, [lager_file_backend, "/var/log/riak/console.log", info])).
```
