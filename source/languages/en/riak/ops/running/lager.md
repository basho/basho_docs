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
    `./log/console.log`.
* `console` --- Console logs will be emitted to standard output, which
    can be viewed by running the `[[riak attach-direct|riak Command
    Line#attach-direct]]` command
* `both` --- Console logs will be emitted both to a file and to standard
    output
* `off` --- Console log messages will be disabled

In addition to the the placement of console logs, you can also choose
the severity of those messages from the following four options:

* `info` (the default)
* `debug`
* `warning`
* `error`

## Enabling and Disabling Debug Logging


