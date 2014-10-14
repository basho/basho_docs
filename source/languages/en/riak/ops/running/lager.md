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

Levels:

Level | Meaning
:-----|:------

## Enabling and Disabling Debug Logging


