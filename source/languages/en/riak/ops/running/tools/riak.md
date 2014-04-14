---
title: riak Command Line
project: riak
version: 0.10.0+
document: reference
toc: true
audience: beginner
keywords: [command-line, riak]
moved: {
    '1.4.0-': '/references/Command-Line-Tools---riak'
}
---

# Command Line Tools - `riak`

This is the primary script for controlling the Riak node process.

```bash
Usage: riak { start | stop | restart | reboot | ping | console | attach | chkconfig }
```

## start

Starts the Riak node in the background. If the node is already started, you will receive the message `Node is already running!` If the node is not already running, no output will be given.

```bash
riak start
```

## stop

Stops the running Riak node. Prints `ok` when successful or `Node <nodename> not responding to pings.` when the node is already stopped or not responding.

```bash
riak stop
```

## restart

Stops and then starts the running Riak node without exiting the Erlang VM.
Prints `ok` when successful, `Node <nodename> not responding to pings.` when the node is already stopped or not responding.

```bash
riak restart
```

## reboot

Stops and then starts the running Riak node, exiting the Erlang VM. Prints `ok` when successful or `Node <nodename> not responding to pings.` when the node is already stopped or not responding.

```bash
riak reboot
```

## ping

Checks that the Riak node is running. Prints `pong` when successful or `Node <nodename> not responding to pings.` when the node is stopped or not responding.

```bash
riak ping
```

## console

Starts the Riak node in the foreground, giving access to the Erlang shell and
runtime messages. Prints `Node is already running - use 'riak attach' instead`
when the node is running in the background.

```bash
riak console
```

## attach

Attaches to the console of a Riak node running in the background, giving access to the Erlang shell and runtime messages. Prints `Node is not running!` when the node cannot be reached.

```bash
riak attach
```

## chkconfig

Checks whether the {{#2.0.0-}}`app.config`{{/2.0.0-}}{{#2.0.0+}}`riak.conf`{{/2.0.0+}} configuration file is valid. If so, `config is OK` will be included in the output.

```bash
riak chkconfig
```
