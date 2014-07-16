---
title: Riak CS Command Line Tools
project: riakcs
version: 1.5.0+
document: cookbook
toc: true
audience: intermediate
keywords: [command-line, riak-cs]
---

Riak CS comes equipped with a variety of command-line interfaces that you can use to manage each node in your Riak CS cluster. The scripts for these commands are available by default in the `/bin` directory of each node.

## riak-cs

This is the primary script for controlling the processes associated with a Riak CS node. Running the `riak-cs` command by itself will output a listing of available commands:

```
Usage: riak-cs {start | stop | restart | reboot | ping | console | attach | 
                    attach-direct | ertspath | chkconfig | escript | version | 
                    getpid | top [-interval N] [-sort reductions|memory|msg_q] [-lines N] }
```

#### start

Starts the Riak CS node.

```bash
riak-cs start
```

If starting the node is successful, you will see no return output. If the node is already running, this command will return `Node is already running!`.

#### stop

Stops the Riak CS node.

```bash
riak-cs stop
```

This command will print `ok` if the stoppage is successful.

If you attempt to run `riak-cs stop` on a node that is not currently running, you will see the following:

```
Node <nodename> not responding to pings.
Node is not running!
```

#### restart

Stops and then starts a running Riak CS node without exiting the Erlang VM.

```bash
riak-cs restart
```

Prints `ok` when successful. If the node is already stopped or not responding, you will see the following output:

```
Node <nodename> not responding to pings.
```

#### reboot

Stops all applications and starts without restarting the Erlang VM.

```bash
riak-cs reboot
```

<div class="note">
<div class="title">Deprecation notice</div>
The <code>riak-cs reboot</code> command has been deprecated. We recommend using the <code>riak-cs restart</code> command instead.
</div>

#### ping

Checks whether the Riak CS node is currently running.

```bash
riak-cs ping
```

Prints `pong` when the node is running or `Node <nodename> not responding to pings` when the node is stopped or not responding.

#### console

Starts the Riak CS node in the foreground, providing direct access to the node via the Erlang shell.

```bash
riak-cs console
```

If the node is already running in the background, you will see the output `Node is already running - use 'riak-cs attach' instead`. If the command is successful, you can exit the shell by pressing **Ctrl-C** twice.

#### attach

Attaches to the console of a Riak CS node running in the background, providing access to the Erlang shell and runtime messages.

```bash
riak-cs attach
```

Prints `Node is not running!` when the node cannot be reached.

#### attach-direct

#### ertspath

#### chkconfig

#### escript

#### version

#### getpid

#### top

## riak-cs-access

#### flush

## riak-cs-admin

#### gc

#### access

#### storage

#### stanchion

#### cluster-info

## riak-cs-debug

## riak-cs-gc

#### batch

#### status

#### pause

#### resume

#### cancel

#### set-interval

#### set-leeway

## riak-cs-multibag

#### list-bags

#### weight

#### weight-manifest

#### weight-block

#### refresh

## riak-cs-stanchion

#### switch

#### show

## riak-cs-storage

#### batch

#### status

#### pause

#### resume

#### cancel