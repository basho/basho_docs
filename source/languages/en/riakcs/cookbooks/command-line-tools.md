---
title: Riak CS Command Line Tools
project: riakcs
version: 1.5.0+
document: cookbook
toc: true
audience: intermediate
keywords: [command-line, riak-cs]
---

Riak CS comes equipped with a variety of command-line interfaces that 
you can use to manage each node in your Riak CS cluster. The scripts for 
these commands are available by default in the `/bin` directory of each 
node.

## riak-cs

This is the primary script for controlling the processes associated with 
a Riak CS node. Running the `riak-cs` command by itself will output a 
listing of available commands:

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

If starting the node is successful, you will see no return output. If 
the node is already running, this command will return `Node is already 
running!`.

#### stop

Stops the Riak CS node.

```bash
riak-cs stop
```

This command will print `ok` if the stoppage is successful.

If you attempt to run `riak-cs stop` on a node that is not currently 
running, you will see the following:

```
Node <nodename> not responding to pings.
Node is not running!
```

#### restart

Stops and then starts a running Riak CS node without exiting the Erlang 
VM.

```bash
riak-cs restart
```

Prints `ok` when successful. If the node is already stopped or not 
responding, you will see the following output:

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
The <code>riak-cs reboot</code> command has been deprecated. We 
recommend using the <code>riak-cs restart</code> command instead.
</div>

#### ping

Checks whether the Riak CS node is currently running.

```bash
riak-cs ping
```

Prints `pong` when the node is running or `Node <nodename> not 
responding to pings` when the node is stopped or not responding.

#### console

Starts the Riak CS node in the foreground, providing direct access to 
the node via the Erlang shell.

```bash
riak-cs console
```

If the node is already running in the background, you will see the 
output `Node is already running - use 'riak-cs attach' instead`. If the 
command is successful, you can exit the shell by pressing **Ctrl-C** 
twice.

#### attach

Attaches to the console of a Riak CS node running in the background, 
providing access to the Erlang shell and to runtime messages.

```bash
riak-cs attach
```

Prints `Node is not running!` when the node cannot be reached.

#### attach-direct

Attaches to the console of a Riak CS node running in the background 
using a directly connected first-in-first-out (FIFO), providing access 
to the Erlang shell and to runtime messages.

```bash
riak-cs attach-direct
```

Prints `Node is not running!` when the node cannot be reached. You can 
exit the shell by pressing **Ctrl-D**.

#### ertspath

Outputs the path of Riak CS's Erlang runtime environment.

```bash
riak-cs ertspath
```

#### chkconfig

Checks whether the Riak CS nodes configuration files are valid.

```bash
riak-cs chkconfig
```

If the files are valid, `config is OK` will be included in the output.

#### escript

Provides a means of calling [escript](http://www.erlang.org/doc/man/escript.html)
scripts using Riak CS's Erlang runtime environment.

```bash
riak-cs escript <filename>
```

#### version

Outputs the Riak CS version identifier.

```bash
riak-cs version
```

#### getpid

Outputs the process identifier for the currently running instance of
Riak CS.

```bash
riak-cs getpid
```

#### top

The `riak-cs top` command provides information about what the Erlang
processes inside of Riak CS are doing. `top` reports process reductions
(an indicator of CPU utilization), memory used, and message queue sizes.

```bash
riak-cs top [-interval N] [-sort reductions|memory|msg_q] [-lines N]
```

Options:

* `interval` specifies the number of seconds between each update of the `top` output and defaults to 5
* `sort` determines the category on which `riak-cs top` sorts and defaults to `reductions`
* `lines` specifies the number of processes to display in the `top` output and defaults to 10

More information about Erlang's etop can be found in the
[etop documentation](http://www.erlang.org/doc/man/etop.html).

## riak-cs-access

#### flush

```bash
riak-cs flush [-w N]
```

Options:

* `w` --- Specifies the number of retries. Defaults to 10.

## riak-cs-admin

#### gc

```bash
riak-cs-admin gc [subcommand]
```

Subcommands:

* `batch [<leeway_seconds>]`
* `status`
* `pause`
* `resume`
* `cancel`
* `set-interval <interval_seconds>`
* `set-leeway <leeway_seconds>`

#### access

```bash
riak-cs-admin access [subcommand]
```

Subcommands:

* `flush`

#### storage

```bash
riak-cs-admin storage [subcommand]
```

Subcommands:

* `batch`
* `status`
* `pause`
* `resume`
* `cancel`

#### stanchion

```bash
riak-cs-admin stanchion [subcommand]
```

Subcommands:

* `switch HOST PORT`
* `show`

#### cluster-info

```bash
riak-cs-admin cluster-info <output_file>
```

## riak-cs-debug

```bash
riak-cs-debug
```

## riak-cs-gc

This command is the direct equivalent to `riak-cs-admin gc`. For
documentation, see [[the section above|Riak CS Command Line Tools#riak-cs-admin]].

## riak-cs-multibag

#### list-bags

```bash
riak-cs-multibag list-bags
```

#### weight

```bash
riak-cs-multibag weight
```

#### weight-manifest

```bash
riak-cs-multibag weight-manifest
```

#### weight-block

```bash
riak-cs-multibag weight-block
```

#### refresh

```bash
riak-cs-multibag refresh
```

## riak-cs-stanchion

#### switch

```bash
riak-cs-stanchion switch HOST PORT
```

#### show

## riak-cs-storage

This command is the direct equivalent to `riak-cs-admin storage`.