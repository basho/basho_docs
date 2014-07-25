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

More information about Erlang's etop tool can be found in the
[official documentation](http://www.erlang.org/doc/man/etop.html).

## riak-cs-access

#### flush

This command is the direct equivalent to `riak-cs-admin access`. For
documentation, see [[the section above|Riak CS Command Line Tools#riak-cs-admin]].

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

## riak-cs-gc

This command is the direct equivalent to `riak-cs-admin gc`. For
documentation, see [[the section above|Riak CS Command Line Tools#riak-cs-admin]].

## riak-cs-stanchion

This command interface controls aspects of the interaction between Riak
CS and Stanchion, the access control and user management platform
undergirding Riak CS.

#### switch

```bash
riak-cs-stanchion switch HOST PORT
```

#### show

## riak-cs-storage

This command is the direct equivalent of `riak-cs-admin storage`
documented [[above|Riak CS Command Line Tools#riak-cs-admin]].

## stanchion

This command interface enables you to control Stanchion, the user
management and access control platform undergirding Riak CS.

#### start

Starts Stanchion in the background.

```bash
stanchion start
```

If Stanchion is already running on the node, the message `Node is
already running!` will be returned.

#### stop

Stops Stanchion on the node.

```bash
stanchion stop
```

Prints `ok` when successful or `Node <nodename> not responding to
pings` if the Stanchion node is not running.

#### restart

Stops and then starts the running Stanchion node without exiting the
Erlang VM. Prints `ok` when successful or `Node <nodename> not
responding to pings.` when the node is stopped or not responding.

```bash
stanchion restart
```

#### reboot

Stops and then restarts the running node, exiting the Erlang VM. Prints
`ok` when successful or `Node <nodename> not responding to pings.` when
the node is stopped or not responding.

```bash
stanchion reboot
```

#### ping

Checks that the Stanchion node is running. Prints `pong` when
successful or `Node <nodename> not responding to pings.` when the
Stanchion node is stopped or not responding.

```bash
stanchion ping
```

#### console

Starts the Stanchion node in the foreground, providing access to the
Erlang shell and to runtime messages.

```bash
stanchion console
```

Prints `Node is already running - use 'stanchion attach' instead` if
the node is already running in the background.

#### attach

Attaches to the console of a Stanchion node running in the background,
providing access to the Erlang shell and to runtime messages.

```bash
stanchion attach
```

Prints `Node is not running!` when the node cannot be reached.

#### attach-direct

Attaches to the console of a Stanchion node running in the background
using a directly connected first-in-first-out (FIFO), providing access
to the Erlang shell and to runtime messages.

```bash
stanchion attach-direct
```

Prints `Node is not running!` when the node cannot be reached. You can
exit the shell by typing **Ctrl-D**.

#### ertspath

Outputs the path of the Stanchion node's Erlang runtime environment.

```bash
stanchion ertspath
```

#### chkconfig

Checks whether Stanchion's configuration file is valid.

```bash
stanchion chkconfig
```

If the file is valid, `config is OK` will be returned. If not,
appropriate error messages will be returned.

#### escript

Provides a means of calling [escript](http://www.erlang.org/doc/man/escript.html)
scripts using Stanchion's Erlang runtime environment.

```bash
stanchion escript <filename>
```

#### version

Outputs the Stanchion version identifier.

```bash
stanchion version
```

#### getpid

Outputs the process identifier for the currently running instance of
Stanchion.

```bash
stanchion getpid
```

#### top


## riak-cs-multibag

Riak CS version 1.5 offers support for multibag operations. The
`multibag` command interface enables you to interact with that system.
More information can be found in [[Riak CS Multibag Support]].

<div class="note">
<div class="title">Note: technical preview</div>
Riak CS multibag support is available only as a technical preview for
users of Riak CS installations with support for Multi-Datacenter
Replication.
</div>

#### list-bags

Lists the bags currently available in a multi-cluster Riak CS setup.

```bash
riak-cs-multibag list-bags
```

The output will list the name, host, and port for each bag, as in the
following example output:

```
bag-A 127.0.0.1:10017
bag-B 127.0.0.1:10027
# and so on
```

#### weight

When new buckets are created, they are randomly assigned to a bag. The
weight of each bag is the likelihood, expressed as a percentage, that
new buckets will be stored in a given bag. You can use the commands
under the `weight` heading to set, list, and refresh weight information
stored in the master bag (which is shared between all Riak nodes).

When the `weight` command itself is used without an argument, it will
return the weights of all bags.

```bash
riak-cs-multibag weight
```

You can also return the weight for a specific bag on the basis of its
bag ID:

```
riak-cs-multibag weight <bag id>
```

You can also set the weight for a bag:

```bash
riak-cs-multibag weight <bag id> <weight>
```

This command would set the weight for bag `bag-A` to 40%:

```bash
riak-cs-multibag weight bag-A 40
```



#### weight-manifest

Retrieves the weight manifests for all currently available bags.

```bash
riak-cs-multibag weight-manifest
```

You can also retrieve the weight manifest for a specific bag on the
basis of its bag ID:

```bash
riak-cs-multibag weight-manifest <bag id>
```

You can also set the weight manifest for a bag:

```bash
riak-cs-multibag weight-manifest <bag id> <weight>
```

#### weight-block

Retrieves the weight block for all currently available bags.

```bash
riak-cs-multibag weight-block
```

You can also retrieve the weight block for a specific bag on the basis
of its bag ID:

```bash
riak-cs-multibag weight-block <bag id>
```

Or you can set the weight block for a specific bag:

```bash
riak-cs-multibag weight-block <bag id> <weight>
```

#### refresh

At any time you can refresh all weights:

```bash
riak-cs-multibag refresh
```
