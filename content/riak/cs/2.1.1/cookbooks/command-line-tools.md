---
title: "Riak CS Command-line Tools"
description: ""
menu:
  riak_cs-2.1.1:
    name: "Command-line Tools"
    identifier: "run_cli"
    weight: 100
    parent: "run"
project: "riak_cs"
project_version: "2.1.1"
aliases:
  - /riakcs/2.1.1/cookbooks/command-line-tools/
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

{{% note title="Deprecation notice" %}}
The `riak-cs reboot` command has been deprecated. We recommend using the
`riak-cs restart` command instead.
{{% /note %}}

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
command is successful, you can exit the shell by pressing **Ctrl-G q**.

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
exit the shell by pressing **Ctrl-G q**.

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

* `interval` specifies the number of seconds between each update of the
  `top` output and defaults to 5
* `sort` determines the category on which `riak-cs top` sorts and
  defaults to `reductions`
* `lines` specifies the number of processes to display in the `top`
  output and defaults to 10

More information about Erlang's etop tool can be found in the
[official documentation](http://www.erlang.org/doc/man/etop.html).

## riak-cs-admin gc

This command controls Riak CS's [garbage collection]({{<baseurl>}}riak/cs/2.1.1/cookbooks/garbage-collection) system.

```bash
riak-cs-admin gc <subcommand>
```

#### batch

Starts garbage collection for a batch of eligible objects.

```bash
riak-cs-admin gc batch
```

Optionally, you can specify the number of leeway seconds:

```bash
riak-cs-admin gc batch <leeway_seconds>
```

In Riak CS 2.1 you can specify the target range of GC bucket key range, with start and end timestamps. For example:

```bash
riak-cs-admin gc batch --end=20150801T000000Z
```

Or

```bash
riak-cs-admin gc batch --start=20150801T000000Z --end=20150901T000000Z
```

#### status

Returns the status of the garbage collection daemon, depending on its
current state.

```bash
riak-cs-admin gc status
```

#### pause

Pauses the current batched garbage collection process and halts any
further garbage collection until the daemon is resumed.

```bash
riak-cs-admin gc pause
```

#### resume

Resumes a paused garbage collection process. This will have no effect if
there is no previously paused process.

```bash
riak-cs-admin gc resume
```

#### cancel

Cancels the current batch of garbage collection. This will have no
effect if there is no currently running garbage collection process.

```bash
riak-cs-admin gc cancel
```

#### set-interval

Sets or updates the garbage collection interval. Expressed in terms of
seconds:

```bash
riak-cs-admin gc set-interval <interval_in_seconds>
```

#### set-leeway

Sets or updates the garbage collection leeway time, which indicates how
many seconds must elapse after an object is deleted or overwritten
before the garbage collection system may reap the object. Expressed in
seconds.

```bash
riak-cs-admin gc set-leeway <interval_in_seconds>
```

#### earliest-keys

Find oldest entry after `epoch_start` in garbage collection.

```bash
riak-cs-admin gc earliest-keys <supercluster members>
```

## riak-cs-stanchion

This command interface controls aspects of the interaction between Riak
CS and Stanchion, the access control and user management platform
undergirding Riak CS.

#### switch

Temporarily changes the host and/or port used by Stanchion. This change
is effective until the node is restarted, at which point Stanchion will
begin listening on the host and port specified in your [configuration files]({{<baseurl>}}riak/cs/2.1.1/cookbooks/configuration/reference).

```bash
riak-cs-stanchion switch HOST PORT
```

The following command would change the host to 100.0.0.1 and the port to
9999:

```bash
riak-cs-stanchion switch 100.0.0.1 9999
```

The following output would appear if the change were successful:

```
Successfully switched stanchion to 100.0.0.1:9999: This change is only effective until restart.
To make permanent change, be sure to edit app.config file.
```

#### show

Shows the current host/port address for Stanchion.

```bash
riak-cs-stanchion show
```

The output should look something like this:

```
Current Stanchion Address: http://127.0.0.1:8085
```

## riak-cs-admin storage

This command is the direct equivalent of `riak-cs-admin storage`
documented [above](#riak-cs-admin).

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

The `stanchion top` command provides information about what the Erlang
processes inside of Stanchion are doing. `top` reports process
reductions (an indicator of CPU utilization), memory used, and message
queue sizes.

```bash
stanchion top [-interval N] [-sort reductions|memory|msg_q] [-lines N]
```

Options:

* `interval` specifies the number of seconds between each update of the
  `top` output and defaults to 5
* `sort` determines the category on which `riak-cs top` sorts and
  defaults to `reductions`
* `lines` specifies the number of processes to display in the `top`
  output and defaults to 10

More information about Erlang's etop tool can be found in the
[official documentation](http://www.erlang.org/doc/man/etop.html).

## riak-cs-access

This command is the direct equivalent of `riak-cs-admin access`,
documented [above](#riak-cs-admin-access).

## riak-cs-supercluster

Riak CS version 1.5 offers support for supercluster operations. The
`supercluster` command interface enables you to interact with that system.
More information can be found in [Riak CS Supercluster Support]({{<baseurl>}}riak/cs/2.1.1/cookbooks/supercluster).

{{% note title="Note: technical preview" %}}
Riak CS supercluster support is available only as a technical preview for
users of Riak CS installations with support for Multi-Datacenter Replication.
{{% /note %}}

#### list-members

Lists the members currently available in a multi-cluster Riak CS setup.

```bash
riak-cs-supercluster list-members
```

The output will list the name, host, and port for each member, as in the
following example output:

```
sc-member-A 127.0.0.1:10017
sc-member-B 127.0.0.1:10027
# and so on
```

#### weight

When new buckets are created, they are randomly assigned to a member. The
weight of each member is the likelihood, expressed as a percentage, that
new buckets will be stored in a given member. You can use the commands
under the `weight` heading to set, list, and refresh weight information
stored in the master member (which is shared between all Riak nodes).

When the `weight` command itself is used without an argument, it will
return the weights of all members.

```bash
riak-cs-supercluster weight
```

You can also return the weight for a specific member on the basis of its
member ID:

```
riak-cs-supercluster weight <member id>
```

You can also set the weight for a member:

```bash
riak-cs-supercluster weight <member id> <weight>
```

This command would set the weight for member `sc-member-A` to 40:

```bash
riak-cs-supercluster weight sc-member-A 40
```

Weights are assigned to members as an integer. The percentage weight
applied to a given member is a function of the total weight assigned to all
members. So if you assign 30 to member A, 30 to member B, and 60 to member C, they will bear the following weights, respectively: 25%, 25%, and 50%.
Consequently, there is no specific number to which all member weights need
to add up.

#### weight-manifest

Retrieves the manifest weights for all currently available members.

```bash
riak-cs-supercluster weight-manifest
```

You can also retrieve the manifest weights for a specific member on the
basis of its member ID:

```bash
riak-cs-supercluster weight-manifest <member id>
```

You can also set the manifest weight for a specific member:

```bash
riak-cs-supercluster weight-manifest <member id> <weight>
```

#### weight-block

Retrieves the block weights for all currently available members.

```bash
riak-cs-supercluster weight-block
```

You can also retrieve the block weight for a specific member on the basis
of its member ID:

```bash
riak-cs-supercluster weight-block <member id>
```

Or you can set the weight block for a specific member:

```bash
riak-cs-supercluster weight-block <member id> <weight>
```

#### refresh

Fetches all current weights from the master member.

```bash
riak-cs-supercluster refresh
```

When a member's weight is updated, that weight is stored in the [master member]({{<baseurl>}}riak/cs/2.1.1/cookbooks/supercluster/#the-master-member) and cached in Riak CS. Riak CS fetches weights from the master member only periodically. The
`refresh` command syncs the weights stored in the master member with the
weights cached in Riak CS so that there is no discrepancy.

This command is particularly useful immediately after any member weight
changes are made that need to be registered across all clusters.
