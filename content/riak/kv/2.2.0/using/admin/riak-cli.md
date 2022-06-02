---
title: "riak Command Line Interface"
description: ""
project: "riak_kv"
project_version: "2.2.0"
menu:
  riak_kv-2.2.0:
    name: "riak CLI"
    identifier: "cluster_admin_riak_cli"
    weight: 102
    parent: "managing_cluster_admin"
toc: true
aliases:
  - /riak/2.2.0/ops/running/tools/riak
  - /riak/kv/2.2.0/ops/running/tools/riak
---

[configuration file]: {{<baseurl>}}riak/kv/2.2.0/configuring/reference/
[escript]: http://www.erlang.org/doc/man/escript.html
[`riak-admin`]: {{<baseurl>}}riak/kv/2.2.0/using/admin/riak-admin/#top
[configuration]: {{<baseurl>}}riak/kv/2.2.0/configuring/reference/

## riak

This is the primary script for controlling the processes associated with a Riak node. Running the `riak` command by itself will output a listing of available commands:

```bash
Usage: riak «command»
where «command» is one of the following:
    { help | start | stop | restart | ping | console | attach
      attach-direct | ertspath | chkconfig | escript | version | getpid
      top [-interval N] [-sort { reductions | memory | msg_q }] [-lines N] } |
      config { generate | effective | describe VARIABLE } [-l debug]
```

## help

Provides a brief description of all available commands.

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

## ping

Checks that the Riak node is running. Prints `pong` when successful or `Node <nodename> not responding to pings.` when the node is stopped or not responding.

```bash
riak ping
```

## console

Starts the Riak node in the foreground, giving access to the Erlang shell and
runtime messages. Prints `Node is already running - use 'riak attach' instead`
when the node is running in the background. You can exit the shell by pressing **Ctrl-C** twice.

```bash
riak console
```

## attach

Attaches to the console of a Riak node running in the background, giving access to the Erlang shell and runtime messages. Prints `Node is not running!` when the node cannot be reached.

```bash
riak attach
```

## attach-direct

Attaches to the console of a Riak running in the background using a directly-connected first-in-first-out (FIFO), providing access to the Erlang shell and runtime messages. Prints `Node is not running!` when the node cannot be reached. You can exit the shell by pressing **Ctrl-D**.

```bash
riak attach-direct
```

## ertspath

Outputs the path of the Riak Erlang runtime environment:

```bash
riak ertspath
```

## chkconfig

Checks whether the [configuration file][configuration file] is valid. If so, `config is OK` will be included in the output.

```bash
riak chkconfig
```

## escript

Provides a means of calling [escript][escript] scripts using the Riak Erlang runtime environment:

```bash
riak escript <filename>
```

## version

Outputs the Riak version identifier:

```bash
riak version
```

## getpid

Outputs the process identifier for the currently-running instance of Riak:

```bash
riak getpid
```

## top

The `riak top` command is the direct equivalent of `riak-admin top`:

```bash
riak top [-interval N] [-sort { reductions | memory | msg_q }] [-lines N] }
```

More detailed information can be found in the [`riak-admin`][`riak-admin`] documentation.

## config

Provides information about the current [configuration][configuration] of a Riak node, i.e. the parameters and values in the node's riak.conf configuration.

```bash
riak config { generate | effective | describe VARIABLE } [-l debug]
```

* `generate` will cause the configuration files to be re-processed.  This behavior happens automatically at node startup; however `riak config generate` can be used to test for configuration errors that would prevent the node from starting after modifying the riak.conf or advanced.config files.
    The output of a successful run will show the paths to the newly generated configuration files.  These configuration files will contain a timestamp to indicate when they were generated.  For example:

    ```
     -config /var/lib/riak/generated.configs/app.2016.12.02.17.47.32.config -args_file /var/lib/riak/generated.configs/vm.2016.12.02.17.47.32.args -vm_args /var/lib/riak/generated.configs/vm.2016.12.02.17.47.32.args  
    ```

    If you are using the legacy configuration file format (app.config/vm.args), you will receive the following message:

    ```
    -config /etc/riak/app.config -args_file /etc/riak/vm.args -vm_args /etc/riak/vm.args  
    ```

* `effective` prints the effective configuration in the following syntax:
    
    ```
    parameter1 = value1
    parameter2 = value2
    ```

    If you are using the legacy configuration file format (app.config/vm.args), you will receive the following error:

    ```
    Disabling cuttlefish, legacy configuration files found:
      /etc/riak/app.config
      /etc/riak/vm.args
    Effective config is only visible for cuttlefish conf files. 
    ```

* `describe VARIABLE` prints the setting specified by `VARIABLE`, along with documentation and other useful information, such as the affected location in the configuration file, the data type of the value, the default value, and the effective value. For example, running `riak config describe storage_backend` will return the following:
    
    ```
    Documentation for storage_backend
    Specifies the storage engine used for Riak's key-value data
    and secondary indexes (if supported).

       Valid Values: 
         - one of: bitcask, leveldb, memory, multi, prefix_multi
       Default Value : bitcask
       Set Value     : bitcask
       Internal key  : riak_kv.storage_backend 
    ```

Adding the `-l debug` flag to any `riak config` command will produce additional debugging information that can be used in advanced troubleshooting of "cuttlefish", Riak's configuration subsystem.
