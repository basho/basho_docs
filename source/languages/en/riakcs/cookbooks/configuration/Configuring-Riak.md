---
title: Configuring Riak for CS
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, configuration]
---

Because Riak CS is an application built on top of Riak, it's important
to pay special attention to your Riak configuration when running Riak
CS. This document is both a tutorial on Riak configuration as well as a
reference document listing important configurable parameters.

## The Proper Backends for Riak CS

The default backend used by Riak is the [[Bitcask]] backend, but the
Riak CS package includes a special backend that should be used by the
Riak cluster that is part of the Riak CS system. It is a custom version
of the standard [[Multi]] backend that ships with Riak.

Some of the Riak buckets used internally by Riak CS use secondary
indexes, which currently requires the [[LevelDB]] backend. Other parts
of the Riak CS system can benefit from the use of the Bitcask backend.
The use of the custom [[Multi]] backend enables Riak CS to take
advantage of the strengths of both of these backends to achieve the best
blend of performance and features. The next section covers how to
properly set up Riak to use this Multi backend.

Additionally, the Riak CS storage calculation system uses Riak's
[[MapReduce|Using MapReduce]] to sum the files in a bucket. This means
that you must tell all of your Riak nodes where to find Riak CS's
compiled files before calculating storage.

A few other settings must be modified to configure a Riak node as part
of a Riak CS system, such as the node IP address and the IP address and
port to use for communicating through Protocol Buffers. Other settings
can be modified if necessary. The following sections describe how to
configure a Riak node to work as part of a Riak CS system.

## Setting up the Proper Riak Backend

First, edit Riak's `riak.conf`, or the old-style `advanced.config` or
`app.config` [[configuration file|Configuration Files]]. These files can be found
in the `/etc/riak` or `/opt/riak/etc` directories. By default, Riak uses the
[[Bitcask]] backend. The first thing we need to do is to change that by removing
the following line:

```riakconf
## Delete this line:
storage_backend = bitcask
```

```advancedconfig
{riak_kv, [
    %% Delete this line:
    {storage_backend, riak_kv_bitcask_backend},
]}
```

```appconfig
{riak_kv, [
    %% Delete this line:
    {storage_backend, riak_kv_bitcask_backend},
]}
```

Next, we need to expose the necessary Riak CS modules to Riak and instruct Riak
to use the custom backend provided by Riak CS. We need to use either the
`advanced.config` or `app.config` file and insert the following options:

```advancedconfig
{riak_kv, [
    %% Other configs
    {add_paths, ["/usr/lib/riak-cs/lib/riak_cs-{{VERSION}}/ebin"]},
    {storage_backend, riak_cs_kv_multi_backend},
    {multi_backend_prefix_list, [{<<"0b:">>, be_blocks}]},
    {multi_backend_default, be_default},
    {multi_backend, [
        {be_default, riak_kv_eleveldb_backend, [
            {max_open_files, 50},
            {data_root, "/var/lib/riak/leveldb"}
        ]},
        {be_blocks, riak_kv_bitcask_backend, [
            {data_root, "/var/lib/riak/bitcask"}
        ]}
    ]},
    %% Other configs
]}
```

```appconfig
{riak_kv, [
    %% Other configs
    {add_paths, ["/usr/lib/riak-cs/lib/riak_cs-{{VERSION}}/ebin"]},
    {storage_backend, riak_cs_kv_multi_backend},
    {multi_backend_prefix_list, [{<<"0b:">>, be_blocks}]},
    {multi_backend_default, be_default},
    {multi_backend, [
        {be_default, riak_kv_eleveldb_backend, [
            {max_open_files, 50},
            {data_root, "/var/lib/riak/leveldb"}
        ]},
        {be_blocks, riak_kv_bitcask_backend, [
            {data_root, "/var/lib/riak/bitcask"}
        ]}
    ]},
    %% Other configs
]}
```

It's important to note that many of these values will depend on various
directories specific to your [[operating system|Installing and
Upgrading]], so make sure to adjust them accordingly. The `add_paths`
parameter, for example, assumes that Riak CS is installed in
`/usr/lib/riak-cs`, while the `data_root` parameters assume that Riak is
installed in `/var/lib/`.

This configuration also assumes that the Riak CS package is installed on
the same machine as Riak. If not, the package will need to be copied
onto the same box.

## Allowing for Sibling Creation

Now, we need to set the `allow_mult` parameter to `true`. We can add this line
to the either the `riak.conf` configuration file, or to the `riak_core` section
of old-style `advanced.config` or `app.config` files:

```riakconf
buckets.default.allow_mult = true
```

```advancedconfig
{riak_core, [
    %% Other configs
    {default_bucket_props, [{allow_mult, true}]},
    %% Other configs
]}
```

```appconfig
{riak_core, [
    %% Other configs
    {default_bucket_props, [{allow_mult, true}]},
    %% Other configs
]}
```

This will enable Riak to create [[siblings|Causal Context#Siblings]],
which is necessary for Riak CS to function. If you are connecting to
Riak CS from a [[client library|Client Libraries]], don't worry: you
will not have to manage [[conflict resolution]], as all Riak CS
operations are strongly consistent by definition.

<div class="note">
<div class="title">Note on <code>allow_mult</code></div>
Any Riak node that also supports Riak CS should have `allow_mult` set to
`true` at all times. Riak CS will refuse to start if `allow_mult` is
set to `false`.
</div>

## Specifying the Nodename and IP Address

Every Riak node has a name that can be specified in `riak.conf` using the
`nodename` option. If you are using the old-style `app.config` configuration
file, you will need to create a file named `vm.args` in the same directory as
the `app.config` file, and set the node name using the `-name` flag. We
recommend providing nodes a name of the form `<name>@<host>`. So if you have
three nodes running on the host `100.0.0.1`, you could name them
`riak1@100.0.0.1`, `riak2@100.0.0.1`, and `riak3@100.0.0.1`, or you could give
them names that are more specific, such as `test_cluster1@100.0.0.1`,
`user_data3@100.0.0.1`, and so on. The example below demonstrates changing a
node's name to `riak1@127.0.0.1`, which would work for a node running on
`localhost`:

```riakconf
nodename = riak1@127.0.0.1
```

```vmargs
-name riak1@127.0.0.1
```

You should name _all_ nodes prior to starting them and connecting them
to a cluster.

## Testing the Configuration

Now that the necessary changes have been made to the Riak node's configuration,
we can attempt to start Riak:

```bash
riak start
```

This could take a second. We can then test whether the node is running:

```bash
riak ping
```

If the response is `pong`, then Riak is running; if the response is
`Node not responding to pings`, then something has gone wrong.

If the node has not started properly, look at the `erlang.log.1` in the
`/log` directory of the node to see if the problem can be identified.
One common error is `invalid_storage_backend`, which indicates that the
path to the Riak CS library in `advanced.config` or in `app.config` is incorrect
(or that Riak CS is not installed on the server). In spite of this error, make
sure that you do not change the backend from `riak_cs_kv_multi_backend` to
`riak_kv_multi_backend`.

## Setting Up Riak to Use Protocol Buffers

The Riak [[Protocol Buffers|PBC API]] settings reside in the Riak `riak.conf`,
or in the `riak_api` section of the the old-style `advanced.config` or
`app.config` files, which is located in the `/etc/riak/` folder. The default host
is `127.0.0.1` and the default port is `8087`. You will need to change this if
you plan on running Riak and Riak CS in a non-local environment. Replace
`127.0.0.1` with the IP address of the Riak node and `8087` with the appropriate
port:

```riakconf
listener.protobuf.internal = 10.0.2.10:10001
```

```advancedconfig
{riak_api, [
    %% Other configs
    {pb, ["10.0.2.10", 10001]},
    %% Other configs
]}
```

```appconfig
{riak_api, [
    %% Other configs
    {pb, ["10.0.2.10", 10001]},
    %% Other configs
]}
```

**Note**: The `listener.protobuf.internal` values in the Riak `riak.conf` (or
the `pb` value in `advanced.config`/`app.config`) file must match the values for
`riak_host`in the Riak CS and Stanchion `riak-cs.config` (or `riak_ip` and
`riak_pb_port` in `advanced.config`/`app.config`) files.

<div class="note">
<div class="title">Note on port numbers</div>
A different port number might be required if the port number conflicts
with ports used by another application or if you use a load balancer or
proxy server.
</div>

It is also recommended that users insure that the size of Riak's
`protobuf.backlog` (or in the `advanced.config`/`app.config` files, the
`pb_backlog`) is equal to or greater than the size of the
`pool.request.size`, specified in the Riak CS `riak-cs.conf` (or
the `request_pool` size in the `advanced.config`/`app.config` files).

If the `pool.request.size` value in Riak CS is changed, the `protobuf.backlog`
value in Riak should be updated as well.

## Other Riak Settings

The `riak.conf` and `advanced.config` files includes other settings, such as
turning on the creation of log files and specifying where to store them. These
settings have default values that should work in most cases. For more
information, we recommend reading our [configuration files][riak_conf_files]
documentation.

## Specifying the Riak IP Address

By setting the Riak IP address you ensure that your Riak nodes have unique IP
addresses, whether you're working with a single node or adding additional nodes
to the system. The Riak IP address setting resides in the Riak `riak.conf` or
-- if you're using the `app.config` file -- in the `vm.args` configuration file,
which is located in the same `/etc/riak/` directory (or in `/opt/riak/etc/` on
some operating systems).

Initially, the line that specifies the riak node IP address is set to the local
host, as follows:

```riakconf
nodename = riak@127.0.0.1
```

```vmargs
-name riak@127.0.0.1
```

Replace `127.0.0.1` with the appropriate IP address or hostname for the Riak
node.

### Performance and Capacity settings

For performance reasons, we strongly recommended that you insert the following
values into Riak's `riak.conf`, or the old-style `vm.args`, configuration file,
located in the `/etc/riak` or `/opt/riak/etc` folder:

```riakconf
erlang.max_ports = 65536
```

```vmargs
## This setting should already be present for recent Riak installs.
-env ERL_MAX_PORTS 65536
```

### Disable JavaScript MapReduce

It is recommended that you not use the now-deprecated JavaScript MapReduce in
conjunction with _any_ version of Riak CS. For performance reasons, you should
disable the VM that performs JavaScript MapReduce operations by setting the
following in the `riak.conf` configuration file, or the `riak_kv` section of the
old-style `advanced.config` or `app.config`:

```riakconf
javascript.map_pool_size = 0
javascript.reduce_pool_size = 0
javascript.hook_pool_size = 0
```

```advancedconfig
{riak_kv, [
    %% Other configs
    {map_js_vm_count, 0},
    {reduce_js_vm_count, 0},
    {hook_js_vm_count, 0}
    %% Other configs
]}
```

```appconfig
{riak_kv, [
    %% Other configs
    {map_js_vm_count, 0},
    {reduce_js_vm_count, 0},
    {hook_js_vm_count, 0}
    %% Other configs
]}
```


[riak_conf_files]: http://docs.basho.com/riak/2.0.5/ops/advanced/configs/configuration-files/
