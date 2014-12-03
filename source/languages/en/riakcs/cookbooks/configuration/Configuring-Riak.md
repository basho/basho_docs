---
title: Configuring Riak for CS
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, configuration]
---

The default backend used by Riak is the Bitcask backend, but the Riak CS
package includes a special backend that should be used by the Riak
cluster that is part of the Riak CS system. It is a custom version of
the standard [[Multi]] backend that ships with Riak.

Some of the Riak buckets used internally by Riak CS use secondary
indexes, which currently requires the [[LevelDB]] backend. Other parts
of the Riak CS system can benefit from the use of the Bitcask backend.
The use of the custom [[Multi]] backend enables Riak CS to take
advantage of the strengths of both of these backends to achieve the best
blend of performance and features. The next section covers how to
properly set up Riak to use this multi backend.

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

First, edit Riak's `app.config` [[configuration file|Configuration
Files]]. The `app.config` file can be found in the `/etc/riak` or
`/opt/riak/etc` directory. By default, Riak uses the [[Bitcask]]
backend. The first thing we need to do is to change that by removing the
following line:

```appconfig
{riak_kv, [
    %% Delete this line:
    {storage_backend, riak_kv_bitcask_backend},
]}
```

Next, we need to expose the necessary Riak CS modules to Riak and to
instruct Riak to use a the custom backend used by Riak CS. In the same
`riak_kv` section from which we deleted the line shown above, we need
to insert the following section:

```appconfig
{riak_kv, [
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
]}
```

It's important to note that many of these values will depend on various
directories specific to your [[operating system|Installing and
Upgrading]], so make sure to adjust them accordingly. The `add_paths`
parameter, for example, assumes that Riak CS is installed in
`/usr/lib/riak-cs`, while the `data_root` parameters assume that Riak is
installed in `/var/lib`.

This configuration also assumes that the Riak CS package is installed on
the same machine as Riak. If not, the package will need to be copied on
to the same box.

## Allowing for Sibling Creation

Now, we need to set `allow_mult` to `true`. We can add this line to the
`riak_core` section of `app.config`:

```appconfig
{riak_core, [
    %% Other configs

    {default_bucket_props, [{allow_mult, true}]},

    %% Other configs
]}
```

This will enable Riak to create [[siblings|Vector Clocks#siblings]],
which is necessary for Riak CS to function. If you are connecting to
Riak CS from a [[client library|Client Libraries]], don't worry: you
will not have to manage conflict resolution, as all Riak CS operations
are strongly consistent.

<div class="note">
<div class="title">Note on <code>allow_mult</code></div>
Any Riak node that also supports Riak CS should have
<code>allow_mult</code> set to <code>true</code> at all times. Riak CS
will refuse to start if <code>allow_mult</code> is set to
<code>false</code>.
</div>

## Specifying the Nodename and IP Address

Every Riak node has a name that can be specified in `vm.args`, using
the `-name` flag. The `vm.args` configuration file can be found in the
same directory as your `app.config` file (which varies based on
operating system). We recommend providing nodes a name of the form
`<name>@<host>`. So if you have three nodes running on the host
`100.0.0.1`, you could name them `riak1@100.0.0.1`, `riak2@100.0.0.1`,
and `riak3@100.0.0.1`, or you could give them names that are more
specific, such as `test_cluster1@100.0.0.1`, `user_data3@100.0.0.1`, and
so on. The example below demonstrates changing a node's name to
`riak1@127.0.0.1`, which would work for a node running on `localhost`:

```vmargs
-name riak1@127.0.0.1
```

You should name _all_ nodes prior to starting them and connecting them
to a cluster.

## Testing the Configuration

Now that the necessary changes have been made to the Riak node's
`app.config`, we can attempt to start Riak:

```bash
bin/riak start
```

This could take a second. We can then test whether the node is running:

```bash
bin/riak ping
```

If the response is `pong`, then Riak is running; if the response is
`Node not responding to pings`, then something has gone wrong.

If the node has not started properly, look at `log/erlang.log.1` to see
if the problem can be identified. One common error is
`invalid_storage_backend`, which indicates that the path to the Riak CS
library in `app.config` is incorrect (or that Riak CS is not installed
on the server). In spite of this error, make sure that you do not change
the backend from `riak_cs_kv_multi_backend` to `riak_kv_multi_backend`.

## Specifying the Riak IP Address

By setting the Riak IP address you ensure that your Riak nodes have
unique IP addresses, whether you're working with a single node or adding
additional nodes to the system. The Riak IP address setting resides in
the Riak `vm.args` configuration file, which is located in the same
`/etc/riak` folder as `app.config` (or in `/opt/riak/etc` on some
operating systems).

Initially, the line that specifies the riak node IP address is set to
the local host, as follows:

```vmargs
-name riak@127.0.0.1
```

Replace `127.0.0.1` with the IP address or hostname for the Riak node.

## Setting Up Riak to Use Protocol Buffers

The Riak [[Protocol Buffers|PBC API]] settings reside in the Riak
`app.config` file, which is located in the `/etc/riak` folder. The
settings appear in the `riak_api` section of the file.

First, replace `127.0.0.1` with the IP address of the Riak node and
`8087` with the port:

```appconfig
{riak_api, [
    %% Other configs

    {pb, ["<IP for your node>", <port>]},

    %% Other configs
]}
```

**Note**: The `pb` values in the Riak `app.config` file must match the
values for `riak_ip` and `riak_pb_port` in the Riak CS and Stanchion
`app.config` files.

<div class="note">
<div class="title">Note on port numbers</div>
A different port number might be required if the port number conflicts
with ports used by another application or if you use a load balancer or
proxy server.
</div>

It is also recommended that you increase the size of Riak's `pb_backlog`
to be greater than the size of `request_pool` specified in the Riak CS
`app.config` file. At minimum, `pb_backlog` should be set to `64`. The
default value is `5`. To do so, replace the `{pb_backlog, 5}` line
with this:

```appconfig
{riak_api, [
    %% Other configs

If you need to use a different port, replace `8087` with the port number you want to use.

    %% Other configs
]}
```

If the `request_pool` value in Riak CS is changed, the `pb_backlog`
value in Riak should be updated as well.

## Other Riak Settings

The `app.config` file includes other settings, such as turning on the
creation of log files and specifying where to store them. These
settings have default values that work in most cases.

### Performance and Capacity settings

For performance reasons, we strongly recommended that you inert the
following values be set in the Riak `vm.args` configuration file,
located in the `/etc/riak` or `/opt/riak/etc` folder:

```vmargs
## This setting should already be present for recent Riak installs.
-env ERL_MAX_PORTS 64000
```

### Disable JavaScript MapReduce

It is recommended that you not use the now-deprecated JavaScript
MapReduce in conjunction with _any_ version of Riak CS. For performance
reasons, you should disable the VM that performs JavaScript MapReduce
operations by setting the following in the `riak_kv` section of your
`app.config`:

```erlang
{riak_kv, [
    %% Other configs

    {map_js_vm_count, 0},
    {reduce_js_vm_count, 0},
    {hook_js_vm_count, 0}

    %% Other configs
]}
```
