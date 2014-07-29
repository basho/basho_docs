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
directories specific to your installation, so make sure to adjust them
accordingly. The `add_paths` parameter, for example, assumes that Riak
CS is installed in `/usr/lib/riak-cs`, while the `data_root` parameters
assume that Riak is installed in `/var/lib`.

This configuration also assumes that the Riak CS package is installed on
the same machine as Riak. If not, the package will need to be copied on
to the same box.

<div class="note">
<div class="title">Note</div>
The path for <code>add_paths</code> may be <code>/usr/lib/riak-cs</code> or <code>/usr/lib64/riak-cs</code> [[depending on your operating system|Installing and Upgrading]].
</div>

Next, add this to the `riak_core` section of `app.config`:

```appconfig
{riak_core, [

]}
```

**For Riak nodes that support Riak CS, never set `allow_mult` to any
value other than `true`.** (As of version 1.4, Riak CS will refuse to
start if `allow_mult` is not set to `true`.)

<div class="note">
<div class="title">Note on <code>allow_mult</code> and Riak clients</div>
In Riak, the <code>allow_mult=true</code> setting is used only internally. Clients connecting to Riak CS will not need to engage in conflict resolution or deal with siblings.
</div>

{{#1.4.0+}} <div class="note"><div class="title">Note</div>As of version 1.4,
Riak CS will refuse to start if <code>allow_mult</code> is not set to <code>true</code>.</div>
{{/1.4.0+}}

1. Attempt to start Riak: `bin/riak start`
2. Test to see whether the node is running: `bin/riak ping`

If the `ping` command displays `pong`, Riak is running. If it displays `Node not responding to pings`, then something has gone wrong.

If the Riak node has not started properly, look at `log/erlang.log.1`. One common error is `invalid_storage_backend`, which indicates that the path to the Riak CS library in Riak's `app.config` is incorrect (or that Riak CS is not installed on the server). *Do not change the storage backend from `riak_cs_kv_multi_backend` to `riak_kv_multi_backend`.*

<div class="note"><div class="title">Note</div>It is important to use <code>CTRL+D</code> to detach the console and leave Riak running after doing a <code>riak attach</code>. <code>CTRL+C</code> will cause the Riak node to exit and in many cases this is not the desired outcome of detaching from the console.</div>

## Specifying the Riak IP Address
By setting the Riak IP address you ensure that your Riak nodes have unique IP addresses, whether you work with a single node or add additional nodes to the system. The Riak IP address setting resides in the Riak `vm.args` configuration file, which is located in the `/etc/riak` folder.

Initially, the line that specifies the riak node IP address is set to the local host, as follows:

```config
-name riak@127.0.0.1
```

Replace `127.0.0.1` with the IP address or hostname for the Riak node.

## Setting Up Riak to Use Protocol Buffers
The Riak Protocol Buffers settings reside in the Riak `app.config` file, which is located in the `/etc/riak` folder. The settings appear in the` riak_api` config section of the file.

{{#1.4.0-}}

* `pb_ip` --- Replace `127.0.0.1` with the IP address of the Riak node.

If you need to use a different port:

* `pb_port` --- Replace `8087` with the port number you want to use.

The `pb_ip` and `pb_port` values in the Riak `app.config` file must match the
values for `riak_ip` and `riak_pb_port` in the Riak CS and Stanchion `app.config` files.

{{/1.4.0-}}

{{#1.4.0+}}

* `pb` --- Replace `127.0.0.1` with the IP address of the Riak node:

    ```erlang
    {pb, [ {"10.11.4.203", 8087 } ]}
    ```

If you need to use a different port, replace `8087` with the port number you want to use.

The `pb` values in the Riak `app.config` file must match the values for `riak_ip` and `riak_pb_port` in the Riak CS and Stanchion `app.config` files.

{{/1.4.0+}}

<div class="note"><div class="title">Note</div>A different port number might be required if the port number conflicts with ports used by another application or you use a load balancer or proxy server.</div>

It is also recommended that you increase the size of Riak's `pb_backlog` to be greater than the size of `request_pool` specified in the Riak CS `app.config` file. At minimum, `pb_backlog` should be set to `64`. The default value is `5`.

* `pb_backlog` --- Replace the default Riak configuration, which has `pb_backlog` commented out with `%%`

    ```erlang
    %% {pb_backlog, 64},
    ```

    with the following:

    ```erlang
    {pb_backlog, 256},
    ```

If the `request_pool` value in Riak CS is changed, the `pb_backlog` value in Riak should be updated as well.

### Other Riak Settings

The `app.config` file includes other settings, such as turning on the creation of log files and specifying where to store them. These settings have default values that work in most cases.

{{#1.2.0+}}
### Performance & Capacity settings

It is strongly recommended that the following values be set in the
Riak `vm.args` configuration file, which is located in the `/etc/riak` or `/opt/riak/etc` folder:

```erlang
## This setting is not present in default Riak installations, so
## it should be added. In some cases, a value of 128000 may be
## appropriate.
+zdbbl 96000

## This setting should already be present for recent Riak installs.
-env ERL_MAX_PORTS 64000
```
{{/1.2.0+}}

### Disable JavaScript MapReduce

It is recommended that you not use JavaScript MapReduce in conjunction with _any_ version of Riak CS, and that you disable the VM that performs JavaScript MapReduce operations (for performance reasons). To do so, set the following in your `app.config`, in the `riak_kv` section:

```erlang
{map_js_vm_count, 0},
{reduce_js_vm_count, 0},
{hook_js_vm_count, 0}
```
