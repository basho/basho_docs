---
title: Configuring Riak for CS
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, configuration]
---

The default backend used by Riak is the Bitcask backend, but the Riak CS package includes a special backend that should be used by the Riak cluster that is part of the Riak CS system. It is a custom version of the standard multi backend that ships with Riak.

Some of the Riak buckets used internally by Riak CS use secondary indexes, which currently requires the [[eLevelDB|LevelDB]] backend. Other parts of the Riak CS system can benefit from the use of the Bitcask backend. The use of the custom multi backend enables Riak CS to take advantage of the strengths of both of these backends to achieve the best blend of performance and features. The next section covers how to properly set up Riak to use this multi backend.

Additionally, the Riak CS storage calculation system uses Riak's MapReduce to sum the files in a bucket. This means that you must tell all of your Riak nodes where to find Riak CS's compiled files before calculating storage.

A few other settings must be modified to configure a Riak node as part of a Riak CS system, such as the node IP address and the IP address and port to use for communicating through Protocol Buffers. Other settings can be modified if necessary. The following sections describe how to configure a Riak node to work as part of a Riak CS system.

## Setting up the Proper Riak Backend

First, edit Riak's `app.config` file and find and delete the line containing the `storage_backend` property in the `riak_kv` section. The `app.config` file can be found in the `/etc/riak` or `/opt/riak/etc` directory. The default setting is for the Bitcask backend and would look like this:

```erlang
{storage_backend, riak_kv_bitcask_backend},
```

Next, expose the necessary Riak CS modules to Riak and instruct Riak to use the custom multi backend. Continue editing Riak's `app.config` file, and add this to the `riak_kv` section:

```erlang
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
```

where **X.Y.Z** is the version of Riak CS that you have installed.

This assumes Riak and Riak CS packages are installed on the same machine. If the Riak CS package is not installed on the Riak box, then the files `riak-cs-machine:/usr/lib/riak-cs/lib/riak_cs-X.Y.Z/ebin/*` must be copied to the Riak box, with the copy destination added to the `add_paths` directive.

<div class="note"><div class="title">Note</div>The path for <code>add_paths</code> may be <code>/usr/lib/riak-cs</code> or <code>/usr/lib64/riak-cs</code> [[depending on your operating system|Installing and Upgrading]].</div>

Next, add this to the `riak_core` section of `app.config`:

```erlang
{default_bucket_props, [{allow_mult, true}]},
```

You should never set `allow_mult` to any value other than `true`. If this is not set to `true`, certain writes will be chosen arbitrarily by timestamp,
potentially leading to data loss and other inconsistencies.

<div class="note">
<div class="title">Note on <code>allow_mult</code> and Riak clients</div>
In Riak, the <code>allow_mult=true</code> setting is used only internally. Clients connecting to Riak CS will not need to engage in conflict resolution or deal with siblings.
</div>

{{#1.4.0+}} <div class="note"><div class="title">Note</div>As of version 1.4,
Riak CS will refuse to start if <code>allow_mult</code> is not set to <code>true</code>.</div>
{{/1.4.0+}}

Save and exit the editing session on the `app.config` file. To test that you have configured a Riak node correctly, start Riak and connect to its console (using `riak attach`), then run:

```erlang
(riak@127.0.0.1)1> code:which(riak_cs_kv_multi_backend).
"/usr/lib64/riak-cs/lib/riak_cs-X.Y.Z/ebin/riak_cs_kv_multi_backend.beam"
```

If the path that you added to Riak's `app.config` is returned, your node is configured correctly. If the atom `non_existing` is returned instead, then Riak was unable to find the Riak CS code.

<div class="note"><div class="title">Note</div>It is important to use <code>CTRL+D</code> to detach the console and leave Riak running after doing a <code>riak attach</code>. <code>CTRL+C</code> will cause the Riak node to exit and in many cases this is not the desired outcome of detaching from the console.</div>

## Specifying the Riak IP Address
By setting the Riak IP address you ensure that your Riak nodes have unique IP addresses, whether you work with a single node or add additional nodes to the system. The Riak IP address setting resides in the Riak `vm.args` configuration file, which is located in the `/etc/riak` folder.

Initially, the line that specifies the riak node IP address is set to the local host, as follows:

```config
-name riak@127.0.0.1
```

Replace `127.0.0.1` with the IP address for the Riak node.

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

It is also recommended that you increase the size of Riak's `pb_backlog`
to be greater than or equal to the size of `request_pool` + the size of
the `bucket_list_pool` specified in the Riak CS `app.config` file.
Assuming the default sizes for each of the connection pools,
`pb_backlog` should be set to at least `133`, but using a value of `256`
is recommended to provide more leeway. The default value is `5`.

* `pb_backlog` --- Replace the default Riak configuration, which has `pb_backlog` commented out with `%%`:

    ```erlang
    %% {pb_backlog, 64},
    ```

    with the following:

    ```erlang
    {pb_backlog, 256},
    ```

If the `request_pool` value in Riak CS is changed, the `pb_backlog` value in Riak should be updated as well.

### Enabling SSL in Riak

In the Riak `app.config` file, first uncomment the following lines:

```erlang
%% https is a list of IP addresses and TCP ports that the Riak
%% HTTPS interface will bind.
%{https, [{ "127.0.0.1", 8098 }]},
%% Default cert and key locations for https can be overridden
%% with the ssl config variable, for example:
%{ssl, [
%       {certfile, "/etc/riak/cert.pem"},
%       {keyfile, "/etc/riak/key.pem"} ]},
%      ]},
```

For the `https` variable, replace the IP address with the address of the Riak node. Replace the port number if necessary.

For the `certfile` and `keyfile` variables, replace the text in quotes with the path and filename for your SSL encryption files.

###Other Riak Settings

The `app.config` file includes other settings, such as turning on the creation of log files and specifying where to store them. These settings have default values that work in most cases.

{{#1.2.0+}}
### Performance & Capacity settings

It is strongly recommended that the following values be set in the
Riak `vm.args` configuration file, which is located in the `/etc/riak` or `/opt/riak/etc` folder:

```erlang
## This setting is not present in default Riak installations, so
## it should be added.  In some cases, a value of 128000 may be
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
