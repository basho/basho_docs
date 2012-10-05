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

Some of the Riak buckets used internally by Riak CS use secondary indexes, which currently requires the eLevelDB backend. Other parts of the Riak CS system can benefit more from the use of the Bitcask backend. The use of the custom multi backend enables Riak CS to take advantage of the strengths of both of these backends to achieve the best blend of performance and features. The next section covers how to properly setup Riak to use this multi backend.

Additionally, the Riak CS storage calculation system uses Riak's MapReduce to sum the files in a bucket. This means you must tell all of your Riak nodes where to find Riak CS's compiled files before calculating storage.

A few other settings must be modified to configure a Riak node as part of a Riak CS system: the node IP address and the IP address and port to use for communicating through protocol buffers. Other settings can be modified if necessary. The following sections describe how to configure a Riak node to work as part of a Riak CS system.

## Setting up the Proper Riak Backend
First, edit Riak's `app.config` file and find and delete the line containing the `storage_backend` property in the `riak_kv `section. The `app.config `file can be found in the `/etc/riak` directory. The default setting is for the Bitcask backend and would look like this:

```
{storage_backend, riak_kv_bitcask_backend},
```

Next, expose the necessary Riak CS modules to Riak and instruct Riak to use the custom multi backend. Continue editing Riak's `app.config` file, and add this to the `riak_kv` section:

```
{add_paths, ["/usr/lib/riak-cs/lib/riak_moss-X.Y.Z/ebin"]},
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

where **X.Y.Z** is the version of Riak CS you have installed.

Next, add this to the **riak_core** section of `app.config`:

    {default_bucket_props, [{allow_mult, true}]},

Save and exit the editing session on the `app.config` file. To test that you have configured a Riak node correctly, start Riak and connect to its console (using `riak attach`), then run:

```
(riak@127.0.0.1)1> code:which(riak_cs_kv_multi_backend).
"/usr/lib64/riak-cs/lib/riak_moss-X.Y.Z/ebin/riak_cs_kv_multi_backend.beam"
```

If the path that you added to Riak's `app.config` is returned, your node is configured correct. If instead, the atom **`non_existing`** is returned, Riak was unable to find the Riak CS code.

<div class="note"><div class="title">Note</div>It is important to use `CTRL+D` to detach the console and leave Riak running after doing a `riak attach`. `CTRL+C` will cause the Riak node to exit and in many cases this is not the desired outcome of detaching from the console.</div>

## Specifying the Riak IP Address
By setting the Riak IP address you ensure that your Riak nodes have unique IP addresses, whether you work with a single node or add additional nodes to the system. The Riak IP address setting resides in the Riak vm.args configuration file, which is located in the `/etc/Riak` folder.

Initially, the line that specifies the riak node IP address is set to the local host, as follows:

```
-name riak@127.0.0.1
```

Replace 127.0.0.1 with the IP address for the Riak node.

## Setting Up Riak to Use Protocol Buffers
The Riak protocol buffer settings reside in the Riak `app.config` file, which is located in the `/etc/riak` folder. The settings appear in the` riak_kv` config section of the file.

**pb_ip**: Replace 127.0.0.1 with the IP address of the Riak node.

If you need to use a different port:

**pb_port**: Replace 8087 with the port number you want to use.

<div class="note"><div class="title">Note</div>A different port number might be required if the port number conflicts with ports used by another application or you use a load balancer or proxy server.</div>

The `pb_ip` and `pb_port` values in the Riak `app.config` file must match the values for `riak_ip` and `riak_pb_port` in the Riak CS and Stanchion `app.config` files.

### Enabling SSL in Riak
In the Riak `app.config` file, first uncomment the following lines:

```
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

For the https variable, replace the IP address with the address of the Riak node. Replace the port number if necessary.

For the **certfile** and **keyfile** variables, replace the text in quotes with the path and filename for your SSL encryption files.

###Other Riak Settings

The `app.config` file includes other settings, such as turning on the creation of log files and specifying where to store them. These settings have default values that work in most cases.
