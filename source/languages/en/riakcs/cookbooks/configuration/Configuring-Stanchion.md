---
title: Configuring Stanchion
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, configuration]
---

On the Stanchion node, you set the IP address and port for listening and the Riak IP address and port. On each Riak CS node, you have to set three Stanchion configuration properties.

## Specifying the Stanchion IP Address and Port
If you have a single node, you don't have to change the settings for the address to listen on, because Stanchion simply listens to the requests from the local host. If your Riak CS system has multiple nodes, you set the IP address and port that Stanchion listens on for requests from other nodes.

These settings reside in the Stanchion `app.config` file, which is located in the `/etc/stanchion` folder. The settings appear in the `stanchion` config section of the file.

**stanchion_ip**: Replace 127.0.0.1 with the IP address of the Stanchion node.

<div class="note"><div class="title">Note</div>The IP address you enter here must match the IP address specified for the stanchion_ip variable in the Riak app.config file and the Riak CS app.config file. </div>

If you want to use a different port for Stanchion to accept connections on, you must change the following port setting:

**stanchion_port**: Replace 8085 with the port number you want to use.

The `stanchion_ssl` variable is set to false by default. If you want to use SSL, change this variable to true.

## Specifying Riak Information

If you have a single node, you don't have to change the setting for the Riak address, because Stanchion and Riak are both on the local host. If your Riak CS system has multiple nodes, you set the IP address and port for Riak in the Stanchion `app.config` file, which is located in the `/etc/riak-cs` folder. The settings appear in the Stanchion config section of the file.

**riak_ip**: Replace 127.0.0.1 with the IP address of the Riak node.

<div class="note"><div class="title">Note</div>The IP address you enter here must match the IP address specified for the Protocol Buffers interface in the Riak app.config file. If a server has more than one network interface card (NIC), you can use the IP address for a specific NIC. If you want Riak CS to listen on all of them, set riak_ip to "0.0.0.0". </div>

If you configured Riak to use a different port for protocol buffers, change the following port setting and then restart Riak:

**riak_pb_port**: Replace 8087 with the port number set in the Riak `app.config` file.
