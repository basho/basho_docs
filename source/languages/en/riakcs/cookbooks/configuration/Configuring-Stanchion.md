---
title: Configuring Stanchion
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, configuration]
---

On the Stanchion node, you set the IP address and port for listening and
the Riak IP address and port. On each Riak CS node, you have to set
three Stanchion configuration properties.

## Specifying the Stanchion IP Address and Port

If you have a single node, you don't have to change the settings for the
address to listen on because Stanchion simply listens to the requests
from the local host. If your Riak CS system has multiple nodes, you set
the IP address and port that Stanchion listens on for requests from
other nodes.

These settings reside in the Stanchion `app.config` file, which is
located in the `/etc` folder on most operating systems. The settings
appear in the `stanchion` config section of the file, which looks like
this:

```appconfig
{stanchion, [

    %% Configs here

]}
```


You can set the IP using the `stanchion_ip` parameter. Replace
`127.0.0.1` with the IP address of the Stanchion node.

<div class="note">
<div class="title">Note on matching IP addresses</div>
The IP address you enter here must match the IP address specified for
the <code>stanchion_ip</code> variable in the Riak
<code>app.config</code> file and the Riak CS <code>app.config</code>
file.
</div>

If you want to use a different port for Stanchion to accept connections
on, you must change the `stanchion_port` setting. Replace `8085` with
the port number you want to use.

The `stanchion_ssl` variable is set to `false` by default. If you want
to use SSL, change this variable to `true`.

## Specifying Riak Information

If you are running a single node for experimentation, or if a Riak node
is running locally and configured to listen for protocol buffer traffic
on `0.0.0.0`, the default Riak configuration for Stanchion should be
fine.

Otherwise, update the IP address and port for Riak in the Stanchion
`app.config` file. The settings appear in the `stanchion` section:

* `riak_ip` --- Replace `127.0.0.1` with the IP address of the Riak node
* `riak_pb_port` --- Replace `8087` with the port number set in the Riak
  `app.config` file
