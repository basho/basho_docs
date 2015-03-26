---
title: Configuring Stanchion
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, configuration]
---

In your cluster, you must include one -- and only one -- Stanchion node. All the
Riak CS nodes in that cluster must then be configured to communicate with that
Stanchion node so that the cluster is able to track and negotiate
causally-sensitive operations.

All of the settings used by the Stanchion node are stored in the
`stanchion.conf` file, which is located in the `/etc/riak-cs` folder on most
operating systems.

If you're upgrading from a version of Riak CS prior to 2.0.0 -- when the
`stanchion.conf` and `riak-cs.conf` files was introduced -- you can still use
the old-style `app.config` configuration files. Examples for both configuration
types will be provided.

```stanchionconf
configuration.name = value
```

```appconfig
{stanchion, [
             %% Configs here
            ]}
```

## Specifying the Stanchion IP Address and Port

If you have a single node, you don't have to change the Stanchion settings
because Stanchion simply listens to the requests from the local host. If your
Riak CS cluster has multiple nodes, you must set the IP address and port that
Stanchion listens on for requests from other nodes.

You can set the IP using the `listener` parameter. Replace `127.0.0.1` with the
IP address of the Stanchion node, and `8080` with the port of the Stanchion
node.

```stanchionconf
listener = 127.0.0.1:8080
```

```appconfig
{stanchion, [
             {stanchion_ip, "127.0.0.1"},
             {stanchion_port, 8080}
             %% Other configs
            ]}
```

<div class="note"><div class="title">Note on matching IP addresses</div>
The IP address you enter here must match the IP address specified for the
<code>stanchion_host</code> variable in the Riak <code>riak.conf</code> file and
the Riak CS <code>riak-cs.conf</code> file.
</div>

If you want to use SSL, make sure the `ssl.certfile` and `ssl.keyfile` settings
are not commented out, and have been set correctly.

```stanchionconf
ssl.certfile = "./etc/cert.pem"
ssl.keyfile = "./etc/key.pem"
```

```appconfig
{stanchion, [
             {ssl, [
                    {certfile, "./etc/cert.pem"},
                    {keyfile, "./etc/key.pem"}
                   ]},
             %% Other configs
            ]}
```

## Specifying Riak Information

If you are running a single node for experimentation, or if a Riak node is
running locally and configured to listen for protocol buffer traffic on
`0.0.0.0`, the default Riak configuration for Stanchion should be fine.

Otherwise, update the IP address and port for the Riak host in the Stanchion
configuration file.

```stanchionconf
riak_host = 127.0.0.1:8087
```

```appconfig
{stanchion, [
             {riak_ip, "127.0.0.1"},
             {riak_pb_port, 8087}
             %% Other configs
            ]}
```
