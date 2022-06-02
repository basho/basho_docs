---
title: "Configuring Stanchion"
description: ""
menu:
  riak_cs-2.0.0:
    name: "Configuring Stanchion"
    identifier: "config_stanchion"
    weight: 102
    parent: "config"
project: "riak_cs"
project_version: "2.0.0"
aliases:
  - /riakcs/2.0.0/cookbooks/configuration/Configuring-Stanchion/
  - /riak/cs/2.0.0/cookbooks/configuration/Configuring-Stanchion/
---

In your cluster, you must include one -- and only one -- Stanchion node. All the
Riak CS nodes in that cluster must then be configured to communicate with that
Stanchion node so that the cluster is able to track and negotiate
causally-sensitive operations.

All of the settings used by the Stanchion node are stored in the
`stanchion.conf` file, which is located in the `/etc/stanchion` folder on most
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
             {host, {"127.0.0.1", 8085}},
             %% Other configs
            ]}
```

{{% note title="Note on matching IP addresses" %}}
The IP address you enter here must match the IP address specified for the
`stanchion_host` variable in the Riak `riak.conf` file and the Riak CS
`riak-cs.conf` file.
{{% /note %}}

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

## Specifying the Admin User

The admin user is created during the [configuration of Riak CS]({{<baseurl>}}riak/cs/2.0.0/cookbooks/configuration/riak-cs/#specifying-the-admin-user). 
The same user credentials must be added to each Stanchion used in the cluster. 
This is set in the `stanchion.conf` file, which is located in the 
`/etc/stanchion` directory. Enter the same `admin.key` and `admin.secret` as 
used in `riak-cs.conf`:

```stanchionconf
admin.key = OUCXMB6I3HOZ6D0GWO2D
admin.secret = a58Mqd3qN-SqCoFIta58Mqd3qN7umE2hnunGag==
```

```appconfig
{stanchion, [
           %% Admin user credentials
           {admin_key, "OUCXMB6I3HOZ6D0GWO2D"},
           {admin_secret, "a58Mqd3qN-SqCoFIta58Mqd3qN7umE2hnunGag=="},
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
             {riak_host, {"127.0.0.1", 8087}},
             %% Other configs
            ]}
```
