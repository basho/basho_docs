---
title: "Riak Control"
description: ""
project: "riak_kv"
project_version: 2.9.1
menu:
  riak_kv-2.9.1:
    name: "Riak Control"
    identifier: "cluster_admin_riak_control"
    weight: 103
    parent: "managing_cluster_admin"
toc: true
aliases:
  - /riak/2.9.1/ops/advanced/riak-control
  - /riak/kv/2.9.1/ops/advanced/riak-control
---

[config reference]: {{<baseurl>}}riak/kv/2.9.1/configuring/reference

Riak Control is a web-based administrative console for inspecting and
manipulating Riak clusters.

## Requirements

Though Riak Control [is maintained as a separate application](https://github.com/basho/riak_control), the necessary code for it ships with versions of Riak 1.1 and above and requires no additional installation steps.

Before getting started, you should know the address and port of the HTTP (or
HTTPS) listeners for the cluster member(s) running Riak Control.  You can obtain
this information from the configuration files as indicated here:

```riakconf
listener.http.<name> = 127.0.0.1:8098

or

listener.https.<name> = 127.0.0.1:8096

## *** The default listeners in the riak.conf file are
##     named `internal`, so you would consult the value of
##     `listener.http.internal` in your configuration.

```

```appconfig
 {riak_api,
     [
        %% Other configs
        ... if HTTP is configured ...
        {http,[{"127.0.0.1",8098}]},
        ... if HTTPS is configured ...
        {https,[{"127.0.0.1",8069}]},
         %% Other configs
     ]},

%% *** This is a truncated configuration to illustrate the
%%     pertinent items -- the `http` and `https` tuples within
%%     the `riak_api` tuple's value list.
```

{{% note title="Note on SSL" %}}
We strongly recommend that you enable SSL for Riak Control. It is disabled by
default, and if you wish to enable it you must do so explicitly. More
information can be found in the document below.
{{% /note %}}

## Enabling and Disabling Riak Control

Riak Control is disabled by default, meaning that you should see the
following in your [configuration files][config reference]:

```riakconf
riak_control = off
```

```appconfig
{riak_control, [
                %% Other configs
                {enabled, false},
                %% Other configs
               ]}
```

Enabling Riak Control is simple:

```riakconf
riak_control = on
```

```appconfig
{riak_control, [
                %% Other configs
                {enabled, true},
                %% Other configs
               ]}
```

Make sure to restart the node once you have enabled Riak Control for the
change to take effect.

After restarting the node, you should be able to access it by going
to `http://ip_address_of_listener:port/admin`. In the case of a development
cluster using the default configuration, you would access Riak Control at
<http://127.0.0.1:8098/admin></a>

If you enabled authentication for Riak Control while performing the above
configuration, you will be unable to access Riak Control until you have enabled
and configured SSL and HTTPS.  

## Enabling SSL and HTTPS

In order to use SSL in conjunction with Riak Control, SSL must be
enabled on each Riak node. For more information, see our [security documentation]({{<baseurl>}}riak/kv/2.9.1/using/security/basics#enabling-ssl). Once SSL is enabled, you can proceed to setting up [authentication](#authentication) for Riak Control.

Please note that Riak Control will not work if you have enabled
authentication but SSL is not set up properly.

## Authentication

Riak Control provides you the option of requiring authentication (via
HTTP basic auth) for users of the web interface. It is disabled by
default. To enable authentication:

```riakconf
riak_control.auth.mode = userlist
```

```appconfig
{riak_control, [
                %% Other configs
                {auth, userlist}, %% The only other available option is "none"
                %% Other configs
               ]}
```

When authentication is enabled, you can specify as many
username/password pairs as you wish. The default pair is the username

`user` and the password `pass`. We strongly recommend selecting
different credentials. The example below would set up three user-defined
pairs:

```riakconf
riak_control.auth.user.bob.password = bob_is_the_coolest
riak_control.auth.user.polly.password = h4x0r123
riak_control.auth.user.riakrocks.password = cap_theorem_4_life
```

```appconfig
{riak_control, [
                %% Other configs
                {userlist, [
                            {"bob", "bob_is_the_coolest"},
                            {"polly", "h4x0r123"},
                            {"riakrocks", "cap_theorem_4_life"}
                            ]}
                %% Other configs
]}
```

## User Interface

To begin using Riak Control, navigate to https://ip_address_of_https_listener:https_port/admin
For a default configuration, this will be <https://localhost:8069/admin>.

If your browser warns you that it cannot authenticate the page, this may
be because you are using self-signed certificates. If you have
authentication enabled in your configuration, you will next be asked to
authenticate. Enter an appropriate username and password now.

{{% note title="Note on browser TLS" %}}
Your browser needs to be support TLS v1.2 to use Riak Control over HTTPS. A
list of browsers that support TLS v1.2 can be found
[here](https://en.wikipedia.org/wiki/Transport_Layer_Security#Web_browsers).
TLS v1.2 may be disabled by default on your browser, for example if you are
using Firefox versions earlier than 27, Safari versions earlier than 7, Chrome
versions earlier than 30, or Internet Explorer versions earlier than 11.  To
enable it, follow browser-specific instructions.
{{% /note %}}

### Snapshot View

When you first navigate to Riak Control, you will land on the Snapshot
view:

[ ![Snapshot View]({{<baseurl>}}images/control_current_snapshot.png) ] ({{<baseurl>}}images/control_current_snapshot.png)

In this interface, the health of your cluster is made immediately
obvious. In the event that something isn't quite right (or has the
potential to cause problems in the near future), the green check mark
will turn into a red `X`. The red `X` is accompanied by a list of
reasons for concern. Each item in the list links to a page where you can
get more information about the issue.

### Cluster Management View

On the top right side of the admin panel are navigation tabs. If you
click the **Cluster** tab, you will be taken to the cluster management
page.

On this page, you can see all of the nodes in your cluster, along with
their status, the percentage of the ring owned by that node, and memory
consumption. You can also stage and commit changes to the cluster, such
as adding, removing, and marking nodes as down.

Staged changes to the cluster:

[ ![Cluster Management Staged]({{<baseurl>}}images/control_cluster_management_staged.png) ] ({{<baseurl>}}images/control_cluster_management_staged.png)

Changes committed; transfers active:

[ ![Cluster Management Transfers]({{<baseurl>}}images/control_cluster_management_transfers.png) ] ({{<baseurl>}}images/control_cluster_management_transfers.png)

Cluster stabilizes after changes:

[ ![Cluster Management Stable]({{<baseurl>}}images/control_cluster_management_stable.png) ] ({{<baseurl>}}images/control_cluster_management_stable.png)

### Node Management View

The node management view allows you to operate against the individual
nodes in the cluster.

[ ![Node Management]({{<baseurl>}}images/control_node_management.png) ] ({{<baseurl>}}images/control_node_management.png)

### Ring View

One level deeper than the cluster view is the ring view. This is where you can
see the health of each [vnode]({{<baseurl>}}riak/kv/2.9.1/learn/glossary/#vnode).

[ ![Ring View]({{<baseurl>}}images/control_current_ring.png) ] ({{<baseurl>}}images/control_current_ring.png)

Most of the time, your ring will be too large to effectively manage from
the ring view. That said, with filters you can easily identify partition
ownership, unreachable primaries, and in-progress handoffs.
