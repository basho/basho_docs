---
title: Riak Control
project: riak
version: 1.0.0+
document: appendix
toc: true
audience: intermediate
keywords: [control]
moved: {
    '1.4.0-': '/references/appendices/Riak-Control'
}
---

Riak Control is a web-based administrative console for inspecting and
manipulating Riak clusters.

## Requirements

Though Riak Control [is maintained as a separate
application](https://github.com/basho/riak_control), the necessary code
for it ships with versions of Riak 1.1 and above and requires no
additional installation steps.

<div class="note">
<div class="title">Note on SSL</div>
We strongly recommend that you enable SSL for Riak Control. It is
enabled by default, but if you wish to disable it you must do so
explicitly. More information can be found in the document below.
</div>

## Enabling and Disabling Riak Control

Riak Control is disabled by default, meaning that you should see the
following in your [[configuration files]]:

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

## Enabling SSL and HTTPS

In order to use SSL in conjunction with Riak Control, SSL must be
enabled in each Riak node. For more information, see our [[security
documentation|Authentication and
Authorization#Enabling-SSL]]. Once SSL is enabled, you can proceed to
setting up [[authentication|Riak Control#Authentication]] for Riak
Control.

## Authentication

Riak Control provides you the option of requiring authentication (via
HTTP basic auth) for users of the web interface. It is disabled by
default. To enable authentication:

```riakconf
riak_control.auth.mode = on
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
riak_control.auth.user.suzy.password = h4x0r123
riak_control.auth.user.riakrocks.password = cap_theorem_4_life
```

```appconfig
{riak_control, [
                %% Other configs
                {userlist, [
                            {"bob", "bob_is_the_coolest"},
                            {"suzy", "h4x0r123"},
                            {"riakrocks", "cap_theorem_4_life"}
                            ]}
                %% Other configs
]}
```

## User Interface

To begin using Riak Control, navigate to <https://localhost:8069/admin>.
If you're using a different HTTPS port, substitute the proper port in
the URL.

If your browser warns you that it cannot authenticate the page, this may
be because you are using self-signed certificates. If you have
authentication enabled in your configuration, you will next be asked to
authenticate. Enter an appropriate username and password now.

<div class="note">
<div class="title">Note on browser TLS</div>
Your browser needs to be support TLS v1.2 to use Riak Control over
HTTPS. A list of browsers that support TLS v1.2 can be found
[here](https://en.wikipedia.org/wiki/Transport_Layer_Security#Web_browsers).
TLS v1.2 may be disabled by default on your browser, for example if you
are using Firefox versions earlier than 27, Safari versions earler than
7, Chrome versions earlier than 30, or Internet Explorer versions
earlier than 11.  To enable it, follow browser-specific instructions.
</div>

### Snapshot View

When you first navigate to Riak Control, you will land on the Snapshot
view:

[ ![Snapshot View](/images/control_current_snapshot.png) ] (/images/control_current_snapshot.png)

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

[ ![Cluster Management Staged](/images/control_cluster_management_staged.png) ] (/images/control_cluster_management_staged.png)

Changes committed; transfers active:

[ ![Cluster Management Transfers](/images/control_cluster_management_transfers.png) ] (/images/control_cluster_management_transfers.png)

Cluster stabilizes after changes:

[ ![Cluster Management Stable](/images/control_cluster_management_stable.png) ] (/images/control_cluster_management_stable.png)

### Node Management View

The node management view allows you to operate against the individual
nodes in the cluster.

[ ![Node Management](/images/control_node_management.png) ] (/images/control_node_management.png)

### Ring View

One level deeper than the cluster view is the ring view. This is where you can
see the health of each [[vnode|Vnodes]].

[ ![Ring View](/images/control_current_ring.png) ] (/images/control_current_ring.png)

Most of the time, your ring will be too large to effectively manage from
the ring view. That said, with filters you can easily identify partition
ownership, unreachable primaries, and in-progress handoffs.
