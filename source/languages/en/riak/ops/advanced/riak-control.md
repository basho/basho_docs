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

{{#1.4.0-}}
See the below video for a quick introduction to Riak Control and its features.

<div style="display:none" class="iframe-video" id="http://player.vimeo.com/video/38345840"></div>
{{/1.4.0-}}

## Requirements

Though Riak Control [is maintained as a separate application](https://github.com/basho/riak_control), the necessary code for Control ships with versions of Riak 1.1 and above, and requires no additional installation steps.

It is strongly recommended that SSL be enabled for Riak Control. SSL is required unless you {{#2.0.0-}}explicitly set `{auth, none}` in `app.config`{{/2.0.0-}}{{#2.0.0+}}`riak_control.auth.mode = off` in `riak.conf`{{/2.0.0_}}. SSL can be enabled in the [[configuration files]].

## Setting up Riak Control

### Enabling SSL and HTTPS

{{#2.0.0-}}
In the `riak_core` section of `app.config` are two, commented-out sections:
`https` and `ssl`.

Uncomment the `https` line, and change the port to `8069`. You can choose any
unused port, but be sure and change it from the default, which is configured
to be the same as the standard `http` port for accessing Riak.

```erlang
{https, [{ "127.0.0.1", 8069 }]},
```

If you do not have your own SSL certificate, follow the instructions located
[here](http://www.akadia.com/services/ssh_test_certificate.html) to generate
one.

Next, uncomment the entire `ssl` section.  Point the `keyfile` and `certfile`
paths at your SSL certificate.

```erlang
{ssl, [
       {certfile, "./etc/cert.pem"},
       {keyfile, "./etc/key.pem"}
      ]},
```
{{/2.0.0-}}
{{#2.0.0+}}

In each Riak node's `riak.conf` configuration file, you will need to set the port for your HTTPS connection. Make sure that this port is different from that of your HTTP connection (if you choose to use a standard HTTP connection). An example:

```riakconf
listener.https.internal = 10018
```

If you do not have your own SSL certificate, follow the instructions located
[here](http://www.akadia.com/services/ssh_test_certificate.html) to generate
one.

Next, you need to specify which version(s) of SSL/TLS you wish to use. Each version can be set to `off` or `on` in `riak.conf`. For example:

```riakconf
tls_protocols.sslv3 = off
tls_protocols.tlsv1 = off
tls_protocols.tlsv1.1 = off
tls_protocols.tlsv1.2 = on
```

Finally, you need to specify the path of each of your certificates. For example:

```riakconf
ssl.certfile = ./etc/cert.pem
ssl.keyfile = ./etc/key.pem
```
{{/2.0.0+}}

#### SSL with Intermediate Authorities

{{#2.0.0-}}

If you are using a certificate that includes an intermediate authority, add
the `cacertfile` key and value:

```erlang
{ssl, [
       {certfile, "./etc/cert.pem"},
       {cacertfile, "./etc/cacert.pem"},
       {keyfile, "./etc/key.pem"}
      ]},
```
{{/2.0.0-}}
{{#2.0.0+}}

If you are using a certificate that includes an intermediate authority, add
the path to your `cacertfile` in `riak.conf`. Here's an example:

```riakconf
ssl.cacertfile = ./etc/cacertfile.pem
```
{{/2.0.0+}}

### Enabling Riak Control

{{#2.0.0-}}

Down near the bottom of the `app.config` file is the `riak_control` section.
By default, Riak Control is disabled, so you have to change the `enabled` flag
to `true`:

```erlang
{riak_control, [
         %% Set to false to disable the admin panel.
          {enabled,true},
```
{{/2.0.0-}}
{{#2.0.0+}}

To enable Riak Control in your `riak.conf` configuration file, set the value of `riak_control` to `on`:

```riakconf
riak_control = on
```
{{/2.0.0+}}

## Additional Configuration

### Authentication

Currently, Riak Control only supports two forms of authentication: {{#2.0.0-}}`none` and `userlist` (HTTP basic authentication){{/2.0.0-}}`off` (no authentication) and `userlist`{{#2.0.0+}}{{/2.0.0+}}. The default is `userlist`. If you wish to disable authentication, simple change the {{#2.0.0-}}`auth` setting to `none`{{/2.0.0-}}{{#2.0.0+}}`riak_control.auth.mode` to `off`{{/2.0.0+}}, or comment it out.

{{#2.0.0-}}

```erlang
%% Authentication style used for access to the admin
%% panel. Valid styles are 'userlist' and 'none'.
{auth, userlist}
```
{{/2.0.0-}}
{{#2.0.0+}}

```riakconf
riak_control.auth.mode = off
```
{{/2.0.0+}}

Using the `userlist` authentication method, you must next specify a list of
users and their passwords in plain text.

{{#2.0.0-}}

```erlang
%% If auth is set to 'userlist' then this is the
%% list of usernames and passwords for access to the
%% admin panel.
{userlist, [{"user", "pass"}
           ]},
```

The default username and password are `user` and `pass`. Feel free to modify these as needed.
{{/2.0.0-}}
{{#2.0.0+}}

{{/2.0.0+}}

**Note**: Any changes to {{#2.0.0-}}`app.config`{{/2.0.0-}}{{#2.0.0+}}`riak.conf`{{/2.0.0+}} require you to restart your node(s).

Once you have set up authentication, you should be able to login to Riak
Control.

### Enabling Modules

Riak Control was designed to allow the cluster administrator to enable and disable various modules. Currently there is only one module (`admin`) which is used to enable each resource. If you disable it, none of Riak Control's functionality will be available.

{{#2.0.0-}}

```erlang
%% The admin panel is broken up into multiple
%% components, each of which is enabled or disabled
%% by one of these settings.
{admin, true}
```
{{/2.0.0-}}
{{#2.0.0+}}

{{/2.0.0+}}

## User Interface

Navigate to <https://localhost:8069/admin> (or to your specified port if different).

If your browser warns you that it cannot authenticate the page, this is
because you are using the self-signed certificates.

If you have authentication enabled in the {{#2.0.0-}}`app.config`{{/2.0.0-}}{{#2.0.0+}}`riak.conf`{{/2.0.0+}}, you will next be asked to authenticate. Enter your username and password now.

### Snapshot View

When you first navigate to Riak Control, you will land on the Snapshot view:

{{#1.4.0-}}
![Snapshot View](/images/control_snapshot.png)
{{/1.4.0-}}
{{#1.4.0+}}
[![Snapshot View](/images/control_current_snapshot.png)](/images/control_current_snapshot.png)
{{/1.4.0+}}

Here, the health of your cluster is made immediately obvious. In the event that something isn't quite right (or has the potential to cause problems in the near future), the green check mark will turn into a red `X`. The red `X` is accompanied by a list of reasons for concern. Each item in the list links to a page where you can get more information about the issue.

### Cluster Management View

On the top right of the admin panel are navigation tabs. If you click the
**Cluster** tab, you will be taken to the cluster management page.

{{#1.4.0-}}
On this page you can see all of the nodes in your cluster, along with their
status, percentage of the ring ownership, and memory consumption. You can also
make changes to the cluster, such as adding, removing, and marking nodes as
down.

![Cluster View](/images/control_cluster.png)
{{/1.4.0-}}
{{#1.4.0+}}
On this page you can see all of the nodes in your cluster, along with their
status, percentage of the ring ownership, and memory consumption. You can also
stage and commit changes to the cluster, such as adding, removing, and marking
nodes as down.

Staged changes to the cluster:

[![Cluster Management Staged](/images/control_cluster_management_staged.png)](/images/control_cluster_management_staged.png)

Changes committed; transfers active:

[![Cluster Management Transfers](/images/control_cluster_management_transfers.png)](/images/control_cluster_management_transfers.png)

Cluster stabilizes after changes:

[![Cluster Management Stable](/images/control_cluster_management_stable.png)](/images/control_cluster_management_stable.png)

### Node Management View

The node management view allows you to operate against the individual nodes in
the cluster.

[![Node Management](/images/control_node_management.png)](/images/control_node_management.png)
{{/1.4.0+}}

### Ring View

One level deeper than the cluster view is the ring view. This is where you can
see the health of each vnode.

{{#1.4.0-}}
![Ring View](/images/control_ring.png)
{{/1.4.0-}}
{{#1.4.0+}}
[![Ring View](/images/control_current_ring.png)](/images/control_current_ring.png)
{{/1.4.0+}}

Most of the time, your ring will be too large to effectively manage from the
ring view. That said, with filters you can easily identify partition ownership,
unreachable primaries, and in-progress handoffs.