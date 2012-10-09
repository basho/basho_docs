---
title: Riak Control
project: riak
version: 1.0.0+
document: appendix
toc: true
audience: intermediate
keywords: [control]
---

Riak Control is Basho's OSS, REST-driven, user-interface for managing Riak clusters. It is designed to give you quick insight into the health of your cluster and allow for easy management of nodes.

See the below video for a quick introduction to Riak Control and its features.

<div style="display:none" class="iframe-video" id="http://player.vimeo.com/video/38345840"></div>

## Requirements

Though Riak Control [is maintained as a separate application](https://github.com/basho/riak_control), the necessary code for Control ships with versions of Riak 1.1 and greater and requires no additional downloads outside of your preferred package or source build. 

It is strongly recommended that SSL be enabled for riak-control, and SSL is requied unless you set `{auth, none}`.  SSL can be enabled in the [[Configuration Files]].

## Setting Up Riak Control

Your `app.config` file should already have all the predeclared, necessary sections to get Riak Control up and running, we'll just turn them on.

### Enabling SSL and HTTPS

In the `riak_core` section of `app.config` are two, commented-out sections: `https` and `ssl`.

Uncomment the `https` line, and change the port to `8069`. You can actually choose any unused port, but be sure and change it from the default, which is configured to be the same as the standard `http` port use for accessing Riak.

```erlang
{https, [{ "127.0.0.1", 8069 }]},
```

If you do not have your own SSL certificate, follow the instructions
located [here](http://www.akadia.com/services/ssh_test_certificate.html)
to generate one.

Next, uncomment the entire `ssl` section.  Point the `keyfile` and
`certfile` paths at your SSL certificate.

```erlang
{ssl, [
       {certfile, "./etc/cert.pem"},
       {keyfile, "./etc/key.pem"}
      ]},
```

#### SSL with Intermediate Authorities

If you are using a certificate that includes an intermediate authority, add the `cacertfile` key and value:

```erlang
{ssl, [
       {certfile, "./etc/cert.pem"},
       {cacertfile, "./etc/cacert.pem"},
       {keyfile, "./etc/key.pem"}
      ]},
```

### Enabling Riak Control

Down near the bottom of the `app.config` file is the `riak_control` section. By default, Riak Control is disabled, and you toggle this by changing the `enabled` flag to `true`:

```erlang
{riak_control, [
				 %% Set to false to disable the admin panel.
			    {enabled,true},
```

## Additional Configuration

When you first browse to the Riak Control UI, you should be prompted for a username and password. Authentication is handled via the `app.config` file and we'll go over these settings now.

### Authentication

Currently, Riak Control only supports two (very basic) forms of authentication: `none` and `userlist` (plain text username & password). There are later plans to support additional authentication methods (e.g. LDAP). The default is to use `userlist`. If you wish to disable authentication, simple change the `auth` setting to `none` or comment it out.

```erlang
%% Authentication style used for access to the admin
%% panel. Valid styles are 'userlist' and 'none'.
{auth, userlist}
```

Using the `userlist` authentication method, you must next specify a list of users and their passwords (in plain text).

```erlang
%% If auth is set to 'userlist' then this is the
%% list of usernames and passwords for access to the
%% admin panel.
{userlist, [{"user", "pass"}
           ]},
```

The default user and password are "user" and "pass". Feel free to modify these if you like (*remember, any changes to app.config require you to restart your node*).

Once you have setup the authentication, you should be able to log into the Riak Control admin panel.

### Enabling Modules

Riak Control is designed to be modular, and to allow the cluster administrator to enable and disable various modules. Currently there is only a single module (`admin`) which is used to enable each of REST-API routes. If you disable it, none of Riak Control will be available.

```erlang
%% The admin panel is broken up into multiple
%% components, each of which is enabled or disabled
%% by one of these settings.
{admin, true}
```

## Two-Minute Tour

Once you have configured your node to run Riak Control, you're ready to dive in!

### Start (or Restart) Your Node

Start your node and direct your browser to <https://localhost:8069/admin>. 

If your browser warns you that it cannot authenticate the page, this is because you are using the self-signed certificates; you can safely ignore this warning and continue past it. 

If you have authentication enabled in the `app.config` you will next be asked to authenticate. Enter your username and password now.

### The Snapshot Page

When you first navigate to the Riak Control UI, you will land on the Snapshot page:

![The Snapshot Page](/images/control_snapshot.png)

Immediately obvious is whether or not your cluster is "healthy". If this is your first time firing up Riak, then you should be presented with a comforting green check mark indicating that everything is okay. In the event that something isn't quite right (or has the potential to cause problems in the near future) the check mark will turn into a red X and there will be a list of reasons for the concern, each linking you to a page where you can get more information or even correct the problem.

### The Cluster View

On the left side of the admin panel are navigation tabs. If you click the "Cluster" tab, you will be taken to the cluster management page.

![The Cluster View](/images/control_cluster.png)

On this page you can quickly see all the nodes in your cluster, their status, what percentage of the ring partitions they own, how much memory they are using, and more. You can also add nodes to the cluster, remove nodes, and take nodes offline.

### The Ring View

One level deeper than the cluster view is the ring page. This is where you can see the health of each partition.

![The Ring View](/images/control_ring.png)

Most of the time, your ring will be too large to really manage from the ring view. But, with the filters you can immediately find which partitions are owned by which nodes, partitions whose primary nodes are unreachable, current handoffs, and more.
