---
title: "Riak CS Control"
description: ""
menu:
  riak_cs-2.0.0:
    name: "Riak CS Control"
    identifier: "advanced_riak_cs_control"
    weight: 102
    parent: "run_advanced"
project: "riak_cs"
project_version: "2.0.0"
aliases:
  - /riakcs/2.0.0/references/appendices/RiakCS-Control/
  - /riak/cs/2.0.0/references/appendices/RiakCS-Control/
---

Riak CS Control is a standalone user management application for Riak CS.
It provides a user interface for filtering, disabling, creating and
managing users in a Riak CS Cluster.

## Installing Riak CS Control

Riak CS Control [is maintained as a separate application](https://github.com/basho/riak_cs_control) and can be installed via [source or package]({{<baseurl>}}riak/cs/2.0.0/downloads).

## Setting Up Riak CS Control

In the `/etc/riak-cs-control/app.config` file, configure the application
with the information needed to connect to the Riak CS cluster you wish
to administer.

### Configuring Riak CS Control

``` erlang
{riak_cs_control, [
  %% What port to run the application on.
  {port, 8000 },

  %% Instance of Riak CS you wish to talk to.
  {cs_hostname, "s3.amazonaws.com" },
  {cs_port, 80 },
  {cs_protocol, "http" },

  %% Proxy information; necessary if you are using s3.amazonaws.com as
  %% your hostname.
  {cs_proxy_host, "localhost" },
  {cs_proxy_port, 8080 },

  %% Credentials you want the application to run as.
  {cs_admin_key, "admin-key" },
  {cs_admin_secret, "admin-secret" },

  %% Specify the bucket name for administration options.
  {cs_administration_bucket, "riak-cs" }
]},
```

### Running Riak CS Control

Start Riak CS Control as you would Riak or Riak CS with:

```bash
riak-cs-control start
```

## The Users Page

When you first navigate to the Riak CS Control UI, you will land on the
Users page:

![Users Page]({{<baseurl>}}images/cs_control_users.png)

On this page you can quickly see all current Riak CS users along with
their status, e-mail address, and credentials. From here you can filter,
disable, create, and manage users in a Riak CS Cluster.
