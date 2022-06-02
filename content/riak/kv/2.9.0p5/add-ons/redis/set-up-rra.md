---
title: "Setting Up Riak Redis Add-on"
description: ""
project: "riak_kv"
project_version: "2.9.0p5"
menu:
  riak_kv-2.9.0p5:
    name: "Set Up Redis Add-on"
    identifier: "add-ons_redis_setup"
    weight: 201
    parent: "add-ons_redis"
toc: true
commercial_offering: true
aliases:
  - /riak/2.9.0p5/add-ons/redis/set-up-rra/
  - /riak/2.9.0/add-ons/redis/set-up-rra/
  - /riak/kv/2.9.0/add-ons/redis/set-up-rra/
  - /riak/kv/2.9.0p1/add-ons/redis/set-up-rra/
  - /riak/kv/2.9.0p2/add-ons/redis/set-up-rra/
  - /riak/kv/2.9.0p3/add-ons/redis/set-up-rra/
  - /riak/kv/2.9.0p4/add-ons/redis/set-up-rra/
---


[addon redis develop]: ../developing-rra/
[addon redis use]: ../using-rra
[ee]: https://www.tiot.jp/en/about-us/contact-us/
[install index]: {{<baseurl>}}riak/kv/2.9.0p5/setup/installing
[perf open files]: {{<baseurl>}}riak/kv/2.9.0p5/using/performance/open-files-limit/#changing-the-limit
[lab ansible]: https://github.com/paegun/ansible-cache-proxy

This page will walk you through the process of installing Riak Redis Add-on (RRA) and configuring it to run in your environment. Check the [prerequisites](#prerequisites) before you get started to make sure you have everything you need in order to successfully install and use RRA.

## Prerequisites

Before you begin installing Riak Redis Add-on (RRA), you will need to ensure that you have root or sudo access on the nodes where you plan to install RRA. You will also need to have Riak KV already [installed][install index].

While this page assumes that Redis is not already installed, existing installations of Redis are supported. If you have an existing Redis installation, look for the *skip ahead* instructions as you go.

This page assumes that Redis is (or will be) installed on separate hosts from Riak KV. You will need the list of Riak KV and Redis host:port combinations. RRA communicates with Riak KV via the protobuf port, and the host:port values are used
to configure the cache proxy.

## In the Lab

An ansible setup for the Riak Redis Add-on (RRA) was developed to provide a
runnable example of an installation, see [ansible cache proxy][lab ansible].
The remainder of this setup guide lists the commands required to install and
configure RRA manually.

## Installing

1. On all Redis and Riak Redis Add-on hosts, change the [open-files limit][perf open files].
2. On all Redis hosts, install Redis. **Skip ahead* if you already have Redis installed.
3. Install Riak Redis Add-on.

### Change the open-files limit

As with Riak KV, both the total open-files limit and the per-user open-files limit
must be high enough to allow Redis and Riak Redis Add-on (RRA) to function.

For a complete guide on changing limit in Riak KV, see
[Changing the limit][perf open files].

#### Linux

On most Linux distributions, the total limit for open files is controlled by `sysctl`.

```bash
sudo sysctl fs.file-max fs.file-max=65536
sudo sysctl -p
```

To change the per-user file limit, you need to edit `/etc/security/limits.conf`.

#### CentOS

On CentOS systems, set a proper limit for the user you're usually logging in with
to do any kind of work on the machine, including managing Riak KV, Redis, or RRA services. On CentOS, `sudo` properly inherits the values from the
executing user.

#### Ubuntu

On Ubuntu systems, the following settings are recommended:

```config
»USERNAME« hard nofile 65536
»USERNAME« soft nofile 65536
root hard nofile 65536
root soft nofile 65536
```

>**Note:** You may need to log out of your shell and then log back in for these changes to take effect.


### Install Redis

>**Note:** If you already have Redis installed, *skip ahead* to "Install Riak Redis Add-on".

#### Install on Ubuntu

If you are on Ubuntu, run the following to install Redis:

```bash
# add the dotdeb repositories to your APT sources.
sudo bash -c "cat >> /etc/apt/sources.list.d/dotdeb.org.list" <<EOF
deb http://packages.dotdeb.org squeeze all
deb-src http://packages.dotdeb.org squeeze all
EOF

# authenticate the dotdeb repositories using their public key.
wget -q -O - http://www.dotdeb.org/dotdeb.gpg | sudo apt-key add -

# update APT cache
sudo apt-get update

# install Redis
sudo apt-get install redis-server
```

Then, you'll need to configure Redis, which is shown below.

#### Install on CentOS

If you are on CentOS, run the following to install Redis:

```bash
# install the EPEL repository
VERSION_ID=$(case $(cat /etc/centos-release) in *6*) echo "6";; *7*) echo "7";; esac)
wget -r --no-parent -A 'epel-release-*.rpm' http://dl.fedoraproject.org/pub/epel/$VERSION_ID/x86_64/e/
rpm -Uvh dl.fedoraproject.org/pub/epel/$VERSION_ID/x86_64/e/epel-release-*.rpm

# install Redis
yum install redis
```

Then, you'll need to configure Redis, which is shown below.

#### Configure Redis

To configure Redis, edit the configuration file `/etc/redis.conf` .

Since Redis is being used as a cache where every key will have an expiry set,
the following configuration values should be set (assuming a max memory limit of
2 megabytes as an example):

```config
maxmemory 2mb
maxmemory-policy allkeys-lru
```

To set the Redis listen port, the following configuration values should be
set (assuming the default Redis port 6379 as an example):

```config
port 6379
```

If you are on Ubuntu, run the following to restart Redis:

```bash
sudo service redis-server stop
sudo service redis-server start
```

If you are on CentOS, run the following to restart Redis and ensure redis-server
is enabled to start on boot:

```bash
systemctl stop redis.service
systemctl start redis.service
systemctl enable redis.service
```

To verify Redis is running and listening on the expected port, run the following
(using the loopback interface and the default Redis port 6379 as an example):

```bash
redis-cli -h 127.0.0.1 -p 6379 ping
```

Redis should respond with `PONG`.

If Redis did not respond with the expected output, run the following to verify
that Redis is running on the expected port:

```bash
ss -nlp |grep [r]edis
```

>**Notes:** ss is used here to support a minimal installed system, but netstat may be used as well.

### Install Riak Redis Add-on (RRA)

>**Note:**
>Riak Redis Add-on (RRA) is available to Enterprise customers for download in the usual Zendesk forums.

If you are on CentOS, run the following to install RRA:

```bash
sudo yum -y localinstall cache_proxy_ee_1.1.0_x86_64.rpm
```

If you are on Ubuntu, run the following to install RRA:

```bash
sudo dpkg -i cache_proxy_ee_1.1.0_amd64.deb
```

## Configuring Riak Redis Add-on

To configure Riak Redis Add-on (RRA), edit the configuration file: /etc/cache_proxy/cache_proxy_22122.yml.

The RRA configuration file is in YAML format. An example configuration
file is provided in the install, and it contains all relevant configuration elements:

```config
» XML node name« :
  listen: 0.0.0.0:22122
  hash: fnv1a_64
  distribution: ketama
  auto_eject_hosts: true
  redis: true
  server_retry_timeout: 2000
  server_failure_limit: 1
  server_ttl: 1h
  servers:
    - 127.0.0.1:6379:1
  backend_type: riak
  backend_max_resend: 2
  backends:
    - 127.0.0.1:8087
```

Set the `listen` configuration value to set the RRA listen port.

To set the time-to-live (TTL) for values stored in cache, set the `server_ttl`
configuration value. Human-readable time values can be specified,
with the most likely units being `s` for seconds or `ms` for milliseconds.

Set the list of Redis servers by listing the servers, separated by `-`, under the `servers` configuration value in the format `»host«:»port«:»weight«` (weight is optional).

Set the list of Riak KV servers by listing the servers, separated by `-`, under the `backends` configuration value in the format `»host«:»port«:»weight«`
(weight is optional). You will want to make sure to list the Riak KV protobuf (pb) port here.

### Verify your configuration

If you are on Ubuntu, run the following to start RRA:

```bash
sudo service cache_proxy start
```

If you are on CentOS, run the following to restart Redis and ensure redis-server
is enabled to start on boot:

```bash
systemctl start cache_proxy
```

To verify RRA is running and listening on the expected port, run the
following (using the loopback interface and the default RRA port 22122
as an example):

```bash
redis-cli -h 127.0.0.1 -p 22122 set test:redis-add-on SUCCESS
redis-cli -h 127.0.0.1 -p 22122 get test:redis-add-on SUCCESS
```

Redis should respond with `SUCCESS`.

If RRA is responding with the expected output, run the following to
clean up and remove the test value:

```bash
redis-cli -h 127.0.0.1 -p 22122 del test:redis-add-on
```

If you did not get the expected output, run the following
to verify that RRA is running on the expected port:

```bash
ss -nlp |grep [n]utcracker
```

>**Note:** ss is used here to support a minimal installed system, but netstat may be used as well.

## Next Steps

Get started with some [basic usage][addon redis use] or checkout out more info on [setting up for development (with examples)][addon redis develop].
