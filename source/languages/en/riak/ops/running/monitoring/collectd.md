---
title: Riak Monitoring with collectd
project: riak
version: 2.0.0+
document: tutorial
toc: true
audience: intermediate
keywords: [operator, monitoring, collectd]
---

This tutorial guides you through using the [collectd](http://collectd.org) data collection daemon to gather performance and other data from Riak.

Using [collectd](http://collectd.org) in conjunction with Riak involves three basic steps:

1. Installing collectd
2. Configuring your collectd installation's `[collectd.conf](http://collectd.org/documentation/manpages/collectd.conf.5.shtml)` configuration file
3. Starting the daemon

## Installing collectd

Detailed and OS-specific installation instructions can be found on collectd's [downloads page](https://collectd.org/download.shtml).

While we won't detail the installation process here, it's important to note that it might be useful to install binary collectd distributions using a tool such as `[tar](http://unixhelp.ed.ac.uk/CGI/man-cgi?tar)`. In general, binary distributions are easier to copy to and install on multiple Riak nodes. Instructions on creating binary distributions can also be found on the collectd [downloads page](https://collectd.org/download.shtml).

## Configuring collectd

Once collectd has been installed, the installation's `[collectd.conf](http://collectd.org/documentation/manpages/collectd.conf.5.shtml)` file needs to be configured to tell collectd which plugins to use, how each plugin should be configured, which local URL to interact with, and so on.

The following example `collectd.conf` file sets up collectd for use with a Riak installation with the following characteristics:

Setting | Value
:-------|:-----
Hostname | `ubuntu-12`
Base directory for collectd | `/var/lib/collectd`
collectd [`.pid` file](http://collectd.org/documentation/manpages/collectd.1.shtml) | `/var/run/collectd.pid`
collectd [plugin](https://collectd.org/wiki/index.php/Table_of_Plugins) directory | `/usr/local/lib/collectd`
collectd [`types.db`](http://collectd.org/documentation/manpages/types.db.5.shtml) file | `/usr/local/share/collectd/types.db`
Riak stats URL | `http://localhost:8098/stats`
Riak node/instance name | `riak1@127.0.0.1`

The configuration file also sets up collectd to use the following plugins:

Plugin | Description
:------|:-----------
[syslog](https://collectd.org/wiki/index.php/Plugin:SysLog) | Receives log messages from the collectd daemon and dispatches them to syslog. In this installation, messages at log levels `info` and `debug` will be emitted.
[logfile](https://collectd.org/wiki/index.php/Plugin:LogFile) | Receives log messages from the daemon and writes them to a text file. Here, those messages are written to `/var/log/collectd.log` via stdout and include timestamps and `info`- and `debug`-level log messages. This is useful primarily when setting up collectd and testing it by running it in the foreground.
[cURL_JSON](https://collectd.org/wiki/index.php/Plugin:cURL-JSON) | Queries an HTTP endpoint and parses it as JSON, specified by `URL`. This is particularly useful in Riak, as the `/stats` endpoint of any Riak node returns a wide variety of Riak metrics as JSON. The cURL_JSON plugin here is set up here to display the values of a wide variety of keys in JSON returned from `/stats`, e.g. `memory_total`, `memory_proccesses`, etc. A complete list of these metrics can be found in the documentation on [[Inspecting a Node]].

  **Note**: While the other plugins are optional and included here for illustrative purposes, cURL_JSON plugin support is required to use collectd with Riak.

<div class="note">
<div class="title">Note on template</div>
The specifics of your operating system, Riak node(s), and collectd installation may differ markedly from those in this configuration file. It should be used only as a suggestive template. We recommend checking out the <a href="http://collectd.org/documentation.shtml">collectd documentation</a> for more information.
</div>

```
Hostname    "ubuntu-12"
BaseDir     "/var/lib/collectd"
PIDFile     "/var/run/collectd.pid"
PluginDir   "/usr/local/lib/collectd"
TypesDB     "/usr/local/share/collectd/types.db"

LoadPlugin logfile
LoadPlugin syslog
LoadPlugin curl_json

<Plugin syslog>
  LogLevel info
  LogLevel debug
</Plugin>

<Plugin logfile>
    LogLevel debug
    File STDOUT
    LogLevel info
    File "/var/log/collectd.log"
    Timestamp true
</Plugin>

<Plugin curl_json>
  <URL "http://localhost:8098/stats"> 
    Instance "riak1@127.0.0.1"
    <Key "memory_total">
      Type "bytes"
    </Key>
    <Key "memory_processes">
      Type "bytes"
    </Key>
    <Key "memory_system">
      Type "bytes"
    </Key>
    <Key "memory_code">
      Type "bytes"
    </Key>
    <Key "memory_ets">
      Type "bytes"
    </Key>
    <Key "cpu_avg1">
      Type "gauge"
    </Key>
    <Key "cpu_avg5">
      Type "gauge"
    </Key>
    <Key "cpu_avg15">
      Type "gauge"
    </Key>
    <Key "pbc_active">
      Type "gauge"
    </Key>
    <Key "pbc_connects_total">
      Type "counter"
    </Key>
    <Key "node_gets">
      Type "gauge"
    </Key>
    <Key "node_gets_total">
      Type "counter"
    </Key>
    <Key "node_get_fsm_time_mean">
      Type "gauge"
    </Key>
    <Key "node_get_fsm_time_median">
      Type "gauge"
    </Key>
    <Key "node_get_fsm_time_95">
      Type "gauge"
    </Key>
    <Key "node_get_fsm_time_99">
      Type "gauge"
    </Key>
    <Key "node_get_fsm_time_100">
      Type "gauge"
    </Key>
    <Key "node_get_fsm_objsize_mean">
      Type "gauge"
    </Key>
    <Key "node_get_fsm_objsize_median">
      Type "gauge"
    </Key>
    <Key "node_get_fsm_objsize_95">
      Type "gauge"
    </Key>
    <Key "node_get_fsm_objsize_99">
      Type "gauge"
    </Key>
    <Key "node_get_fsm_objsize_100">
      Type "gauge"
    </Key>
    <Key "node_get_fsm_siblings_mean">
      Type "gauge"
    </Key>
    <Key "node_get_fsm_siblings_median">
      Type "gauge"
    </Key>
    <Key "node_get_fsm_siblings_95">
      Type "gauge"
    </Key>
    <Key "node_get_fsm_siblings_99">
      Type "gauge"
    </Key>
    <Key "node_get_fsm_siblings_100">
      Type "gauge"
    </Key>
    <Key "node_puts">
      Type "gauge"
    </Key>
    <Key "node_puts_total">
      Type "counter"
    </Key>
    <Key "node_put_fsm_time_mean">
      Type "gauge"
    </Key>
    <Key "node_put_fsm_time_median">
      Type "gauge"
    </Key>
    <Key "node_put_fsm_time_95">
      Type "gauge"
    </Key>
    <Key "node_put_fsm_time_99">
      Type "gauge"
    </Key>
    <Key "node_put_fsm_time_100">
      Type "gauge"
    </Key>
    <Key "vnode_gets">
      Type "gauge"
    </Key>
    <Key "vnode_gets_total">
      Type "counter"
    </Key>
    <Key "vnode_puts">
      Type "gauge"
    </Key>
    <Key "vnode_puts_total">
      Type "counter"
    </Key>
    <Key "vnode_index_reads">
      Type "gauge"
    </Key>
    <Key "vnode_index_writes">
      Type "gauge"
    </Key>
  </URL>
</Plugin>
```

## Starting collectd

Once collectd has been installed and configured, you can start the daemon using the `collectd` script in the `/sbin` subdirectory of your collectd installation.
