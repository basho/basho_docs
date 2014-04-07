---
title: Riak Monitoring with collectd and Graphite
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, monitoring, collectd]
---



## Riak and collectd

Using [collectd](http://collectd.org) in conjunction with Riak involves just two basic steps:

1. Installing collectd
2. Configuring you collectd installation's `collectd.conf` file

#### Installing collectd

Detailed and OS-specific installation instructions can be found on collectd's [downloads page](https://collectd.org/download.shtml).

While we won't detail the installation process here, it's important to note that it might be useful to install binary collectd distributions using a tool such as `[tar](http://unixhelp.ed.ac.uk/CGI/man-cgi?tar)`. In general, binary distributions are easier to copy to and install on multiple Riak nodes. Instructions on creating binary distributions can also be found on the collectd [downloads page](https://collectd.org/download.shtml).

#### Setting up Monitoring with collectd

Once collectd has been installed, the installation's `[collectd.conf](http://collectd.org/documentation/manpages/collectd.conf.5.shtml)` file needs to be configured to tell collectd which plugins to use, how each plugin should be configured, which local URL to interact with, and so on.

The following example `collectd.conf` file sets up collectd to use the following plugins:

Plugin | Description
:------|:-----------
[syslog](https://collectd.org/wiki/index.php/Plugin:SysLog) | Receives log messages from the collectd daemon and dispatches them to syslog. In this installation, messages at log levels `info` and `debug` will be emitted.
[logfile](https://collectd.org/wiki/index.php/Plugin:LogFile) | Receives log messages from the daemon and writes them to a text file. Here, those messages are written to `/var/log/collectd.log` via stdout and include timestamps and `info`- and `debug`-level log messages.
[cpu](https://collectd.org/wiki/index.php/Plugin:CPU) | Collects the amount of time spent by the CPU in various states, e.g. executing user code, executing system code, being idle, etc.
[cURL_JSON](https://collectd.org/wiki/index.php/Plugin:cURL-JSON) | Queries an HTTP endpoint and parses it as JSON, specified by `URL`. This is particularly useful in Riak, as the `[[/stats|HTTP Status]]` endpoint of any Riak node returns a wide variety of Riak metrics as JSON. The cURL_JSON plugin here is set up here to display the values of a wide variety of keys in JSON returned from `/stats`, e.g. `memory_total`, `memory_proccesses`, etc. A complete list of these metrics can be found in the documentation on [[Inspecting a Node]].
[FileCount](https://collectd.org/wiki/index.php/Plugin:FileCount) | Counts the number of files in a directory and all of its subdirectories. 

In addition to configuring plugins, 

**Note**: The specifics of your operating system and Riak installation will likely differ from those in this configuration file. It should be used only as a suggestive template.

```
Hostname    "ubuntu-12"
FQDNLookup   true
BaseDir     "/var/lib/collectd"
PIDFile     "/var/run/collectd.pid"
PluginDir   "/usr/local/lib/collectd"
TypesDB     "/usr/local/share/collectd/types.db"

LoadPlugin syslog
<Plugin syslog>
  LogLevel info
  LogLevel debug
</Plugin>

LoadPlugin logfile
<Plugin logfile>
    LogLevel debug
    File STDOUT
    LogLevel info
    File "/var/log/collectd.log"
    Timestamp true
</Plugin>

LoadPlugin cpu
LoadPlugin curl_json
LoadPlugin filecount
LoadPlugin interface
LoadPlugin load
LoadPlugin memory
LoadPlugin network
LoadPlugin uptime
LoadPlugin write_graphite

<Plugin curl_json>
  <URL "http://localhost:8098/stats"> 
    Instance "riak@10.0.3.2"
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
      # NOTE: "counter" vs "gauge"
      # A counter type is used for always-increasing values where the
      # rate of change (derivative) is more important than the
      # increasing value itself. Using a gauge type here will just show
      # an increasing line, while counter will show spikes if the
      # pbc_connects_total goes up quickly.
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

<Plugin filecount>
    <Directory "/var/lib/riak/bitcask">
        Instance "riak@10.0.3.2"
        Name "*.data"
        Recursive true
        IncludeHidden false
    </Directory>
</Plugin>

<Plugin write_graphite>
  <Carbon>
    Host "10.0.3.3"
    Port "2003"
    Protocol "tcp"
    LogSendErrors true
    # Prefix "collectd"
    Postfix "-collectd"
    StoreRates true
    AlwaysAppendDS false
    EscapeCharacter "_"
  </Carbon>
</Plugin>
```

#### Setting up Graphite

In the example configuration file above, you'll see that the [write_graphite]()

## Riak and Graphite


[Monitoring Riak with collectd blog post](http://www.the-eleven.com/tlegg/blog/2012/05/28/monitoring-riak-collectd-5/)
[Graphite](http://graphite.wikidot.com/faq)
[Graphite docs](http://graphite.readthedocs.org/en/latest/)