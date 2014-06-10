---
title: Installing Riak with Chef
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, installing, chef]
moved: {
    '1.4.0-': '/cookbooks/Installing-With-Chef'
}
---

If you manage your infrastructure with [Chef](http://www.opscode.com/chef/),
the open source configuration management framework, you'll be happy to know
that we maintain a [cookbook](http://community.opscode.com/cookbooks/riak) to
install Riak with Chef.

## Getting Started

The Riak cookbook can be used just by adding `recipe[riak]` to the runlist for
a node. The default settings will cause Riak to be installed and configured
via Basho maintained package repositories.

### Package Installation

There are three options for installation: `source`, `package`, and
`enterprise_package`. `package` is the default (installs Riak open source),
and is the recommended option for Red Hat and Debian based operating systems.
For source installations of Riak, Erlang/OTP R15B01 and above is recommended.

### Source Installation

The `source` recipe can be used to install Riak from source. The source
installation requires the `git`, `build-essential`, and `erlang` cookbooks.

### Enterprise Installation

To install Riak Enterprise, populate
`node['riak']['package']['enterprise_key']` with a Basho provided key for the
release.

Riak Enterprise installations managed through the cookbook must be installed
via a package.

### Basic Configuration

All the configuration options exist within the `node['riak']['config']`
namespace. In cases where an Erlang data type is necessary, use the
appropriate methods from the
[erlang_template_helper](https://github.com/basho/erlang_template_helper).

#### Networking

Riak clients communicate with the nodes in the cluster through either the HTTP
or Protocol Buffers interfaces, both of which can be used simultaneously.
Configuration for each interface includes the IP address and TCP port on which
to listen for client connections. The default for the HTTP interface is
`localhost:8098` and for Protocol Buffers `0.0.0.0:8087` (meaning client
connections to any address on the server, TCP port 8087, are accepted).

As the default HTTP configuration is inaccessible to other nodes, it must be
changed if you want clients to use the HTTP interface. That said, it is not
recommended to allow clients direct connection with some type of load
balancing solution between Riak and client traffic.

```ruby
default['riak']['config']['riak_core']['http'] = [[node['ipaddress'].to_erl_string, 8098].to_erl_tuple]
default['riak']['config']['riak_api']['pb'] = [[node['ipaddress'].to_erl_string, 8087].to_erl_tuple]
```

Intra-cluster handoff occurs over a dedicated port, which defaults to `8099`.

```ruby
default['riak']['config']['riak_core']['handoff_port'] = 8099
```

Finally, by default, options are included in the configuration to define the
set of ports used for Erlang inter-node communication.

```ruby
default['riak']['config']['kernel']['inet_dist_listen_min'] = 6000
default['riak']['config']['kernel']['inet_dist_listen_max'] = 7999
```

#### Erlang

A number of Erlang parameters may be configured through the cookbook. The node
`-name` and `-setcookie` are most important for creating multi-node clusters.

The rest of the parameters are primarily for performance tuning, with kernel
polling and SMP enabled by default. A few examples follow:

```ruby
default['riak']['args']['-name'] = "riak@#{node['fqdn']}"
default['riak']['args']['-setcookie'] = "riak"
default['riak']['args']['+K'] = true
default['riak']['args']['+A'] = 64
default['riak']['args']['+W'] = "w"
default['riak']['args']['-env']['ERL_MAX_PORTS'] = 4096
default['riak']['args']['-env']['ERL_FULLSWEEP_AFTER'] = 0
default['riak']['args']['-env']['ERL_CRASH_DUMP'] = "/var/log/riak/erl_crash.dump"
default['riak']['args']['-env']['ERL_MAX_ETS_TABLES'] = 8192
default['riak']['args']['-smp'] = "enable"
```

#### Storage Backends

Riak requires specification of a storage backend along with various backend
storage options specific to each backend. While Riak supports specification of
different backends for different buckets, the Chef cookbook does not yet allow
such configurations.

The most common backends are [[Bitcask]], [[LevelDB]], and the [[multi
backend|multi]]. The typical configuration options and their defaults are
given below.

##### Bitcask

Settings for the default Bitcask backend. See the [[Bitcask]] documentation
for more information.

```ruby
default['riak']['config']['bitcask']['io_mode'] = "erlang"
default['riak']['config']['bitcask']['data_root'] = "/var/lib/riak/bitcask".to_erl_string
```

##### LevelDB

Settings for the LevelDB backend. See the [[LevelDB]] documentation for more
information.

```ruby
default['riak']['config']['eleveldb']['data_root'] = "/var/lib/riak/leveldb".to_erl_string
```

### Lager

[Lager](https://github.com/basho/lager) is the logging framework used within
Riak. It can also be used with Erlang/OTP.

```ruby
error_log = ["/var/log/riak/error.log".to_erl_string,"error",10485760,"$D0".to_erl_string,5].to_erl_tuple
info_log = ["/var/log/riak/console.log".to_erl_string,"info",10485760,"$D0".to_erl_string,5].to_erl_tuple

default['riak']['config']['lager']['handlers']['lager_file_backend'] = [error_log, info_log]
default['riak']['config']['lager']['crash_log'] = "/var/log/riak/crash.log".to_erl_string
default['riak']['config']['lager']['crash_log_msg_size'] = 65536
default['riak']['config']['lager']['crash_log_size'] = 10485760
default['riak']['config']['lager']['crash_log_date'] = "$D0".to_erl_string
default['riak']['config']['lager']['crash_log_count'] = 5
default['riak']['config']['lager']['error_logger_redirect'] = true
```

### Sysmon

Sysmon monitors Riak garbage collection process and logs relevant information
to the status of garbage collection.

```ruby
default['riak']['config']['riak_sysmon']['process_limit'] = 30
default['riak']['config']['riak_sysmon']['port_limit'] = 2
default['riak']['config']['riak_sysmon']['gc_ms_limit'] = 0
default['riak']['config']['riak_sysmon']['heap_word_limit'] = 40111000
default['riak']['config']['riak_sysmon']['busy_port'] = true
default['riak']['config']['riak_sysmon']['busy_dist_port'] = true
```

### Index Merge

Settings pertaining to Secondary Index and Riak Search indexes.

```ruby
default['riak']['config']['merge_index']['data_root'] = "/var/lib/riak/merge_index".to_erl_string
default['riak']['config']['merge_index']['buffer_rollover_size'] = 1048576
default['riak']['config']['merge_index']['max_compact_segments'] = 20
```

## Additional Resources

More information related to cluster configuration and building development
environments is available in our documentation.

* [[Five-Minute Install]]
