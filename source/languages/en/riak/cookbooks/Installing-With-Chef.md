---
title: Installing Riak with Chef
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, installing, chef]
---

If you manage your infrastructure with the open source systems integration
framework [Chef](http://www.opscode.com/chef/), you'll be happy to know that it
is possible to install Riak with Chef by using the [Riak Chef
cookbook](http://community.opscode.com/cookbooks/riak) located in the Opscode
cookbook repository.

## Getting Started

After downloading the Riak cookbook, you can use it by adding `riak` to the
runlist for a node configuration. The default settings will cause Riak to be
installed and configured as a single node.

Creating a cluster of nodes requires you set appropriate attributes,
particularly the Erlang node name (`-name`).

### Package Installation

There are three options for installation: `source`, `package`, and
`enterprise_package`. `package` is the default (installs Riak open source),
and recommended option for Red Hat and Debian based operating systems. For
source installations of Riak Erlang/OTP R15B01 and above is recommended.

### Basic Configuration

Most of the Riak configuration is for networking, Erlang, and storage
backends.

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
node['riak']['config']['riak_core']['http']
node['riak']['config']['riak_api']['pb_ip']
node['riak']['config']['riak_api']['pb_port']
```

Intra-cluster handoff occurs over a dedicated port, which defaults to 8099.

```ruby
node['riak']['config']['riak_core']['handoff_port']
```

Finally, by default, options are included in the configuration to define the
set of ports used for Erlang inter-node communication.

```ruby
default['riak']['config']['kernel']['inet_dist_listen_min']
default['riak']['config']['kernel']['inet_dist_listen_max']
```

#### Erlang

A number of Erlang parameters may be configured through the cookbook. The node
`-name` and `-setcookie` are most important for creating multi-node clusters.
The rest of the parameters are primarily for performance tuning, with kernel
polling and SMP enabled by default.

```ruby
node['riak']['args']['-name']
node['riak']['args']['-setcookie']
node['riak']['args']['+K']
node['riak']['args']['+A']
node['riak']['args']['+W']
node['riak']['args']['-env']['ERL_MAX_PORTS']
node['riak']['args']['-env']['ERL_FULLSWEEP_AFTER']
node['riak']['args']['-env']['ERL_CRASH_DUMP']
node['riak']['args']['-env']['ERL_MAX_ETS_TABLES']
node['riak']['args']['-smp']
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
node['riak']['config']['bitcask']['io_mode']
node['riak']['config']['bitcask']['data_root']
```

##### LevelDB

Settings for the LevelDB backend. See the [[LevelDB]] documentation for more
information.

```ruby
node['riak']['config']['eleveldb']['data_root']
```

### Lager

[Lager](https://github.com/basho/lager) is the logging framework used within
Riak. It can also be used with Erlang/OTP.

```ruby
node['riak']['config']['lager']['handlers']['lager_file_backend']
node['riak']['config']['lager']['crash_log']
node['riak']['config']['lager']['crash_log_msg_size']
node['riak']['config']['lager']['crash_log_size']
node['riak']['config']['lager']['crash_log_date']
node['riak']['config']['lager']['crash_log_count']
node['riak']['config']['lager']['error_logger_redirect']
```

### Sysmon

Sysmon monitors Riak garbage collection process and logs relevant information
to the status of garbage collection.

```ruby
node['riak']['config']['riak_sysmon']['process_limit']
node['riak']['config']['riak_sysmon']['port_limit']
node['riak']['config']['riak_sysmon']['gc_ms_limit']
node['riak']['config']['riak_sysmon']['heap_word_limit']
node['riak']['config']['riak_sysmon']['busy_port']
node['riak']['config']['riak_sysmon']['busy_dist_port']
```

### Index Merge

Settings pertaining to Secondary Index and Riak Search indices.

```ruby
node['riak']['config']['merge_index']['data_root']
node['riak']['config']['merge_index']['buffer_rollover_size']
node['riak']['config']['merge_index']['max_compact_segments']
```

## Additional Resources

More information related to cluster configuration and building development
environments is available in our documentation.

* [[Basic Cluster Setup]]
* [[The Riak Fast Track]]
