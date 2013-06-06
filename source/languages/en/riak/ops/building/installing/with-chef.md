---
title: Installing Riak with Chef
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, installing, chef]
moved: {
    '1.4.0-': '/cookbooks/Installing-With-Chef/'
}
---

If you manage your infrastructure with the open source systems integration framework [Chef](http://www.opscode.com/chef/), you'll be happy to know that it is possible to install
Riak with Chef by using the [Riak Chef cookbook](http://community.opscode.com/cookbooks/riak) located in the Opscode cookbook repository.

## Getting Started
After downloading the Riak cookbook, you can use it by adding `riak` to the runlist for a node configuration. The default settings will cause Riak to be installed and configured as a single node.

Creating a cluster of nodes requires you set appropriate attributes, particularly the Erlang `node_name`, and either manually join nodes to the cluster or use the gossip seed configuration option.

### Package Installation
There are two options for package installation: source and binary. Binary installation is the default and recommended option for Red Hat and Debian based operating systems. For source installations of Riak version 1.2 Erlang/OTP R15B01 is recommended.

The available package parameters are version, type and an optional install prefix for source installation:

    node[:riak][:package][:version][:major] = "0"
    node[:riak][:package][:version][:minor] = "11"
    node[:riak][:package][:version][:incremental] = "0"
    node[:riak][:package][:type] = ("binary" | "source")
    node[:riak][:package][:prefix] = "/usr/local"


### Basic Configuration
Most of the Riak configuration is for networking, Erlang, and storage backends.

#### Networking
Riak clients communicate with the nodes in the cluster through either the HTTP or Protobufs interfaces, both of which may be used simultaneously. Configuration for each interface includes the IP address and TCP port on which to listen for client connections. The default for the HTTP interface is localhost:8098 and for Protobufs 0.0.0.0:8087, meaning client connections to any address on the server, TCP port 8087, are accepted.

As the default HTTP configuration is inaccessible to other nodes, it must be changed if you want clients to use the HTTP interface, though it is not recommended to allow clients direct connection with some type of load balancing solution between Riak and client traffic.

    node[:riak][:core][:http] = [["127.0.0.1", 8098]]
    node[:riak][:kv][:pb_ip] = "0.0.0.0"
    node[:riak][:kv][:pb_port] = 8087

Intra-cluster handoff occurs over a dedicated port, which defaults to 8099.

    node[:riak][:core][:handoff_port] = 8099

Finally, by default, options are included in the configuration to define the set of ports used for Erlang inter-node communication.

    node[:riak][:kernel][:limit_port_range] = true
    node[:riak][:kernel][:inet_dist_listen_min] = 6000
    node[:riak][:kernel][:inet_dist_listen_max] = 7999

On Debian/Ubuntu platforms, [IPTables](http://wiki.debian.org/iptables) rules are automatically generated based on these settings, which explicitly allow these ports and addresses.

#### Erlang
A number of Erlang parameters may be configured through the cookbook. The node name and cookie are most important for creating multi-node clusters. The rest of the parameters are primarily for performance tuning, with kernel polling and SMP enabled by default. Any available Erlang environment variable may be set with the `env_vars` hash.

    node[:riak][:erlang][:node_name] = "riak@#{node[:fqdn]}"
    node[:riak][:erlang][:cookie] = "riak"
    node[:riak][:erlang][:kernel_polling] = (true | false)
    node[:riak][:erlang][:async_threads] = 64
    node[:riak][:erlang][:smp] = ("enable" | "disable")
    node[:riak][:erlang][:env_vars][:ERL_MAX_PORTS] = 4096
    node[:riak][:erlang][:env_vars][:ERL_FULLSWEEP_AFTER] = 10

#### Storage Backends
Riak requires specification of a storage backend along with various backend storage options specific to each backend. While Riak supports specification of different backends for different buckets, the Chef cookbook does not yet allow such configurations.

The most common backends are [[Bitcask]] \(the default), [[LevelDB]], and the [[multi backend|multi]]. The typical configuration options and their defaults are given below.

##### Bitcask
Settings for the default Bitcask backend. See the [[Bitcask]] documentation for more information.

    node[:riak][:bitcask][:data_root] = "/var/lib/riak/bitcask"
    node[:riak][:bitcask][:max_file_size] = 2147483648
    node[:riak][:bitcask][:open_timeout] = 4
    node[:riak][:bitcask][:sync_strategy] = :none
    node[:riak][:bitcask][:frag_merge_trigger] = 60
    node[:riak][:bitcask][:dead_bytes_merge_trigger] = 536870912
    node[:riak][:bitcask][:frag_threshold] = 40
    node[:riak][:bitcask][:dead_bytes_threshold] = 134217728
    node[:riak][:bitcask][:small_file_threshold] = 10485760
    node[:riak][:bitcask][:expiry_secs] = -1

##### LevelDB
Settings for the LevelDB backend. See the [[LevelDB]] documentation for more information. Currently, only setting the root directory is supported. If
you want support for more options, add them in `attributes/eleveldb.rb`.

    node[:riak][:kv][:riak_kv_eleveldb_backend_root] = "/var/lib/riak/leveldb"

### Lager
[Lager](https://github.com/basho/lager) is the logging framework used within Riak. It can also be used with erlang/OTP.

    node[:riak][:lager][:handlers][:lager_console_backend]= :info
    node[:riak][:lager][:crash_log] = "/var/log/riak/crash.log"
    node[:riak][:lager][:crash_log_date] = "$D0"
    node[:riak][:lager][:crash_log_msg_size]  = 65536
    node[:riak][:lager][:crash_log_size] = 10485760
    node[:riak][:lager][:error_logger_redirect] = true
    node[:riak][:lager][:handlers][:lager_file_backend][:lager_error_log] =  ["/var/log/riak/error.log", :error, 10485760, "$D0", 5]
    node[:riak][:lager][:handlers][:lager_file_backend][:lager_console_log] = ["/var/log/riak/console.log", :info, 10485760, "$D0", 5]

### Sysmon
Sysmon monitors Riak garbage collection process and logs relevant information to the status of garbage collection.

    node[:riak][:sysmon][:process_limit] = 30
    node[:riak][:sysmon][:port_limit] = 30
    node[:riak][:sysmon][:gc_ms_limit] = 50 #if gc takes longer than 50ms. Spam the log.
    node[:riak][:sysmon][:heap_word_limit] = 10485760

### Index Merge
Settings pertaining to Secondary Index and Riak Search indices.

    node[:riak][:merge_index][:data_root] = "/var/lib/riak/merge_index"
    node[:riak][:merge_index][:data_root_2i] = "/var/lib/riak/merge_index_2i"
    node[:riak][:merge_index][:buffer_rollover_size] = 1048576
    node[:riak][:merge_index][:max_compact_segments] = 20

## Additional Resources
More information related to cluster configuration and building development environments is available in our documentation.

* [[Five Minute Install]]
