---
title: "Installing Riak CS With Chef"
description: ""
menu:
  riak_cs-2.1.1:
    name: "Installing With Chef"
    identifier: "installing_chef"
    weight: 201
    parent: "index"
project: "riak_cs"
project_version: "2.1.1"
aliases:
  - /riakcs/2.1.1/cookbooks/installing/Riak-CS-Using-Chef/
  - /riak/cs/2.1.1/cookbooks/installing/Riak-CS-Using-Chef/
---

If you manage your infrastructure with [Chef](http://www.opscode.com/chef/),
the open-source configuration management framework, you'll be happy to know
that we maintain a [cookbook](http://community.opscode.com/cookbooks/riak-cs)
for installing Riak CS with Chef.

## Getting Started

The Riak CS cookbook can be used (alongside the Riak cookbook), by adding the
following recipes to your runlist:

```ruby
run_list(
  "recipe[riak-cs::package]",
  "recipe[riak]",
  "recipe[riak-cs]",
  "recipe[riak-cs::stanchion]"
)
```

The default settings will cause Riak and Riak CS to be installed and configured via Basho-maintained package repositories.

### Package Installation

There are two options for installation: `package` and `enterprise_package`.
`package` is the default--- it installs Riak CS open source---and is the recommended option for Red Hat- and Debian-based operating systems. For source installations of Riak, Erlang/OTP R15B01 and above is recommended.

### Enterprise Installation

To install Riak CS Enterprise, populate `node['riak_cs']['package']['enterprise_key']` with a Basho-provided key for
the release.

Riak Enterprise installations managed through the cookbook must be installed
via a package.

### Basic Configuration

All the configuration options exist within the `node['riak_cs']['config']`
namespace. In cases where an Erlang data type is necessary, use the appropriate methods from the `[erlang_template_helper](https://github.com/basho/erlang_template_helper)`.

#### Networking

Riak CS and Stanchion communicate with Riak through the Protocol Buffers interface. By default, Riak listens for Protocol Buffers connections on port
`8087`:

```ruby
# Riak CS
default['riak_cs']['config']['riak_cs']['riak_ip'] = node['ipaddress'].to_erl_string
default['riak_cs']['config']['riak_cs']['riak_pb_port'] = 8087

# Stanchion
default['stanchion']['config']['stanchion']['riak_ip'] = node['ipaddress'].to_erl_string
default['stanchion']['config']['stanchion']['riak_pb_port'] = 8087
```

At the same time, Riak listens for HTTP requests on port `8080` and Stanchion
on port `8085`:

```ruby
# Riak CS
default['riak_cs']['config']['riak_cs']['cs_ip'] = node['ipaddress'].to_erl_string
default['riak_cs']['config']['riak_cs']['cs_port'] = 8080

# Stanchion
default['stanchion']['config']['stanchion']['stanchion_ip'] = node['ipaddress'].to_erl_string
default['stanchion']['config']['stanchion']['stanchion_port'] = 8085
```

#### Credentials

Both Riak CS and Stanchion require administrative user credentials. The two credentials are `admin_key` and `admin_secret`:

```ruby
# Riak CS
default['riak_cs']['config']['riak_cs']['admin_key'] = "admin-key".to_erl_string
default['riak_cs']['config']['riak_cs']['admin_secret'] = "admin-secret".to_erl_string

# Stanchion
default['stanchion']['config']['stanchion']['admin_key'] = "admin-key".to_erl_string
default['stanchion']['config']['stanchion']['admin_secret'] = "admin-secret".to_erl_string
```

#### Webmachine

Webmachine is used to service HTTP requests in Riak CS. Its `server_name` and
Lager `log_handlers` can be configured with the following:

```ruby
default['riak_cs']['config']['webmachine']['server_name'] = "Riak CS".to_erl_string
default['riak_cs']['config']['webmachine']['log_handlers']['webmachine_log_handler'] = ["/var/log/riak-cs".to_erl_string].to_erl_list
default['riak_cs']['config']['webmachine']['log_handlers']['riak_cs_access_log_handler'] = [].to_erl_list
```

#### Erlang

A number of Erlang parameters may be configured through the cookbook. The node
`-name` and `-setcookie` are most important for creating multi-node clusters.

The rest of the parameters are primarily for performance tuning, with kernel
polling and SMP enabled by default. A few examples follow:

```ruby
# Riak CS
default['riak_cs']['args']['-name'] = "riak-cs@#{node['fqdn']}"
default['riak_cs']['args']['-setcookie'] = "riak-cs"
default['riak_cs']['args']['+K'] = true
default['riak_cs']['args']['+A'] = 64
default['riak_cs']['args']['+W'] = "w"
default['riak_cs']['args']['-env']['ERL_MAX_PORTS'] = 4096
default['riak_cs']['args']['-env']['ERL_FULLSWEEP_AFTER'] = 0
default['riak_cs']['args']['-env']['ERL_CRASH_DUMP'] = "/var/log/riak/erl_crash.dump"

# Stanchion
default['stanchion']['args']['-name'] = "stanchion@#{node['ipaddress']}"
default['stanchion']['args']['-setcookie'] = "stanchion"
default['stanchion']['args']['+K'] = true
default['stanchion']['args']['+A'] = 64
default['stanchion']['args']['+W'] = "w"
default['stanchion']['args']['-env']['ERL_MAX_PORTS'] = 4096
default['stanchion']['args']['-env']['ERL_FULLSWEEP_AFTER'] = 0
default['stanchion']['args']['-env']['ERL_CRASH_DUMP'] = "/var/log/stanchion/erl_crash.dump"
```

#### Storage Backends

Riak CS uses a specific combination of storage backends. [Bitcask]({{<baseurl>}}riak/kv/2.1.3/setup/planning/backend/bitcask) is used to
store blocks and [LevelDB]({{<baseurl>}}riak/kv/2.1.3/setup/planning/backend/leveldb) to store manifests. The `riak_cs_kv_multi_backend` must be specified in the Riak configuration file for Riak CS to work:

```ruby
default['riak']['config']['riak_kv']['storage_backend'] = "riak_cs_kv_multi_backend"
```

The Riak cookbook takes care of populating all of the other default required
for the `riak_cs_kv_multi_backend` to be configured correctly.

### Lager

[Lager](https://github.com/basho/lager) is the logging framework used within
Riak CS and Stanchion. It can also be used with Erlang/OTP.

```ruby
# Riak CS
error_log = ["/var/log/riak-cs/error.log".to_erl_string,"error",10485760,"$D0".to_erl_string,5].to_erl_tuple
info_log = ["/var/log/riak-cs/console.log".to_erl_string,"info",10485760,"$D0".to_erl_string,5].to_erl_tuple

default['riak_cs']['config']['lager']['handlers']['lager_file_backend'] = [error_log, info_log]
default['riak_cs']['config']['lager']['crash_log'] = "/var/log/riak-cs/crash.log".to_erl_string
default['riak_cs']['config']['lager']['crash_log_msg_size'] = 65536
default['riak_cs']['config']['lager']['crash_log_size'] = 10485760
default['riak_cs']['config']['lager']['crash_log_date'] = "$D0".to_erl_string
default['riak_cs']['config']['lager']['crash_log_count'] = 5
default['riak_cs']['config']['lager']['error_logger_redirect'] = true

# Stanchion
error_log = ["/var/log/stanchion/error.log".to_erl_string,"error",10485760,"$D0".to_erl_string,5].to_erl_tuple
info_log = ["/var/log/stanchion/console.log".to_erl_string,"info",10485760,"$D0".to_erl_string,5].to_erl_tuple

default['stanchion']['config']['lager']['handlers']['lager_file_backend'] = [error_log, info_log]
default['stanchion']['config']['lager']['crash_log'] = "/var/log/stanchion/crash.log".to_erl_string
default['stanchion']['config']['lager']['crash_log_msg_size'] = 65536
default['stanchion']['config']['lager']['crash_log_size'] = 10485760
default['stanchion']['config']['lager']['crash_log_date'] = "$D0".to_erl_string
default['stanchion']['config']['lager']['crash_log_count'] = 5
default['stanchion']['config']['lager']['error_logger_redirect'] = true
```

## Additional Resources

More information related to cluster configuration and building development environments is available in our documentation.

* [Building a Local Test Environment]({{<baseurl>}}riak/cs/2.1.1/tutorials/fast-track/local-testing-environment)
* [Building a Virtual Testing Environment]({{<baseurl>}}riak/cs/2.1.1/tutorials/fast-track/virtual-test-environment)
