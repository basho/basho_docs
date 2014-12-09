---
Riak CS Configuration Reference
project: riakcs
version: 1.5.0+
document: reference
audience: intermediate
keywords: [riak-cs, operator, configuration]
---

This document is intended as a reference listing of all configurable
parameters for Riak CS. For a more narrative-style walkthrough of
configuring Riak CS, we recommend consulting the [[Configuring Riak CS]]
tutorial.

The configuration for Riak CS is handled through two files, `app.config`
and `vm.args`, both of which are located in each Riak CS node's `/etc`
directory. The `vm.args` file houses settings related to the [Erlang
VM](http://www.erlang.org/) on which both Riak and Riak CS run. These
settings are listed in the [Riak
documentation](http://docs.basho.com/riak/1.4.12/ops/advanced/configs/configuration-files/#Configuring-Your-code-vm-args-code-)
Reference#Erlang-VM-Settings]] section below. All other settings are
managed through `app.config`, which is divided up into the following
sections:

* `riak_cs` --- Most settings are housed in this section of the file
* `webmachine` --- Settings related to
  [Webmachine](https://github.com/basho/webmachine), the HTTP server
  framework that Riak CS uses for HTTP connections
* `lager` --- Settings for [lager](https://github.com/basho/lager), the
  Erlang logging framework used by Riak CS
* `sasl` --- There is only one setting in this section,
  `sasl_error_lager`, which determines whether and how Riak CS uses
  Erlang's [SASL error
  logger](http://www.erlang.org/doc/man/sasl_app.html)

## Connection Information

Config | Description | Default
:------|:------------|:-------
`cs_ip`
`cs_port`
`riak_ip`
`riak_pb_port`
`cs_root_host`

## Connection Pools

Config | Description | Default
:------|:------------|:-------
`request_pool`
`bucket_list_pool`

## Stanchion

Config | Description | Default
:------|:------------|:-------
`stanchion_ip`
`stanchion_port`
`stanchion_ssl`

## Admin and Authentication Settings

Config | Description | Default
:------|:------------|:-------
`admin_key`
`admin_secret`
`admin_ip`
`admin_port`
`anonymous_user_creation`
`rewrite_module`
`auth_module`

## Access Settings

Config | Description | Default
:------|:------------|:-------
`access_archive_period`
`access_archiver_max_backlog`
`access_log_flush_factor`
`access_log_flush_size`

## User Settings

Config | Description | Default
:------|:------------|:-------
`usage_request_limit`

## Storage Settings

Config | Description | Default
:------|:------------|:-------
`storage_schedule`
`storage_archive_period`

## Garbage Collection

Config | Description | Default
:------|:------------|:-------
`gc_interval`
`gc_paginated_indexes`
`gc_retry_interval`
`leeway_seconds`

## Miscellaneous Settings

Config | Description | Default
:------|:------------|:-------
`fold_objects_for_list_keys`
`n_val_1_get_requests`
`cs_version`
`trust_x_forwarded_for`
`dtrace_support`

## Webmachine

Config | Description | Default
:------|:------------|:-------
`server_name` |
`webmachine_log_handler`
`riak_cs_access_log_handler`

## Logging

Config | Description | Default
:------|:------------|:-------
`lager_console_backend`
`lager_file_backend`

Config | Description | Default
:------|:------------|:-------
`crash_log`
`crash_log_msg_size`
`crash_log_size`
`crash_log_date`
`crash_log_count`
`error_logger_redirect`

## SASL

Config | Description | Default
:------|:------------|:-------
`sasl_error_logger`
