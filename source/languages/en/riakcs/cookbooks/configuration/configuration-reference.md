---
title: Riak CS Configuration Reference
project: riakcs
version: 1.5.0+
document: reference
audience: intermediate
keywords: [cs, operator, configuration]
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
section below. All other settings are managed through `app.config`,
which is divided up into the following sections:

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

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code></code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code></code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code></code></td>
<td></td>
<td><code></code></td>
</tr>
</tbody>
</table>


<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>cs_ip</code></td>
<td>The IP address for the Riak CS node</td>
<td><code>127.0.0.1</code></td>
</tr>
<tr>
<td><code>cs_port</code></td>
<td>The TCP port for the Riak CS node (whether HTTP or HTTPS)</td>
<td><code>8087</code></td>
</tr>
<tr>
<td><code>riak_pb_port</code></td>
<td>The TCP port for the Riak CS node's corresponding Riak node (used by
Riak's [[Protocol Buffers|PBC API]] interface)
<td><code>127.0.0.1</code></td>
</tr>
<tr>
<td><code>cs_root_host</code></td>
<td>The root host name accepted by Riak CS.  Changing this setting to,
for example, <code>my_cs_host</code> would enable users to make requests
to a URL such as <code>http://bucket.my_cs_host/object/</code> (or to the
corresponding HTTP host).</td>
<td><code>s3.amazonaws.com</code></td>
</tr>
</tbody>
</table>

## Connection Pools

Riak CS enables you to establish connection pools for normal requests
(such as `GET` and `PUT`) as well as for bucket listing requests. Each
pool is specified as a nested tuple of the following form:

```appconfig
{riak_cs, [
           {Name, {FixedSize, OverflowSize}}
          ]}
```

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>request_pool</code></td>
<td></td>
<td><code>{128, 0}</code></td>
</tr>
<tr>
<td><code></code></td>
<td></td>
<td><code></code></td>
</tr>
</tbody>
</table>

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
