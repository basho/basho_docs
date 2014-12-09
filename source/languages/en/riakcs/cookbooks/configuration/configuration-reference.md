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
<td>Settings for the general request pool for Riak CS. Please note that
we recommend setting Riak's <code>pb_backlog</code> setting higher than
<code>request_pool</code>'s fixed size, i.e. higher than 128. The
default for <code>pb_backlog</code> is 64.</td>
<td><code>{128, 0}</code></td>
</tr>
<tr>
<td><code>bucket_list_pool</code></td>
<td>Settings for the bucket listing request pool for Riak CS.</td>
<td><code>{5, 0}</code></td>
</tr>
</tbody>
</table>

## Stanchion

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>stanchion_ip</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>stanchion_port</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>stanchion_ssl</code></td>
<td></td>
<td><code></code></td>
</tr>
</tbody>
</table>

## Admin and Authentication Settings

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>admin_ip</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>admin_port</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>admin_key</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>admin_secret</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>anonymous_user_creation</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>auth_module</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>rewrite_module</code></td>
<td></td>
<td><code></code></td>
</tr>
</tbody>
</table>

## Access Settings

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>access_archive_period</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>access_archive_max_backlog</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>access_log_flush_size</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>access_log_flush_size</code></td>
<td></td>
<td><code></code></td>
</tr>
</tbody>
</table>

## User Settings

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>usage_request_limit</code></td>
<td></td>
<td><code></code></td>
</tr>
</tbody>
</table>

## Storage Settings

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>storage_schedule</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>storage_archive_period</code></td>
<td></td>
<td><code></code></td>
</tr>
</tbody>
</table>

## Garbage Collection

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>gc_interval</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>gc_paginated_indexes</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>gc_retry_interval</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>leeway_seconds</code></td>
<td></td>
<td><code></code></td>
</tr>
</tbody>
</table>

## Miscellaneous Settings

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>cs_version</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>dtrace_support</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>fold_objects_for_list_keys</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>n_val_1_get_requests</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>trust_x_forwarded_for</code></td>
<td></td>
<td><code></code></td>
</tr>
</tbody>
</table>

## Webmachine

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>server_name</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>webmachine_log_handler</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>riak_cs_access_log_handler</code></td>
<td></td>
<td><code></code></td>
</tr>
</tbody>
</table>

## Logging

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>lager_console_backend</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>lager_file_backend</code></td>
<td></td>
<td><code></code></td>
</tr>
</tbody>
</table>

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>crash_log</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>crash_log_count</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>crash_log_date</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>crash_log_msg_size</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>crash_log_size</code></td>
<td></td>
<td><code></code></td>
</tr>
<tr>
<td><code>error_logger_redirect</code></td>
<td></td>
<td><code></code></td>
</tr>
</tbody>
</table>

## SASL

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>sasl_error_lager</code></td>
<td></td>
<td><code></code></td>
</tr>
</tbody>
</table>
