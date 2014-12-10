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
<td>The root host name accepted by Riak CS. Changing this setting to,
for example, <code>my_cs_host</code> would enable users to make requests
to a URL such as <code>http://bucket.my_cs_host/object/</code> (or to
the corresponding HTTP host).</td>
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
<td>Settings for the bucket listing request pool for Riak CS</td>
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
<td>The IP address for the Stanchion node in the cluster. Please note
that there should be only one Stanchion node in the cluster.</td>
<td><code>127.0.0.1</code></td>
</tr>
<tr>
<td><code>stanchion_port</code></td>
<td>The TCP port used by the Stancion node in the cluster</td>
<td><code>8085</code></td>
</tr>
<tr>
<td><code>stanchion_ssl</code></td>
<td>Whether SSL is enabled for connections between the Riak CS node and
Stanchion</td>
<td><code>false</code></td>
</tr>
</tbody>
</table>

## Admin and Authentication Settings

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>admin_ip</code></td>
<td>You have the option to provide a special endpoint for performing
systems administration tasks in Riak CS. This setting sets the IP
address for that endpoint. If you leave this setting and
<code>admin_port</code> commented, then administrative tasks use the
IP and port as all other Riak CS traffic.</td>
<td><code>8000</code></td>
</tr>
<tr>
<td><code>admin_port</code></td>
<td>The port used for performing systems administration tasks. See the
description for <code>admin_ip</code> above for more information.</td>
<td><code>8000</code></td>
</tr>
<tr>
<td><code>admin_key</code></td>
<td>The admin key used for administrative access to Riak CS, e.g. usage
of the <code>/riak-cs/stats</code> endpoint. Please note that both
<code>admin_key</code> and <code>admin_secret</code> must match the
corresponding settings in the [[Stanchion]] node's
<code>app.config</code>.</td>
<td><code></code></td>
</tr>
<tr>
<td><code>admin_secret</code></td>
<td>The admin secret used for administrative access to Riak CS. See the
description for <code>admin_key</code> above for more information.</td>
<td><code></code></td>
</tr>
<tr>
<td><code>anonymous_user_creation</code></td>
<td>You will need to set this parameter to <code>true</code> to allow
for the creation of an admin user when setting up a new Riak CS cluster.
We recommend, however, that you enable anonymous user creation only
temporarily, <em>unless</em> your use case specifically dictates that
anonymous users should be able to create accounts.</td>
<td><code>false</code></td>
</tr>
<tr>
<td><code>auth_module</code></td>
<td>The module used by Riak CS for authentication. We do not recommend
changing this setting unless you implement a custom authentication
scheme.</td>
<td><code>riak_cs_s3_auth</code></td>
</tr>
<tr>
<td><code>rewrite_module</code></td>
<td>A rewrite module contains a set of rules for translating requests
made using a particular API to requests in the the native [[Riak CS
storage API]]. We do not recommend changing this setting unless you
implement a custom module.</td>
<td><code>riak_cs_s3_rewrite</code></td>
</tr>
</tbody>
</table>

## Access Log Settings

These settings relate to Riak CS's [[access logs|Usage and Billing
Data]].

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>access_archive_period</code></td>
<td>How large each access archive object is. This setting should be a
multiple of <code>access_log_flush_interval</code>. Expressed as an
integer number of seconds (e.g. 3600 translates to 1 hour).</td>
<td><code>3600</code></td>
</tr>
<tr>
<td><code>access_archive_max_backlog</code></td>
<td>The number of access logs that are allowed to accumulate in the
archiver's queue before it begins skipping to catch up. Expressed as an
integer number of logs.</td>
<td><code>2</code></td>
</tr>
<tr>
<td><code>access_log_flush_factor</code></td>
<td>How often the access log should be flushed, as a factor of
<code>access_archive_period</code>, where <code>1</code> means once per
period, <code>2</code> means twice per period, etc.</td>
<td><code>1</code></td>
</tr>
<tr>
<td><code>access_log_flush_size</code></td>
<td>The additional access log flush trigger. After this many accesses
have been recorded, the log will be flushed, even if the flush interval
has not expired. Expressed as an integer number of accesses.</td>
<td><code>1000000</code></td>
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

<!--
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
-->
