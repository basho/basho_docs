---
title: Riak CS Configuration Reference
project: riakcs
version: 1.5.0+
document: reference
audience: intermediate
keywords: [cs, operator, configuration]
---

This document is intended as a reference listing of all configurable parameters
for Riak CS. For a more narrative-style walkthrough of configuring Riak CS, we
recommend consulting the [[Configuring Riak CS]] tutorial.

The configuration for Riak CS is handled through either the `riak-cs.conf` and
`advanced.config` file pair, which were introduced in Riak CS 2.0.0, or the two
old-style `app.config` and `vm.args` files. All configuration files will be
located in each Riak CS node's `/etc` directory. Please note that you may only
use one of these pairs at a time, as the `app.config`/`vm.args` pair will take
priority over the new-style configuration files.

If you are using it, the `vm.args` file will house settings related to the
[Erlang VM](http://www.erlang.org/) on which both Riak and Riak CS run. These
settings have been folded into the `riak-cs.conf` and `riak.conf` configuration
files in newer systems.

The `app.config` and `advanced.config` files share an identical format, and can
control all of Riak CS's behaviors. The files are divided into the following
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

Most of the settings you will need to manipulate have been ported into the newer
`riak-cs.conf` configuration format, but there may be some advanced settings --
such as setting up customized `lager` streams -- that will need to be configured
in `advanced.config`.

<div class="note"><div class="title">A Note About Time Values</div>
In the `app.config` configuration files, time periods were generally written
as either seconds or milliseconds, with no real indication of which was being
used. With the update to `riak-cs.conf`, all values that describe a period of
time are written as an integer and a character, describing the unit of time and
the number of times that unit should be repeated for the period. For example
`31d` represents 31 days, `6h` represents six hours, `6000ms` represents 6,000
milliseconds.</br>
The full list of valid time units are as follows:</br>
`f` -- Fortnights</br>
`w` -- Weeks</br>
`d` -- Days</br>
`h` -- Hours</br>
`m` -- Minutes</br>
`s` -- Seconds</br>
`ms` -- Milliseconds</br>
</div>

The tables below will show settings for both `riak-cs.conf` and
`advanced.config`/`app.config` where applicable, organized by functionality.

## Connection Information

### `riak-cs.conf`

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>listener</code></td>
<td>The IP address/port for the Riak CS node</td>
<td><code>127.0.0.1:8080</code></td>
</tr>
<tr>
<td><code>riak_host</code></td>
<td>The IP address/port for the Riak CS node's corresponding Riak node (used by
Riak's [[Protocol Buffers|PBC API]] interface)
<td><code>127.0.0.1:8087</code></td>
</tr>
<tr>
<td><code>root_host</code></td>
<td>The root host name accepted by Riak CS. Changing this setting to,
for example, <code>my_cs_host</code> would enable users to make requests
to a URL such as <code>http://bucket.my_cs_host/object/</code> (or to
the corresponding HTTP host).</td>
<td><code>s3.amazonaws.com</code></td>
</tr>
</tbody>
</table>

### `advanced.config`/`app.config`

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
(such as `GET` and `PUT`) as well as for bucket listing requests.

### `riak-cs.conf`

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>pool.request.size</code></td>
<td>Fixed-Size settings for the general request pool for Riak CS. Please note
that we recommend setting Riak's <code>protobuf.backlog</code> setting to be
higher than <code>pool.request.size</code>'s fixed size, i.e. higher than 128.
The default for <code>protobuf.backlog</code> is 128.</td>
<td><code>128</code></td>
</tr>
<tr>
<td><code>pool.request.overflow</code></td>
<td>Overflow-size settings for the general request pool for Riak CS.</td>
<td><code>0</code></td>
</tr>
<tr>
<td><code>pool.list.size</code></td>
<td>Fixed-Size settings for the bucket listing request pool for Riak CS.</td>
<td><code>5</code></td>
</tr>
<tr>
<td><code>pool.list.overflow</code></td>
<td>Overflow-size settings for the bucket listing request pool for Riak CS.</td>
<td><code>0</code></td>
</tr>
</tbody>
</table>

### `advanced.config`/`app.config`

In these files, each pool is specified as a nested tuple of the following form:

```advanced.config
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
default for <code>pb_backlog</code> is 128.</td>
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

### `riak-cs.conf`

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
<td><code>off</code></td>
</tr>
</tbody>
</table>

### `advanced.config`/`app.config`

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>stanchion_host</code></td>
<td>The IP address/port for the Stanchion node in the cluster. Please note that
there should be only one Stanchion node in the cluster.</td>
<td><code>127.0.0.1:8085</code></td>
</tr>
<tr>
<td><code>stanchion_ssl</code></td>
<td>Whether SSL is enabled for connections between the Riak CS node and
Stanchion</td>
<td><code>off</code></td>
</tr>
</tbody>
</table>

## Admin and Authentication Settings

### `riak-cs.conf`

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>admin.listener</code></td>
<td>You have the option to provide a special endpoint for performing system
administration tasks in Riak CS. This setting sets the IP address and port for
that endpoint. If you leave this setting commented out, then administrative
tasks use the IP and port as all other Riak CS traffic.</td>
<td><code>127.0.0.1:8000</code></td>
</tr>
<tr>
<td><code>admin.key</code></td>
<td>The admin key used for administrative access to Riak CS, e.g. usage of the
<code>/riak-cs/stats</code> endpoint. Please note that both
<code>admin.key</code> and <code>admin.secret</code> must match the
corresponding settings in the [[Stanchion]] node's <code>stanchion.conf</code>.
</td>
<td><code>admin-key</code></td>
</tr>
<tr>
<td><code>admin.secret</code></td>
<td>The admin secret used for administrative access to Riak CS. See the
description for <code>admin.key</code> above for more information.</td>
<td><code>admin-secret</code></td>
</tr>
<tr>
<td><code>anonymous_user_creation</code></td>
<td>You will need to set this parameter to <code>on</code> to allow for the
creation of an admin user when setting up a new Riak CS cluster. We recommend,
however, that you enable anonymous user creation only temporarily,
<em>unless</em> your use case specifically dictates that anonymous users should
be able to create accounts.</td>
<td><code>off</code></td>
</tr>
<tr>
<td><code>auth_module</code></td>
<td>The module used by Riak CS for authentication. We do not recommend changing
this setting unless you implement a custom authentication scheme.</td>
<td><code>riak_cs_s3_auth</code></td>
</tr>
<tr>
<td><code>rewrite_module</code></td>
<td>A rewrite module contains a set of rules for translating requests made using
a particular API to requests in the the native [[Riak CS storage API]]. We do
not recommend changing this setting unless you implement a custom module.</td>
<td><code>riak_cs_s3_rewrite</code></td>
</tr>
</tbody>
</table>

### `advanced.config`/`app.config`

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>admin_ip</code></td>
<td>You have the option to provide a special endpoint for performing
system administration tasks in Riak CS. This setting sets the IP
address for that endpoint. If you leave this setting and
<code>admin_port</code> commented, then administrative tasks use the
IP and port as all other Riak CS traffic.</td>
<td><code>8000</code></td>
</tr>
<tr>
<td><code>admin_port</code></td>
<td>The port used for performing system administration tasks. See the
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
<td><code>max_buckets_per_user</code></td>
<td>The number of buckets that can be created by each user. If a user
exceeds the bucket creation limit, they are still able to perform other
actions, including bucket deletion.</td>
<td><code>100</code></td>
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

## Usage Recording

These settings relate to Riak CS's [[access logs|Usage and Billing Data]].

### `riak-cs.conf`

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>stats.access.archive_period</code></td>
<td>How large each access archive object is. This setting should be a multiple
of <code>stats.access.flush_factor</code>. Expressed as a time-value.</td>
<td><code>1h</code></td>
</tr>
<tr>
<td><code>stats.access.archiver.max_backlog</code></td>
<td>The number of access logs that are allowed to accumulate in the archiver's
queue before it begins skipping to catch up. Expressed as an integer number of
logs.</td>
<td><code>2</code></td>
</tr>
<tr>
<td><code>stats.access.flush_factor</code></td>
<td>How often the access log should be flushed, as a factor of
<code>access_archive_period</code>, where <code>1</code> means once per period,
<code>2</code> means twice per period, etc.</td>
<td><code>1</code></td>
</tr>
<tr>
<td><code>access_log_flush_size</code></td>
<td>The additional access log flush trigger. After this many accesses have been
recorded, the log will be flushed, even if the flush interval has not expired.
Expressed as an integer number of accesses.</td>
<td><code>1000000</code></td>
</tr>
<tr>
<td><code>riak_cs.usage_request_limit</code></td>
<td>How many archive periods a user can request in one usage read, applied
independently to access/usage and billing/storage. Expressed as a time-value</td>
<td><code>31d</code></td>
</tr>
<tr>
<td><code>stats.storage.schedule.<em>$time</em></code></td>
<td>When to automatically start storage calculation batches. Expressed as an
<code>HH:MM</code> UTC time. For example, <code>"06:00"</code> would calculate
at 6 am UTC every day. If you would like to schedule multiple batches, changing
<em>$time</em> for each entry. For example <code>stats.storage.schedule.2 =
"18:00"</code> could be the second entry, scheduled for 6:00pm UTC.</td>
<td><code>"06:00"</code></td>
</tr>
<tr>
<td><code>stats.storage.archive_period</code></td>
<td>The size of each storage archive object. Should be chosen such that each
<code>stats.storage.schedule</code>-based calculation falls in a different
period. Expressed as a time-value.</td>
<td><code>1h</code></td>
</tr>
</tbody>
</table>

### `advanced.config`/`app.config`

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>access_archive_period</code></td>
<td>How large each access archive object is. This setting should be a
multiple of <code>access_log_flush_factor</code>. Expressed as an
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
<tr>
<td><code>usage_request_limit</code></td>
<td>How many archive periods a user can request in one usage read,
applied independently to access/usage and billing/storage. Expressed as
an integer number of intervals. The default of 744 thus translates to
one month at one-hour intervals.
of 744</td>
<td><code>744</code></td>
</tr>
<tr>
<td><code>storage_schedule</code></td>
<td>When to automatically start storage calculation batches. Expressed
as a list of <code>HHMM</code> UTC times. For example,
<code>["0600"]</code> would calculate at 6 am UTC every day,
<code>["0600", "1945"]</code> would calculate at 6 am and 7:45 pm UTC
every day, and so on.</td>
<td><code>[]</code></td>
</tr>
<tr>
<td><code>storage_archive_period</code></td>
<td>The size of each storage archive object. Should be chosen such
that each <code>storage_schedule</code>-based calculation falls in a
different period. Expressed as an integer number of seconds. The default
of <code>86400</code> translates to 1 day.</td>
<td><code>86400</code></td>
</tr>
</tbody>
</table>

## Garbage Collection

Settings related to Riak CS's [[garbage collection]] \(GC) process.

### `riak-cs.conf`

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<td><code>gc.interval</code></td>
<td>How often the GC daemon waits between GC batch operations. Expressed as a
time-value.</td>
<td><code>15m</code></td>
</tr>
<tr>
<td><code>gc.max_workers</code></td>
<td>The maximum number of worker processes that may be started by the GC daemon
to use for concurrent reaping of GC-eligible objects.</td>
<td><code>2</code></td>
</tr>
<tr>
<td><code>gc.retry_interval</code></td>
<td>How long a move to the GC to-do list can remain failed before it is
re-attempted. Expressed as a time-value.</td>
<td><code>6h</code></td>
</tr>
<tr>
<td><code>gc.leeway_period</code></td>
<td>How long to retain the block for an object after it has been deleted. This
leeway period is set to give the delete indication enough time to propagate to
all replicas. Expressed as a time-value.</td>
<td><code>24h</code></td>
</tr>
</tbody>
</table>

### `advanced.config`/`app.config`

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>epoch_start</code></td>
<td>The time that the GC daemon uses to begin collecting keys from the
GC eligibility bucket. Records in this bucket use keys based the epoch
time the record is created plus <code>leeway_seconds</code>. The default
is <code>0</code> and should be sufficient for general use. A case for
readjusting this value is if the secondary index query run by the GC
daemon continually times out. Raising the starting value can decrease
the range of the query and make it more likely that the query will
succeed. The value must be specified in Erlang binary format, e.g. set
it to `<<10>>` to specify 10.</td>
<td><code>0</code></td>
</tr>
<tr>
<td><code>gc_batch_size</code></td>
<td>This option is used only when <code>gc_paginated_indexes</code> is
set to <code>true</code>. It represents the size used for paginating the
results of the secondary index query.</td>
<td><code>1000</code></td>
</tr>
<td><code>gc_interval</code></td>
<td>How often the GC daemon waits between GC batch operations. Expressed
as an integer number of seconds.</td>
<td><code>900</code> (15 minutes)</td>
</tr>
<tr>
<td><code>gc_max_workers</code></td>
<td>The maximum number of worker processes that may be started by the GC
daemon to use for concurrent reaping of GC-eligible objects.</td>
<td><code>5</code></td>
</tr>
<td><code>gc_paginated_indexes</code></td>
<td>If you're running Riak nodes that are of a version prior to 1.4.0,
set this to <code>false</code>. Otherwise, you will not need to adjust
this setting.</td>
<td><code>true</code></td>
</tr>
<tr>
<td><code>gc_retry_interval</code></td>
<td>How long a move to the GC to-do list can remain failed before it is
re-attempted. Expressed as an integer number of seconds.</td>
<td><code>21600</code> (6 hours)</td>
</tr>
<tr>
<td><code>leeway_seconds</code></td> <td>The number of seconds to retain
the block for an object after it has been deleted. This leeway time is
set to give the delete indication time to propagate to all replicas.
Expressed as an integer number of seconds.</td>
<td><code>86400</code> (24 hours)</td>
</tr>
<tr>
<td><code>max_scheduled_delete_manifests</code></td>
<td>The maximum number of manifests (representative of object versions)
that can be in the <code>scheduled_delete</code> state for a given key.
A value of <code>unlimited</code> means that there is no maximum and
that pruning will not be based on count. An example of where this option
is useful is a use case involving a lot of churn on a fixed set of keys
in a time frame that is relatively short compared to the
<code>leeway_seconds</code> value. This can result in the manifest
objects reaching a size that can negatively impact system performance.
</td>
<td><code>unlimited</code></td>
</tr>
</tbody>
</table>

## Concurrency and Buffering

### `advanced.config`/`app.config` Only

There are two parameters related to concurrency and buffering that you should
consider adding to your Riak CS settings if you are having issues with PUT
requests. Raising the value of both of these settings may provide higher single-
client throughput.

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>put_buffer_factor</code></td>
<td>The number of blocks that will be buffered in-memory in Riak CS
before it begins to slow down reading from the HTTP client.</td>
<td><code>1</code></td>
</tr>
<tr>
<td><code>put_concurrency</code></td>
<td>The number of threads inside of Riak CS that are used to write
blocks to Riak.</td>
<td><code>1</code></td>
</tr>
</tbody>
</table>

## Miscellaneous Settings

### `riak-cs.conf`

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>cs_version</code></td>
<td>The Riak CS version number. This number is used to selectively enable new
features for the current version to better support [[rolling upgrades]]. New
installs shouldn't need to modify this. If you're performing a rolling upgrade,
keep the original value (if not defined, Riak CS uses <code>0</code>) of the old
<code>app.config</code> until all nodes have been upgraded. At that point, set
it to the new value.</td>
<td><code>10300</code></td>
</tr>
<tr>
<td><code>dtrace</code></td>
<td>If your Erlang VM supports <a
href="http://erlang.org/doc/apps/runtime_tools/DTRACE.html">DTrace</a>
or <a
href="http://www.erlang.org/doc/apps/runtime_tools/SYSTEMTAP.html">SystemTap</a>,
set this parameter to <code>on</code>.</td>
<td><code>off</code></td>
</tr>
<tr>
<td><code>trust_x_forwarded_for</code></td>
<td>If your load balancer adds an <code>X-Forwarded-For</code> header and is
reliable, i.e. the load balancer is able to guarantee that it is not added by a
malicious user, set this option to <code>on</code>. Otherwise, Riak CS takes the
source IP address as an input (which is the default).</td>
<td><code>off</code></td>
</tr>
</tbody>
</table>

### `advanced.config`/`app.config`

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>cs_version</code></td>
<td>The Riak CS version number. This number is used to selectively
enable new features for the current version to better support [[rolling
upgrades]]. New installs shouldn't need to modify this. If you're
performing a rolling upgrade, keep the original value (if not defined,
Riak CS uses <code>0</code>) of the old <code>app.config</code> until
all nodes have been upgraded. At that point, set to the new value.</td>
<td><code></code></td>
</tr>
<tr>
<td><code>dtrace_support</code></td>
<td>If your Erlang VM supports <a
href="http://erlang.org/doc/apps/runtime_tools/DTRACE.html">DTrace</a>
or <a
href="http://www.erlang.org/doc/apps/runtime_tools/SYSTEMTAP.html">SystemTap</a>,
set this parameter to <code>true</code>.</td>
<td><code>false</code></td>
</tr>
<tr>
<td><code>fold_objects_for_list_keys</code></td>
<td>If your Riak CS cluster is running Riak nodes prior to version
1.4.0, set this parameter to <code>false</code>. Otherwise, you will not
need to modify it.<strong>This setting has been deprecated and <em>will be
removed<em> in the next major version.</strong></td>
<td><code>true</code></td>
</tr>
<tr>
<td><code>n_val_1_get_requests</code></td>
<td>If set to <code>true</code>, Riak CS will use a special request
option when retrieving the blocks of an object. This special option
instructs Riak to only send a request for the object block to a single
eligible virtual node (vnode) instead of to all eligible vnodes. This
differs from the standard <code>r</code> request option provided by Riak
in that <code>r</code> affects how many vnode responses to wait for
before returning and has no effect on how many vnodes are actually
contacted. Enabling this option (the default) has the effect of
greatly reducing the intra-cluster bandwidth used by Riak when
retrieving objects with Riak CS. This option is harmless when used with
a version of Riak prior to 1.4.0, but the option to disable is provided
as a safety measure. <strong>This setting has been deprecated and
<em>will be removed<em> in the next major version.</strong></td>
<td><code>true</code></td>
</tr>
<tr>
<td><code>trust_x_forwarded_for</code></td>
<td>If your load balancer adds an <code>X-Forwarded-For</code>
header and is reliable, i.e. the load balancer is able to guarantee that
it is not added by a malicious user, set this option to
<code>true</code>. Otherwise, Riak CS takes the source IP address as an
input (which is the default).</td>
<td><code>false</code></td>
</tr>
</tbody>
</table>

## Webmachine

### `advanced.config`/`app.config` Only

Settings specific to [Webmachine](https://github.com/basho/webmachine), the web
server that handles all HTTP and HTTPS connections to Riak CS. The
`riak_cs_access_log_handler` and `webmachine_log_handler` settings are part of a
`log_handlers` sub-grouping:

```appconfig
{webmachine, [
              %% Other configs
              {log_handlers, [
                              {webmachine_access_log_handler, ...},
                              {riak_cs_access_log_handler, ...},
                              ]},
              %% Other configs
             ]}
```

<div class="note"><div class="title">Upgrading to 2.0.0</div>
Due to a WebMachine change for the 2.0.0 update, if `log_handlers` are defined
in `app.config` or `advanced.config`, the log handler's name should be changed
from `webmachine_log_handler` to `webmachine_access_log_handler`. This does not
have to be changed if log_handlers is not defined in `app.config` or
`advanced.config`.
</div>

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
<td>If this setting is commented out or removed, access to Webmachine
log handling will be disabled.</td>
<td><code>["./log"]</code></td>
</tr>
<tr>
<td><code>riak_cs_access_log_handler</code></td>
<td>We do not recommend changing or removing this setting.</td>
<td><code>[]</code></td>
</tr>
</tbody>
</table>

## Logging

### `advanced.config`/`app.config` Only

These settings relate to [lager](https://github.com/basho/lager), the Erlang
logging framework used by Riak CS. They are included in the `lager` settings in
`app.config`.

The `lager_console_backend` and `lager_file_backend` settings are part of a
`handlers` sub-group:

```appconfig
{lager, [
         %% Other configs
         {handlers, [
                     {lager_console_backend, ...},
                     {lager_file_backend, ...}
                    ]},
         %% Other configs
        ]}
```

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>lager_console_backend</code></td>
<td>See the <a
href="https://github.com/basho/lager/blob/master/README.md#configuration">lager
documentation</a> for more details.</td>
<td><code></code></td>
</tr>
<tr>
<td><code>lager_file_backend</code></td>
<td>See the <a
href="https://github.com/basho/lager/blob/master/README.md#configuration">lager
documentation</a> for more details.</td>
<td><code></code></td>
</tr>
</tbody>
</table>

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>crash_log</code></td>
<td>Whether to write to a crash log and where. If commented out,
omitted, or undefined, no crash logging will take place.</td>
<td><code>./log/crash.log</code></td>
</tr>
<tr>
<td><code>crash_log_count</code></td>
<td>The number of crash logs to keep. Setting this parameter to
<code>0</code> (the default) means that only the current log will be
kept.</td>
<td><code>0</code></td>
</tr>
<tr>
<td><code>crash_log_date</code></td>
<td>When to rotate the crash log. The default is no time rotation. For
documentation on the syntax of this parameter, see <a
href="https://github.com/basho/lager/blob/master/README.md">here</a>.</td>
<td><code>$D0</code></td>
</tr>
<tr>
<td><code>crash_log_msg_size</code></td>
<td>The maximum size of events in the crash log, expressed as a number
of bytes.</td>
<td><code>65536</code></td>
</tr>
<tr>
<td><code>crash_log_size</code></td>
<td>The maximum size of the crash log, in bytes, before it is rotated.
Setting this parameter to <code>0</code> disables rotation.</td>
<td><code>10485760</code></td>
</tr>
<tr>
<td><code>error_logger_redirect</code></td>
<td>Whether to redirect <code>error_logger</code> messages into
lager.</td>
<td><code>true</code></td>
</tr>
</tbody>
</table>

## SASL

### `advanced.config`/`app.config` Only

<table class="riak-conf">
<thead><tr><th>Config</th><th>Description</th><th>Default</th></tr></thead>
<tbody>
<tr>
<td><code>sasl_error_lager</code></td>
<td>Whether to enable <a
href="http://www.erlang.org/doc/man/sasl_app.html">, Erlang's built-in
error logger.</td>
<td><code>false</code></td>
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

[config_your_code]: http://docs.basho.com/riak/1.4.12/ops/advanced/configs/configuration-files/#Configuring-Your-code-vm-args-code-
