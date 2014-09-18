---
title: Configuring Riak CS
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, configuration]
---

For Riak CS to operate properly it must know how to connect to Riak.
A Riak CS node typically runs on the same server as its corresponding
Riak node, which means that changes will only be necessary if Riak is
configured using non-default settings.

The settings reside in the Riak CS `app.config` file, which is typically
located in the `/etc` directory on each node. Configurable parameters
related to Riak CS specifically can be found in the `riak_cs` section of
that file. That section looks something like this:

```appconfig
{riak_cs, [
    {parameter1, value},
    {parameter2, value},
    %% and so on...
]},
```

## Host and Port

To connect Riak CS to Riak, make sure that the following parameters are
set to the host and port used by Riak:

* `riak_ip` --- Replace `127.0.0.1` with the IP address of the Riak node
  you want Riak CS to connect to
* `riak_pb_port` --- Replace `8087` with the port number set in the
  variable `pb_port` in the Riak `app.config` file

<div class="note">
<div class="title">Note on IP addresses</div>
The IP address you enter here must match the IP address specified for
the Protocol Buffers interface in the Riak <code>app.config</code> file
unless Riak CS is running on a completely different network and address
translation is required.
</div>

After making any changes to the `app.config` file in Riak CS, restart
the node if it is already running.

## Specifying the Stanchion Node

If you have a single node, you don't have to change the Stanchion
settings because Stanchion runs on the local host. If your Riak CS
system has multiple nodes, however, you must specify the IP address and
port for the Stanchion node and whether or not SSL is enabled.

The Stanchion settings reside in the Riak CS `app.config` file, which is
located in the `/etc` directory of each Riak CS node. The settings
appear in the `riak_cs` config section of the file.

To set the host and port for Stanchion, do the following:

* For the `stanchion_ip` setting, replace `127.0.0.1` with the address
  of the Stanchion node
* For the `stanchion_port` setting, replace `8085` with the port for the
  node

## Enabling SSL

SSL is turned off by default in Stanchion, i.e. the `stanchion_ssl`
variable is set to `false`. If Stanchion is configured to use SSL,
change this variable to `true`. The following example configuration
would set the host to `localhost`, the port to `8085` (the default), and
set up Stanchion to use SSL:

```appconfig
{riak_cs, [
    %% Other configs

    {stanchion_ip, "127.0.0.1"},
    {stanchion_host, 8085},
    {stanchion_ssl, true},

    %% Other configs
]}
```

## Specifying the Node Name

You can also set a more useful name for the Riak CS node, which is
helpful to identify the node from which requests originate during
troubleshooting. This setting resides in the Riak CS `vm.args`
configuration file, which is also located in the `/etc` directory.
This would set the name of the Riak CS node to `riak_cs@127.0.0.1`:

```vmargs
-name riak_cs@127.0.0.1
```

Change `127.0.0.1` to the IP address or hostname for the server on which
Riak CS is running.

## Specifying the Admin User

The admin user is authorized to perform actions such as creating users
or obtaining billing statistics. An admin user account is no different
from any other user account. **You must create an admin user to use Riak
CS**.

<div class="note">
<div class="title">Note on anonymous user creation</div>
Before creating an admin user, you must first set
<code>{anonymous_user_creation, true}</code> in the Riak CS
<code>app.config</code>. You may disable this again once the admin user
has been created.
</div>

To create an account for the admin user, use an HTTP `POST` request with
the username you want to use for the admin account. The following is an
example:

```curl
curl -H 'Content-Type: application/json' \
  -XPOST http://localhost:8080/riak-cs/user \
  --data '{"email":"foobar@example.com", "name":"admin user"}'
```

The JSON response will look something like this:

```json
{
  "Email": "foobar@example.com",
  "DisplayName": "adminuser",
  "KeyId": "324ABC0713CD0B420EFC086821BFAE7ED81442C",
  "KeySecret": "5BE84D7EEA1AEEAACF070A1982DDA74DA0AA5DA7",
  "Name": "admin user",
  "Id":"8d6f05190095117120d4449484f5d87691aa03801cc4914411ab432e6ee0fd6b",
  "Buckets": []
}
```

You can optionally send and receive XML if you set the `Content-Type` to
`application/xml`, as in this example:

Once the admin user exists, you must specify the credentials of the
admin user on each node in the Riak CS system. The admin user credential
settings reside in the Riak CS `app.config` file, which is located in
the `etc/riak-cs` directory. The settings appear in the Riak CS config
section of the file. Paste the `key_id` string between the quotes for
the `admin_key`. Paste the `key_secret` string into the `admin_secret`
variable, as shown here:

```appconfig
%% Admin user credentials
{admin_key, "LXAAII1MVLI93IN2ZMDD"},
{admin_secret, "5BE84D7EEA1AEEAACF070A1982DDA74DA0AA5DA7"},
```

Once the admin user exists, you must specify the credentials of the
admin user in the `app.config` file. Those will be the same credentials
that you received as a JSON object when you ran the `POST` request to
create the user.

## Bucket Restrictions

If you wish, you can limit the number of buckets created per user. The
default maximum is 100. Please note that if a user exceeds the bucket
creation limit, they are still able to perform other actions, including
bucket deletion. You can change the default limit using the
`max_buckets_per_user` parameter in each node's `app.config` file. The
example configuration below would set the maximum to 1000:

```appconfig
{riak_cs, [
    %% Other configs

    {max_buckets_per_user, 1000},

    %% Other configs
]}
```

If you want to avoid setting a limit on per-user bucket creation, you
can set `max_buckets_per_user` to `unlimited`.

## Connection Pools

Riak CS uses two distinct connection pools for communication with
Riak: a **primary** and a **secondary** pool.

The primary connection pool is used to service the majority of API
requests related to the upload or retrieval of objects. It is identified
in the configuration file as `request_pool`. The default size of this
pool is 128.

The secondary connection pool is used strictly for requests to list the
contents of buckets. The separate connnection pool is maintained in
order to improve performance. This secondary connection pool is
identified in the configuration file as `bucket_list_pool`. The default
size of this pool is 5.

The following shows the `connection_pools` default configuration entry
that can be found in the `app.config` file:

```appconfig
{riak_cs, [
    %% Other configs

    {connection_pools,
    [
     {request_pool, {128, 0} },
     {bucket_list_pool, {5, 0} }
    ]},

    %% Other configs
]}
```

The value for each pool is represented as a pair with the first element
representing the normal size of the pool. This is representative of the
number of concurrent requests of a particular type that a Riak CS node
may service. The second element represents the number of allowed
overflow pool requests that are allowed. It is not recommended that you
use any value other than 0 for the overflow amount unless careful
analysis and testing has shown it to be beneficial for a particular use
case.

### Tuning

We strongly recommend you that you [[increase the value of the
`pb_backlog` setting|Configuring Riak for
CS#Setting-Up-Riak-to-Use-Protocol-Buffers]] in Riak. When a Riak CS
node is started, each connection pool begins to establish connections to
Riak.  This can result in a [[thundering herd
problem|http://en.wikipedia.org/wiki/Thundering_herd_problem]] in which
connections in the pool believe they are connected to Riak, but in
reality some of the connections have been reset. Due to TCP `RST` packet
rate limiting (controlled by `net.inet.icmp.icmplim`) some of the
connections may not receive notification until they are used to service
a user's request.  This manifests as an `{error, disconnected}` message
in the Riak CS logs and an error to returned to the user.

## Specifying the Stanchion Node

If you have a single node, you don't have to change the Stanchion
settings because Stanchion runs on the local host. If your Riak CS
system has multiple nodes, you must set the IP address and port for the
Stanchion node and whether or not SSL is enabled.

The Stanchion settings reside in the Riak CS `app.config` file, which is
located in the `/etc/Riak-CS` directory. The settings appear in the Riak
CS config section of the file.

* `stanchion_ip` --- Replace `127.0.0.1` with the IP address of the
  Stanchion node

If you configured Stanchion to use a different port, you must change the
following port setting:

* `stanchion_port` --- Replace `8085` with the port number set in the
  variable `stanchion_port` in the Stanchion `app.config` file

The `stanchion_ssl` variable is set to `false` by default. If you want
to use SSL, change this variable to `true`.

## Specifying the Node IP Address

You can also set the IP address for the Riak CS node, which is helpful
if you must debug code or identify the node from which requests
originate. The Riak CS IP address setting resides in the Riak CS
`vm.args` configuration file, which is located in the `/etc/riak-cs`
directory.

Initially, the line that specifies the Riak CS node IP address is set to
`localhost`, as follows:

```config
## Name of the riak node
-name riak_cs@127.0.0.1
```

## Enabling SSL in Riak CS

```appconfig
%%{ssl, [
%%    {certfile, "./etc/cert.pem"},
%%    {keyfile, "./etc/key.pem"}
%%   ]},
```

Then replace the text in quotes with the path and filename for your SSL
encryption files. By default, there's a `cert.pem` and a `key.pem` in
each node's `/etc` directory. You're free to use those or to supply your
own.

## Proxy vs. Direct Configuration

Riak CS can interact with S3 clients in one of two ways:

* A **proxy** configuration enables an S3 client to communicate with
  Riak CS as if it were Amazon S3 itself, i.e. using typical Amazon URLs.
* A **direct** configuration requires that an S3 client connecting to
  Riak CS be configured for an "S3-compatible service," i.e. with a Riak
  CS endpoint that is not masquerading as Amazon S3. Examples of such
  services include [Transmit](http://panic.com/transmit/),
  [s3cmd](http://s3tools.org/s3cmd), and
  [DragonDisk](http://www.dragondisk.com/).

### Proxy

To establish a proxy configuration, configure your client's proxy
settings to point to Riak CS cluster's address. Then configure your
client with Riak CS credentials.

When Riak CS receives the request to be proxied, it services the request
itself and responds back to the client as if the request went to S3.

On the server side, the `cs_root_host` in the `riak_cs` section of the
`app.config` configuration file must be set to `s3.amazonaws.com`
because all of the bucket URLs request by the client will be destined
for `s3.amazonaws.com`. This is the default.

**Note**: One issue with proxy configurations is that many GUI clients
only allow for one proxy to be configured for all connections. For
customers trying to connect to both S3 and Riak CS, this can prove
problematic.

### Direct

The establish a direct configuration, the `cs_root_host` in the
`riak_cs` section of `app.config` must be set to the FQDN of your Riak
CS endpoint, as all of the bucket URLs will be destined for the FQDN
endpoint.

You will also need wildcard DNS entries for any child of the endpoint to
resolve to the endpoint itself. Here's an example:

```config
data.riakcs.net
*.data.riakcs.net
```

## Garbage Collection Settings

The following options are available to make adjustments to the Riak CS
garbage collection system. More details about garbage collection in
Riak CS are available in [[Garbage Collection]].

* `leeway_seconds` --- The number of seconds that must
  elapse before an object version that has been explicitly deleted or
  overwritten is eligible for garbage collection. The default value is
  86400 (24 hours).
* `gc_interval` --- The interval, in seconds, that the garbage
  collection daemon runs at to search for and reap eligible object
  versions. The default value is 900 seconds (15 minutes).
* `gc_retry_interval` --- The number of seconds that must elapse
  before another attempt is made to write a record for an object
  manifest in the `pending_delete` state to the garbage collection
  eligibility bucket. In general, this condition should be rare, but
  could happen if an error condition caused the original record in the
  garbage collection eligibility bucket to be removed prior to the
  reaping process completing. The default value is 21600 seconds (6 hours).
* `epoch_start` --- The time that the garbage collection daemon uses
  to begin collecting keys from the garbage collection eligibility
  bucket. Records in this bucket use keys based on the epoch time the
  record is created + `leeway_seconds`. The default is 0 and should be
  sufficient for general use. A case for adjusting this value is if
  the secondary index query run by the garbage collection daemon
  continually times out. Raising the starting value can decrease the
  range of the query and make it more likely the query will
  succeed. The value must be specified in Erlang binary format. *e.g.*
  To set it to 10, specify `<<"10">>`.
{{#1.4.2+}}
* `initial_gc_delay` --- The number of seconds to wait in addition to
  the `gc_interval` value before the first execution of the garbage
  collection daemon when the Riak CS node is started. This is option
  could be used in order stagger the execution of garbage collection
  on multiple nodes since there is currently no coordination among
  Riak CS nodes. to The default value is 0.
{{/1.4.2+}}
{{#1.4.3+}}
* `max_scheduled_delete_manifests` --- The maximum number of
  manifests (representative of object versions) that can be in the
  `scheduled_delete` state for a given key. A value of `unlimited`
  means there is no maximum, and pruning will not happen based on
  count. An example of where this option is useful is a use case
  involving a lot of churn on a fixed set of keys in a time frame that
  is relatively short compared to the `leeway_seconds` value. This can
  result in the manifest objects reaching a size that can negatively
  impact system performance. The default value is `unlimited`.
* `gc_paginated_indexes` ---  This option indicates whether the garbage
  collection daemon should use paginated secondary index queries when
  searching the garbage collection bucket for eligible records to reap.
  Setting this option to `true` is generally more efficient and is
  recommended for cases where the underlying Riak nodes are of version
  1.4.0 or above. The default value is {{#1.5.0-}}`false`{{/1.5.0-}}
  {{#1.5.0+}}`true`{{/1.5.0+}}.
* `gc_batch_size` --- This option is only used when
  `gc_paginated_indexes` is set to `true`. It represents the size used
  for paging the results of the secondary index query. The default
  value is 1000.
{{/1.4.3+}}
{{#1.5.0+}}
* `gc_max_workers` --- The maximum number of worker processes that may
  be started by the garbage collection daemon to use for concurrent
  reaping of garbage-collection-eligible objects. The default value is
  5.
{{/1.5.0+}}

## Other Riak CS Settings

There are two configuration options designed to provide improved
performance for Riak CS when using Riak 1.4.0 or later. These options
take advantage of additions to Riak that are not present prior to
version 1.4.0.

* `fold_objects_for_list_keys` --- Setting this option to `true` enables
  Riak CS to use a more efficient method of retrieving Riak CS bucket
  contents from Riak. Using this option provides improved performance
  and stability, especially for buckets that contain millions of objects
  or more. This option should not be enabled unless Riak 1.4.0 or
  greater is being used. The default value for this option is `false`.

* `n_val_1_get_requests` --- This option causes Riak CS to use a special
  request option when retrieving the blocks of an object. This option
  instructs Riak to only send a request for the object block to a single
  eligible virtual node (vnode) instead of all eligible vnodes. This
  differs from the standard `r` request option that Riak provides in
  that `r` affects how many vnode responses to wait for before returning
  and has no effect on how many vnodes are actually contacted. Enabling
  this option has the effect of greatly reducing the intra-cluster
  bandwidth used by Riak when retrieving objects with Riak CS. This
  option is harmless when used with a version of Riak prior to 1.4.0,
  but the option to disable it is provided as a safety measure. The
  default value for this option is `true`.

The `app.config` file includes other settings, such as whether to create
log files and where to store them. These settings have default values
that work in most cases.

## Full Configuration Listing

<table>
<thead>
<tr>
<th>Config</th>
<th>Subsection</th>
<th>Description</th>
<th>Default</th>
</tr>
</thead>

<tbody>

<tr>
<td><code>cs_ip</code></td>
<td></td>
<td>The IP address of the Riak CS node.</td>
<td><code>"127.0.0.1"</code></td>
</tr>

<tr>
<td><code>cs_port</code></td>
<td></td>
<td>The port on which the Riak CS node listens.</td>
<td><code>8080</code></td>
</tr>

<tr>
<td><code>riak_ip</code></td>
<td></td>
<td>The IP address for the Riak node accessed by this Riak CS node.</td>
<td><code>"127.0.0.1"</code></td>
</tr>

<tr>
<td><code>riak_pb_port</code></td>
<td></td>
<td>The [Protocol Buffers](http://code.google.com/p/protobuf/) port for
the Riak node accessed by this Riak CS node.</td>
<td><code>8087</code></td>
</tr>

<tr>
<td><code>stanchion_ip</code></td>
<td></td>
<td>The IP address for the Stanchion node associated with this Riak CS
node.</td>
<td><code>"127.0.0.1"</code></td>
</tr>

<tr>
<td><code>stanchion_port</code></td>
<td></td>
<td>The port on which the Stanchion node associated with this Riak CS
node listens.</td>
<td></td>
</tr>

<tr>
<td><code>stanchion_ssl</code></td>
<td></td>
<td>Whether SSL is enabled on the Stanchion node associated with this
Riak CS node.</td>
<td><code>false</code></td>
</tr>

<tr>
<td><code>anonymous_user_creation</code></td>
<td></td>
<td>Whether users can be created by a currently anonymous user. We
recommend enabling anonymous user creation only if your use case
specifically demands allowing anonymous users to create accounts. You may
also need to temporarily enable anonymous user creation when you are
first setting up your Riak CS installation, so that you can create an
admin user.</td>
<td><code>false</code></td>
</tr>

<tr>
<td><code>admin_key</code></td>
<td></td>
<td>The secret key that admin users must use to authenticate themselves,
particularly for access to things like the <code>/riak-cs/stats</code>
endpoint. Please note that the admin credentials set on Riak CS nodes
must match the admin key set in Stanchion's <code>app.config</code>
file.</td>
<td><code>"admin-key"</code></td>
</tr>

<tr>
<td><code>admin_secret</code></td>
<td>The secret that admin users must use to authenticate themselves. See
the instructions for the <code>admin_key</code> setting above for more
information.</td>
<td><code>"admin-secret"</code></td>
<td></td>
</tr>

<tr>
<td><code>admin_ip</code></td>
<td></td>
<td>The IP address to listen on for admin-related tasks. This setting is
commented out by default, which means that this value is the same as
<code>cs_ip</code> by default. Only uncomment this setting if you wish
to use a different IP for admin tasks.</td>
<td><code>"127.0.0.1"</code> (commented)</td>
</tr>

<tr>
<td><code>admin_port</code></td>
<td></td>
<td>The port to listen on for admin-related tasks. This setting is
commented out by default, which means that this value is the same as
<code>cs_port</code> by default. Only uncomment this setting if you wish
to use a different port for admin tasks.</td>
<td><code>8000</code> (commented)</td>
</tr>

<tr>
<td><code>certfile</code></td>
<td><code>ssl</code></td>
<td>The location of the SSL cert used by this Riak node. This setting is
commented by default. Only uncomment if you need to use SSL. Otherwise,
Riak CS will be available via HTTP only.</td>
<td><code>./etc/cert.pem</code></td>
</tr>

<tr>
<td><code>keyfile</code></td>
<td><code>ssl</code></td>
<td>The location of the SSL keyfile used by this Riak node. This setting
is commented by default. Only uncomment if you need to use SSL.
Otherwise, Riak CS will be available via HTTP only.</td>
<td><code>./etc/key.pem</code> (commented)</td>
</tr>

<tr>
<td><code>cs_root_host</code></td>
<td></td>
<td>The root host name that Riak CS accepts. A CS bucket would be
accessible via a URL like
<code>http://bucket.s3.example.com/object/name</code> if this parameter
were set to <code>s3.example.com</code>.</td>
<td><code>s3.amazonaws.com</code></td>
</tr>

<tr>
<td><code>request_pool</code></td>
<td><code>connection_pools</code></td>
<td>Sets the fixed and overflow sizes of this Riak CS node's request
pool, expressed as a <code>{FixedSize, OverflowSize}</code> tuple.</td>
<td><code>{128, 0}</code></td>
</tr>

<tr>
<td><code>bucket_list_pool</code></td>
<td><code>connection_pools</code></td>
<td>Sets the fixed and overflow sizes of this Riak CS node's connection
pool, expressed as a <code>{FixedSize, OverflowSize}</code> tuple.</td>
<td><code>{5, 0}</code></td>
</tr>

<tr>
<td><code>rewrite_module</code></td>
<td></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code>auth_module</code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code>fold_objects_for_list_keys</code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code>n_val_1_get_requests</code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code>cs_version</code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code>access_log_flush_factor</code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code>access_log_flush_size</code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code>access_archive_period</code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code>access_archiver_max_backlog</code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code>storage_schedule</code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code>storage_archive_period</code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code>usage_request_limit</code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code>leeway_seconds</code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code>gc_interval</code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code>gc_retry_interval</code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code>gc_paginated_indexes</code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code>trust_x_forwarded_for</code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code>dtrace_support</code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code>webmachine_log_handler</code></td>
<td><code>log_handlers</code></td>
<td></td>
</tr>

<tr>
<td><code>riak_cs_access_log_handler</code></td>
<td><code>log_handlers</code></td>
<td></td>
</tr>

</tbody>
</table>

## Webmachine Settings

<table>
<thead>

</thead>

<tbody>

<tr>
<td><code>lager_console_backend</code></td>
<td><code>handlers</code></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td><code></code></td>
<td></td>
<td></td>
</tr>

<tr>
<td><code></code></td>
<td><code></code></td>
<td></td>
<td></td>
</tr>

</tbody>
</table>
