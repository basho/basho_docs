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

Riak CS's settings typically reside in a CS node's `riak-cs.conf` and
`advanced.config` files, both of which are typically located in the `/etc/riak-
cs` directory. The newer `riak-cs.conf` file is a simple list of `configuration
= option` pairs, but there are some configuration options that can only be
changed through the `advanced.config` file. That file looks something like:

```advancedconfig
{riak_cs, [
    {parameter1, value},
    {parameter2, value},
    %% and so on...
]},
```

If you're updating from a version of Riak CS prior to 2.0.0 -- when the
`riak-cs.conf` file was introduced -- you can still use the old-style
`app.config` configuration file in place of the `riak-cs.conf`/`advanced.config`
pair. The `app.config` file has identical syntax to the `advanced.config` file,
so any examples that use the `advanced.config` syntax can be directly translated
to the `app.config` file.

Please note that the older `app.config` file supersedes the newer configuration
files. If an `app.config` file is present, neither the `riak-cs.config` nor the
`advanced.config` will be used.

The sections below walk you through some of the main configuration categories
that you will likely encounter while operating Riak CS. For a comprehensive
listing of available parameters and a full list of `app.config` parameter, see
the [[Full Configuration Reference|Riak CS Configuration Reference]].

## Host and Port

To connect Riak CS to Riak, make sure that the following parameter is set to the
host and port used by Riak:

* `riak_host` --- Replace `127.0.0.1:8087` with the IP address and port number
  of the Riak node you want Riak CS to connect to.

You will also need to set the host listener for Riak CS:

* `listener` --- Replace `127.0.0.1:8080` with the IP address and port number
   of the Riak CS node if you are running CS non-locally. Make sure that the
   port number does not conflict with the `riak_host` port number of the Riak
   node and the Riak CS node that are running on the same machine.

<div class="note">
<div class="title">Note on IP addresses</div>
The IP address you enter here must match the IP address specified for
the Protocol Buffers interface in the Riak <code>riak.conf</code> file
unless Riak CS is running on a completely different network, in which
case address translation is required.
</div>

After making any changes to the `riak-cs.conf` file in Riak CS,
[[restart|Riak CS Command-line Tools#riak-cs]] the node if it is already
running.

## Specifying the Stanchion Node

If you're running a single Riak CS node, you don't have to change the
[[Stanchion|Configuring Stanchion]] settings because Stanchion runs on
the local host. If your Riak CS system has multiple nodes, however, you
must specify the IP address and port for the Stanchion node and whether
or not SSL is enabled.

The Stanchion settings reside in the Riak CS `riak-cs.conf` file, which is
located in the `/etc/riak-cs` directory of each Riak CS node.

To set the host and port for Stanchion, make sure the following parameter is set
to the host and port used by Stanchion:

* `stanchion_host` --- replace `127.0.0.1:8085` with the IP address and port
  number of the Stanchion node

## Enabling SSL

SSL is disabled by default in Stanchion, i.e. the `stanchion_ssl` variable is
set to `off`. If Stanchion is configured to use SSL, change this variable to
`on`. The following example configuration would set the Stanchion host to
`localhost`, the port to `8085` (the default), and set up Stanchion to use SSL:

```riakcsconf
stanchion_host = 127.0.0.1:8085
stanchion_ssl = on
```

```advancedconfig
{riak_cs, [
    %% Other configs
    {stanchion_ip, "127.0.0.1"},
    {stanchion_port, 8085},
    {stanchion_ssl, true},
    %% Other configs
]}
```

## Specifying the Node Name

You can also set a more useful name for the Riak CS node, which is helpful to
identify the node from which requests originate during troubleshooting. This
setting resides in the Riak CS `riak-cs.conf` configuration file, or the old-
style `vm.args` file which is also located in the `/etc/riak-cs` directory. This
would set the name of the Riak CS node to `riak_cs@127.0.0.1`:

```riakcsconf
nodename = riak_cs@127.0.0.1
```

```vmargs
-name riak_cs@127.0.0.1
```

Change `127.0.0.1` to the IP address or hostname for the server on which Riak CS
is running.

## Specifying the Admin User

The admin user is authorized to perform actions such as creating users or
obtaining billing statistics. An admin user account is no different from any
other user account. **You must create an admin user to use Riak CS**.

<div class="note">
<div class="title">Note on anonymous user creation</div>
Before creating an admin user, you must first set `anonymous_user_creation = on`
in the Riak CS `riak-cs.conf` (or set `{anonymous_user_creation, true}` in the
old-style `advanced.config`/`app.config`). You may disable this again once the
admin user has been created.
</div>

To create an account for the admin user, use an HTTP `POST` request with the
username you want to use for the admin account. The following is an

```curl
curl -H 'Content-Type: application/json' \
  -XPOST http://<host>:<port>/riak-cs/user \
  --data '{"email":"foobar@example.com", "name":"admin_user"}'
```

The JSON response will look something like this:

```json
{
  "Email": "foobar@example.com",
  "DisplayName": "adminuser",
  "KeyId": "324ABC0713CD0B420EFC086821BFAE7ED81442C",
  "KeySecret": "5BE84D7EEA1AEEAACF070A1982DDA74DA0AA5DA7",
  "Name": "admin_user",
  "Id":"8d6f05190095117120d4449484f5d87691aa03801cc4914411ab432e6ee0fd6b",
  "Buckets": []
}
```

You can optionally send and receive XML if you set the `Content-Type` to
`application/xml`, as in this example:

Once the admin user exists, you must specify the credentials of the admin user
on each node in the Riak CS system. The admin user credential settings reside in
the Riak CS `riak-cs.conf` file, which is located in the `/etc/riak-cs`
directory. Paste the `key_id` string between the quotes for the `admin.key`.
Paste the `key_secret` string into the `admin.secret` variable, as shown here:

```riakcsconf
admin.key, 324ABC0713CD0B420EFC086821BFAE7ED81442C},
admin.secret, 5BE84D7EEA1AEEAACF070A1982DDA74DA0AA5DA7},
```

```advancedconfig
{riak_cs, [
           %% Admin user credentials
           {admin_key, "324ABC0713CD0B420EFC086821BFAE7ED81442C"},
           {admin_secret, "5BE84D7EEA1AEEAACF070A1982DDA74DA0AA5DA7"},
           %% Other configs
          ]}
```

## Bucket Restrictions

If you wish, you can limit the number of buckets created per user. The default
maximum is 100. Please note that if a user exceeds the bucket creation limit,
they are still able to perform other actions, including bucket deletion. You can
change the default limit using the `max_buckets_per_user` parameter in each
node's `advanced.config` file---there is no equivalent configuration for the
`riak-cs.conf` file. The example configuration below would set the maximum to
1000:

```advancedconfig
{riak_cs, [
           %% Other configs
           {max_buckets_per_user, 1000},
           %% Other configs
          ]}
```

If you want to avoid setting a limit on per-user bucket creation, you can set
`max_buckets_per_user` to `unlimited`.

## Connection Pools

Riak CS uses two distinct connection pools for communication with Riak: a
**primary** and a **secondary** pool.

The primary connection pool is used to service the majority of API requests
related to the upload or retrieval of objects. It is identified in the
configuration file as `pool.request.size`. The default size of this pool is 128.

The secondary connection pool is used strictly for requests to list the contents
of buckets. The separate connection pool is maintained in order to improve
performance. This secondary connection pool is identified in the configuration
file as `pool.list.size`. The default size of this pool is 5.

The following shows the `connection_pools` default configuration entry
that can be found in the `app.config` file:

```riakcsconf
pool.request.size = 128
pool.request.overflow = 0
pool.list.size = 5
pool.list.overflow = 0
```

```advancedconfig
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

The value for each pool split into pairs, with the first number representing the
normal size of the pool. This is representative of the number of concurrent
requests of a particular type that a Riak CS node may service. The second number
represents the number of allowed overflow pool requests that are allowed. It is
not recommended that you use any value other than 0 for the overflow amount
unless careful analysis and testing has shown it to be beneficial for a
particular use case.

### Tuning

We strongly recommend you that take care when setting the value of the
[[`pb_backlog` setting|Configuring Riak for
CS#Setting-Up-Riak-to-Use-Protocol-Buffers]] in Riak. When a Riak CS node is
started, each connection pool begins to establish connections to Riak. This can
result in a [[thundering
herd problem|http://en.wikipedia.org/wiki/Thundering_herd_problem]] in which
connections in the pool believe they are connected to Riak, but in reality some
of the connections have been reset. Due to TCP `RST` packet rate limiting
(controlled by `net.inet.icmp.icmplim`) some of the connections may not receive
notification until they are used to service a user's request. This manifests
itself as an `{error, disconnected}` message in the Riak CS logs and an error
returned to the user.

## Enabling SSL in Riak CS

```riakcsconf
ssl.certfile = "./etc/cert.pem"
ssl.keyfile = "./etc/key.pem"
```

```advancedconfig
{ssl, [
    {certfile, "./etc/cert.pem"},
    {keyfile, "./etc/key.pem"}
   ]},
```

Replace the text in quotes with the path and filename for your SSL encryption
files. By default, there's a `cert.pem` and a `key.pem` in each node's `/etc`
directory. You're free to use those or to supply your own.

Please note that you may also need to provide a [certificate
authority](http://en.wikipedia.org/wiki/Certificate_authority), aka a CA cert.
If you do, you must use the `advanced.config` file, and specify its location
using the `cacertfile` parameter.  Unlike `certfile` and `keyfile`, the
`cacertfile` parameter is not commented out. You will need to
add it yourself. Here's an example configuration with this parameter included:

```advancedconfig
{ssl, [
       {certfile, "./etc/cert.pem"},
       {keyfile, "./etc/key.pem"},
       {cacertfile, "./etc/cacert.pem"}
      ]},
      %% Other configs
```

Instructions on creating your own CA cert can be found
[here](http://www.akadia.com/services/ssh_test_certificate.html).

## Proxy vs. Direct Configuration

Riak CS can interact with S3 clients in one of two ways:

* A [**proxy** configuration](http://basho.com/riak-cs-proxy-vs-direct-configuration/)
  enables an S3 client to communicate with Riak CS as if it were Amazon S3
  itself, i.e. using typical Amazon URLs.
* A **direct** configuration requires that an S3 client connecting to Riak CS be
  configured for an "S3-compatible service," i.e. with a Riak CS endpoint that
  is not masquerading as Amazon S3. Examples of such services include
  [Transmit](http://panic.com/transmit/), [s3cmd](http://s3tools.org/s3cmd), and
  [DragonDisk](http://www.dragondisk.com/).

### Proxy

To establish a proxy configuration, configure your client's proxy settings to
point to Riak CS cluster's address. Then configure your client with Riak CS
credentials.

When Riak CS receives the request to be proxied, it services the request itself
and responds back to the client as if the request went to S3.

On the server side, the `root_host` configuration in the `riak-cs.conf` file
must be set to `s3.amazonaws.com` because all of the bucket URLs request by the
client will be destined for `s3.amazonaws.com`. This is the default.

**Note**: One issue with proxy configurations is that many GUI clients only
allow for one proxy to be configured for all connections. For customers trying
to connect to both S3 and Riak CS, this can prove problematic.

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

The following options are available to make adjustments to the Riak CS garbage
collection system. More details about garbage collection in Riak CS are
available in [[Garbage Collection]].

* `gc.leeway_period` (`leeway_seconds` in `advanced.config` or `app.config`) ---
  The amount of time that must elapse before an object version that has been
  explicitly deleted or overwritten is eligible for garbage collection. The
  default value is `24h` (24 hours).
* `gc.interval` (`gc_interval` in `advanced.config` or `app.config`) --- The
  interval at which the garbage collection daemon runs to search for and reap
  eligible object versions. The default value is `15m` (15 minutes). It is
  important that you have only _one_ garbage collection daemon running in a
  cluster at any point in time. To disable the daemon on a node, set the
  `gc.interval` parameter to `infinity`.
* `gc.retry_interval` (`gc_retry_interval` in `advanced.config` or `app.config`)
  --- The amount of time that must elapse before another attempt is made to
  write a record for an object manifest in the `pending_delete` state to the
  garbage collection eligibility bucket. In general, this timeout should never
  expire, but may if an error condition caused the original record in the
  garbage collection eligibility bucket to be removed prior to the reaping
  process completing. The default value is `6h` (6 hours).
* `gc.max_workers` (`gc.max_workers` in `advanced.config` or `app.config`) ---
  The maximum number of worker processes that may be started by the garbage
  collection daemon to use for concurrent reaping of garbage-collection-eligible
  objects. The default value is 2.

There are some additional settings that may only be configured in the
`advanced.config` or `app.config` configuration files. **None of the below
settings are available through the `riak-cs.conf` configuration file.**

* `epoch_start` --- The time that the garbage collection daemon uses
  to begin collecting keys from the garbage collection eligibility
  bucket. Records in this bucket use keys based on the epoch time the
  record is created + `leeway_seconds`. The default is 0 and should be
  sufficient for general use. A case for adjusting this value is if the
  secondary index query run by the garbage collection daemon continually
  times out. Raising the starting value can decrease the range of the
  query and make it more likely the query will succeed. The value must
  be specified in Erlang binary format. *e.g.* to set it to 10, specify
  `<<"10">>`.
* `initial_gc_delay` --- The number of seconds to wait in addition to
  the `gc_interval` value before the first execution of the garbage
  collection daemon when the Riak CS node is started. **Note**:
  Originally, this setting was used to stagger the execution of GC on
  multiple nodes; we no longer recommend running multiple GC daemons.
  Correspondingly, we do not recommend setting `initial_gc_delay`.
* `max_scheduled_delete_manifests` --- The maximum number of
  manifests (representative of object versions) that can be in the
  `scheduled_delete` state for a given key. A value of `unlimited` means
  there is no maximum, and pruning will not happen based on
  count. An example of where this option is useful is a use case
  involving a lot of churn on a fixed set of keys in a time frame that
  is relatively short compared to the `leeway_seconds` value. This can
  result in the manifest objects reaching a size that can negatively
  impact system performance. The default value is `unlimited`.

<div class="note"><div class="title">Deprecated Configurations</div>
While Riak CS 2.0.0 still allows the configuration of `gc_paginated_indexes` and
`gc_batch_size`, it is strongly recommended that these settings not be used. The
have been deprecated, and <em>will be removed</em> in the next major release.
</div>

## Other Riak CS Settings

For a complete listing of configurable parameters for Riak CS, see the
[[configuration reference|Riak CS Configuration Reference]] document.
