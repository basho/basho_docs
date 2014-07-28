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

The settings reside in the Riak CS `app.config` file, which is located
in the `/etc/riak-cs` directory. Configurable parameters related to
Riak CS specifically can be found in the `riak_cs` section of that file.
That section looks something like this:

```appconfig
{riak_cs, [
	%% Configs
]},
```

### Host and Port

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

After making any changes to the `app.config` file in Riak CS, restart it
if it is already running.

## Specifying the Stanchion Node

If you have a single node, you don't have to change the Stanchion
settings because Stanchion runs on the local host. If your Riak CS
system has multiple nodes, however, you must specify the IP address and
port for the Stanchion node and whether or not SSL is enabled.

The Stanchion settings reside in the Riak CS `app.config` file, which is
located in the `/etc` directory of each Riak CS node. The settings
appear in the `riak_cs` config section of the file.

* `stanchion_ip` --- Replace `127.0.0.1` with the IP address of the
  Stanchion node

If you configured Stanchion to use a different port, you must change the
following port setting:

* `stanchion_port` --- Replace `8085` with the port number set in the
  `stanchion_port` variable

The `stanchion_ssl` variable is set to `false` by default. If Stanchion
is configured to use SSL, change this variable to `true`. The following
example configuration would set the host to `localhost`, the port to
`8085` (the default), and set up Stanchion to use SSL:

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

To create an account for the admin user, use an HTTP `POST` with the
username you want to use for the admin account. The following is an
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

```curl
curl -XPOST http://localhost:8080/riak-cs/user \
  -H 'Content-Type: application/xml' \
  --data '<?xml version="1.0"?><email>foobar@example.com</email><name>admin user</name>'
```

Once the admin user exists, you must specify the credentials of the
admin user in the `app.config` file. Those will be the same credentials
that you received as a JSON object when you ran the `POST` request to
create the user.

In the `riak_cs` section of `app.config`, add the following to establish
the admin user credentials:

```appconfig
{riak_cs, [
			%% Other configs

            {admin_key, "324ABC0713CD0B420EFC086821BFAE7ED81442C"},
			{admin_secret, "5BE84D7EEA1AEEAACF070A1982DDA74DA0AA5DA7"},

			%% Other configs
]}
```

## Enabling SSL in Riak CS

To enable SSL, first uncomment the following lines in your Riak CS 
`app.config` file:

```erlang
%%{ssl, [
%%    {certfile, "./etc/cert.pem"},
%%    {keyfile, "./etc/key.pem"}
%%   ]},
```

Then replace the text in quotes with the path and filename for your SSL
encryption files.

## Proxy vs. Direct Configuration

Riak CS can interact with S3 clients in one of two ways. A **proxy**
configuration enables an S3 client to communicate with Riak CS as if it
were Amazon S3 itself, i.e. using typical Amazon URLs. A **direct**
configuration requires that an S3 client connecting to Riak CS be
configured for an "S3-compatible service," i.e. with a Riak CS endpoint
that is not masquerading as Amazon S3. Examples of such services include
[Transmit](http://panic.com/transmit/), [s3cmd](http://s3tools.org/s3cmd),
and [DragonDisk](http://www.dragondisk.com/).

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

The `app.config` file includes other settings, such as whether to create log files and where to store them. These settings have default values that work in most cases.
