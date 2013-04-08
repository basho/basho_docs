---
title: Configuring Riak CS
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, configuration]
---

## Specifying the Admin User

The admin user is authorized to perform actions such as creating buckets or obtaining billing statistics. An admin user account is no different than any other user account.

<div class="note"><div class="title">Note</div>
Before creating an admin user, you must first set <tt>{anonymous_user_creation, true}</tt> in the Riak CS <tt>app.config</tt>. You may disable this again once the admin user has been created.
</div>

To create an account for the admin user, use an HTTP POST with the username you want for the admin account. The following is an example:

```
curl -H 'Content-Type: application/json' \
  -X POST http://localhost:8080/riak-cs/user \
  --data '{"email":"foobar@example.com", "name":"admin user"}'
```

The JSON response looks something like:

```json
{
"Email": "foobar@example.com",
"DisplayName": "adminuser"
"KeyId": "324ABC0713CD0B420EFC086821BFAE7ED81442C",
"KeySecret": "5BE84D7EEA1AEEAACF070A1982DDA74DA0AA5DA7",
"Name": "admin user",
"Id":"8d6f05190095117120d4449484f5d87691aa03801cc4914411ab432e6ee0fd6b",
"Buckets": []
}
```

You can optionally send and receive XML, if you set the `Content-Type` to `application/xml`.

Once the admin user exists, you must specify the credentials of the admin user on each node in the Riak CS system. The admin user credential settings reside in the Riak CS `app.config` file, which is located in the `etc/riak-cs` directory. The settings appear in the Riak CS config section of the file. Paste the `key_id` string between the quotes for the `admin_key`. Paste the `key_secret` string into the `admin_secret` variable, as shown here:

```
%% Admin user credentials
 {admin_key, "LXAAII1MVLI93IN2ZMDD"},
 {admin_secret, "5BE84D7EEA1AEEAACF070A1982DDA74DA0AA5DA7"},
```

## Listening on a Public Address

If you have a single node, you don't have to change the settings for the address to listen on, because Riak CS simply listens to the requests from the local host. If your system has multiple nodes, you set the IP address and port that Riak CS listens on for requests from other nodes.

The public address settings reside in the Riak CS `app.config` file, which is located in the` /etc/riak-cs` directory. The settings appear in the Riak CS config section of the file.

__riak_ip__: Replace 127.0.0.1 with the IP address of the Riak node you want Riak CS to connect to.

If you configured Riak to use a different port for protocol buffers, you must change the following port setting:

__riak_pb_port__: Replace 8087 with the port number set in the variable `pb_port` in the Riak `app.config` file.

<div class="note"><div class="title">Note</div>The IP address you enter here must match the IP address specified for the pb_ip variable in the Riak app.config file. If a server has more than one network interface card (NIC), you can use the IP address for a specific NIC. If you want Riak CS to listen on all of them, set riak_ip to "0.0.0.0".</div>

After modifying the port numbers, restart Riak if is already running.

## Specifying the Stanchion Node
If you have a single node, you don't have to change the Stanchion settings, because Stanchion runs on the local host. If your Riak CS system has multiple nodes, you set the IP address and port for the Stanchion node and whether SSL is enabled.

The Stanchion settings reside in the Riak CS app.config file, which is located in the /etc/Riak-CS directory. The settings appear in the Riak CS config section of the file.

__stanchion_ip__: Replace 127.0.0.1 with the IP address of the Stanchion node.

If you configured Stanchion to use a different port, you must change the following port setting:

__stanchion_port__: Replace 8085 with the port number set in the variable stanchion_port in the Stanchion `app.config` file.

The __stanchion_ssl__ variable is set to false by default. If you want to use SSL, change this variable to true.

## Specifying the Node IP Address
You can also set the IP address for the Riak CS node, which is helpful if you must debug code or identify the node from which requests originate. The Riak CS IP address setting resides in the Riak CS `vm.args` configuration file, which is located in the `/etc/riak-cs` directory.

Initially, the line that specifies the Riak CS node IP address is set to the localhost, as follows:

```
## Name of the riak node
-name riak_cs@127.0.0.1
```

Replace 127.0.0.1 with the IP address for the Riak CS node.

## Enabling SSL in Riak CS
In the Riak CS `app.config` file, first uncomment the following lines:

```erlang
%%{ssl, [
%%    {certfile, "./etc/cert.pem"},
%%    {keyfile, "./etc/key.pem"}
%%   ]},
```

Replace the text in quotes with the path and filename for your SSL encryption files.

## Other Riak CS Settings
The `app.config` file includes other settings, such as whether to create log files and where to store them. These settings have default values that work in most cases.
