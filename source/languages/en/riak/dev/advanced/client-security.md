---
title: Client-Side Security
project: riak
version: 2.0.0+
document: tutorial
audience: advanced
keywords: [developers, security, ssl, certificate]
---

If you have enabled [[Riak Security|Authentication and Authorization]] and chosen either `password`, `certificate`, or `pam` as your [[security source|Managing Security Sources]], any client attempting to connect to Riak will have to authenticate itself, either using username and password (for `password` or `pam`) or a client-side SSL certificate that matches the certificate on your Riak server.

This tutorial will show you to implement client-side authentication for each of Riak's officially supported [[client libraries]]. A guide to server-side certificate management can be found in our documentation on [[security source management|Managing Security Sources#certificate-based-authentication]].

**Note**: The examples below use host 127.0.0.1 and port 10017 to establish a [[Protocol Buffers|PBC API]] connection to Riak and a username of `riakuser` and password of `rosebud` for authentication.

## Password-based Security

#### Ruby

In the Ruby client, username and password are specified when you create a client object using the `[Riak::Client](https://github.com/basho/riak-ruby-client/blob/master/lib/riak/client.rb)` class. Here is a client object (we'll name it `client`) without a specified username and password:

```ruby
client = Riak::Client.new(host: '127.0.0.1', pb_port: 10017)
```

Username and password are specified within an `authentication` hash:

```ruby
client = Riak::Client.new(
  host: '127.0.0.1',
  pb_port: 10017,
  authentication: {
  	user: 'riakuser',
  	pass: 'rosebud'
  }
)
```

#### Erlang

In the Erlang client, username and password are specified when you establish a process ID for the client's [[Protocol Buffers|PBC API]] socket connection. The following opens a connection without specifying username and password:

```erlang
{ok, Pid} = riakc_pb_socket:start("127.0.0.1", 10017).
```

The following opens a connection with username and password:

```erlang
{ok, Pid} = riakc_pb_socket:start(
    "127.0.0.1", 10017,
    [{credentials, "riakuser", "rosebud"}]
).
```


## Certificate-based Security

Let's say that (a) your server-side SSL certificates have already been created and signed and that your Riak servers have been configured to recognize those certificates, (b) you have copies of those certificates in an `/ssl` directory that is accessible to your Riak client, and (c) your client's username is `riakuser` and the password is `rosebud`.

This tutorial will not cover the basics of generating certificates, and will assume that your signing authority and cert correspond to the files specified in your `riak.conf` [[configuration file|Configuration Files]] under `ssl.cacertfile` and `ssl.certfile`, respectively. For this example, we'll use the default names for those files: `cacertfile.pem` and `cert.pem`.

You will need to point your client to those certificates upon instantiation. Let's say that you're instantiating a client for a one-node cluster, using the host `127.0.0.1` and port `10017`.

<div class="note">
<div class="title">Note on certificates and the HTTP interface</div>
Because

In general, we do not recommend using Riak's HTTP interface in conjunction with SSL certificates.
</div>

#### Ruby

```ruby
client = Riak::Client.new(
  host: '127.0.0.1',
  pb_port: 10017,
  authentication: {
    user: 'riakuser',
    password: 'rosebud',
    server_ca: File.read('/path/to/ssl/cacertfile.pem'),
    server_cert: File.read('/path/to/ssl/cert.pem')
  }
)
```

#### Erlang

```erlang
CertDir = "/path/to/ssl",
{ok, Pid} = riakc_pb_socket:start(
    "127.0.0.1", 10017,
    [{credentials, "riakuser", "rosebud"},
     {cacertfile, filename:join([CertDir, "certfile.pem"])},
     {certfile, filename:join([CertDir, "cert.pem"])}
    ]).
```

## Testing Your Setup

#### Ruby

```ruby
client = Riak::Client.new(host: '127.0.0.1', pb_port: 10017)
bucket = client.bucket('certificate_test')
obj = Riak::RObject.new(bucket, 'test_key')
obj.content_type = 'text/plain'
obj.raw_data = 'SSL now works!'
obj.store
bucket.get('test_key').data
```

#### Erlang

```erlang
%% Using the Pid variable from the code sample above:
Object = riakc_obj:new(<<"certificate_test">>,
                       <<"test_key">>,
                       <<"SSL now works!">>,
                       <<"text/plain">>),
riakc_pb_socket:put(Pid, Object).

{ok, FetchedObject} = riakc_pb_socket:get(Pid,
                                          <<"certificate_test">>,
                                          <<"test_key">>).
```

If you get the `SSL now works!` as a response, then your client-side authentication setup should be working normally.

## Resources

* [OpenSSL tutorial](http://www.madboa.com/geek/openssl/)