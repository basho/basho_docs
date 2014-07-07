---
title: Client-side Security
project: riak
version: 2.0.0+
document: tutorial
audience: advanced
keywords: [developers, security, ssl, certificate]
---

If you have enabled [[Riak Security|Authentication and Authorization]]
and chosen either `password`, `certificate`, or `pam` as your
[[security source|Managing Security Sources]], any client attempting to
connect to Riak will have to authenticate itself, either using username
and password (for the `password` or `pam` security sources) or a
client-side SSL certificate signed by the same authority as the SSL
certificate recognized by your Riak cluster.

This tutorial will show you to implement client-side authentication for
each of Riak's officially supported [[client libraries]]. A guide to
server-side certificate management can be found in our documentation on
[[security source management|Managing Security Sources#certificate-based-authentication]].

## Password-based Security

The sections below cover password-based security for each of the
official Basho Riak clients. Each example uses the username of
`riakuser` and a password of `rosebud`.

### Ruby

In the Ruby client, username and password are specified when you create
a client object using the `[Riak::Client](https://github.com/basho/riak-ruby-client/blob/master/lib/riak/client.rb)`
class. Here is a client object (we'll name it `client`) without a
specified username and password:

```ruby
client = Riak::Client.new(host: '127.0.0.1', pb_port: 8087)
```

To specify a username and password, do so within an `authentication`
hash along with the host and port:

```ruby
client = Riak::Client.new(
  host: '127.0.0.1',
  pb_port: 8087,
  authentication: {
  	user: 'riakuser',
  	password: 'rosebud'
  }
)
```

### Erlang

In the Erlang client, username and password are specified when you
establish a process ID for the client's [[Protocol Buffers|PBC API]]
socket connection. The following opens a connection without specifying
username and password:

```erlang
{ok, Pid} = riakc_pb_socket:start("127.0.0.1", 8087).
```

To specify a username and password, do so within a `credentials` tuple
passed to the `start` function along with the host and port:

```erlang
{ok, Pid} = riakc_pb_socket:start(
    "127.0.0.1", 8087,
    [{credentials, "riakuser", "rosebud"}]
).
```

## Certificate-based Security

The examples below will assume that you have already done the following:

1. Your server-side SSL certificates have been created and signed and your Riak servers have been configured to recognize a cert (`cert.pem`) and a signing authority (`cacertfile.pem`)
2. Your client has access to a `/ssl` directory that houses a CA file to validate the server cert (`ca.crt`) and an SSL key file (`key.pem`)
3. The required username and password for your client are `riakuser` and `rosebud`, respectively

This tutorial will not cover the basics of generating certificates, and
will assume that your signing authority and cert correspond to the files
specified in your `riak.conf` [[configuration file|Configuration Files]]
under `ssl.cacertfile` and `ssl.certfile`, respectively. For this
example, we'll use the default names for those files: `cacertfile.pem`
and `cert.pem`.

<div class="note">
<div class="title">Note on certificates and the HTTP interface</div>
In general, we advise against using Riak's HTTP interface in
conjunction with SSL certificates.
</div>

### Ruby

In the Ruby client, SSL certificate information is passed to a `client`
object in the same `authentication` hash used to specify username and
password. The following example specifies the CA, key, and client and
server certificates on the basis of their respective filenames (using
the filenames listed above, i.e. `ca.crt`, etc.):

```ruby
client = Riak::Client.new(
  host: '127.0.0.1',
  pb_port: 8087,
  authentication: {
    user: 'riakuser',
    password: 'rosebud',
    ca_file: '/ssl/ca.crt',
    key: '/ssl/key.pem',
    ca_path: '/ssl/certs'
  }
)
```

In addition to specifying certs on the basis of filenames, you can also
pass in either OpenSSL-compatible string data or properly initialized
OpenSSL objects. The following example passes in string data:

```ruby
client = Riak::Client.new(
  # connection info from above
  authentication: {
    # username/password from abvoe
    ca_file: File.read('/ssl/ca.crt'),
    key: File.read('/ssl/key.pem'),
    ca_path: Dir.glob('/ssl/certs/*')
  }
)
```

This example uses OpenSSL-compatible objects, using the
[`jruby-openssl`](https://rubygems.org/gems/jruby-openssl) gem:

```ruby
client = Riak::Client.new(
  # connection info from above
  authentication: {
    # username/password from abvoe
    ca_file: OpenSSL::X509::Certificate.new(File.read('/ssl/ca.crt')),
    key: OpenSSL::PKey::RSA.new(File.read('/ssl/key.pem')),
    ca_path: # ?
  }
)
```

### Erlang

```erlang
CertDir = "/path/to/ssl",
{ok, Pid} = riakc_pb_socket:start(
    "127.0.0.1", 8087,
    [{credentials, "riakuser", "rosebud"},
     {cacertfile, filename:join([CertDir, "certfile.pem"])},
     {certfile, filename:join([CertDir, "cert.pem"])}
    ]).
```

## Testing Your Setup

### Ruby

In the Ruby client, you can test your setup using the `ping` method on
the client object you set up:

```ruby
# Using the client object from above

client.ping
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

If you get `SSL now works!` as a response, then your client-side authentication setup should be working normally.

## Resources

* [OpenSSL tutorial](http://www.madboa.com/geek/openssl/)