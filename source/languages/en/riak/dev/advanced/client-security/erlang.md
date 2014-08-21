---
title: "Client-side Security: Erlang"
project: riak
version: 2.0.0+
document: tutorial
audience: advanced
keywords: [developers, security, ssl, erlang]
---

This tutorial shows you how to set up a Riak Erlang client to
authenticate itself when connecting to Riak using all four of the
[[available security sources|Managing Security Sources]]:

* [[trust|Managing Security Sources#Trust-based-Authentication]]
* [[password|Managing Security Sources#Password-based-Authentication]]
* [[certificates|Managing Security Sources#Certificate-based-Authentication]]
* [[pluggable authentication modules (PAM)|Managing Security
  Sources#PAM-based-Authentication]])

<div class="note">
<div class="title">Note on certificate generation</div>
This tutorial does not cover certificate generation. It assumes that all
necessary certificates have already been created and are stored in a
directory called `/ssl_dir`. This directory name is used only for
example purposes.
</div>

## Erlang Client Basics

When connecting to Riak using an Erlang-based client, you typically use
a process identifier to refer to the client connection. The following
example creates a process identifier (we'll call it `Pid`) for a
connection to `localhost` on port 8087:

```erlang
{ok, Pid} = riakc_pb_socket:start("127.0.0.1", 8087).
```

If you are using Riak security, _all_ connecting clients should have
access to the same Certificate Authority (CA) used on the server side,
regardless of which [[security source|Managing Security Sources]] you
choose. In addition, all clients should provide a username. The example
above created a connection to Riak without specifying a username or CA.
That information is specified as a list of options passed to the
`start` function. We'll specify those options in a list called
`SecurityOptions`.

```erlang
CertDir = "/ssl_dir",
SecurityOptions = [
                   {credentials, "riakuser", ""},
                   {cacertfile, filename:join([CertDir, "cacertfile.pem"])}
                  ],
{ok, Pid} = riakc_pb_socket:start("127.0.0.1", 8087, SecurityOptions).
```

Please note that you do not need to specify a password if you are not
using password-based authentication. If you are using a different
security source, Riak will ignore the password. You can enter an empty
string (as in the example above) or anything you'd like.

This client is not currently set up to use any of the available security
sources, with the exception of trust-based authentication, provided that
the [CIDR](http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing)
from which the client is connecting has been specified as trusted. More
on specifying trusted CIDRs can be found in [[Trust-based
Authentication|Managing Security Sources#Trust-based-Authentication]].

## Password-based Authentication

To enable our client to use password-based auth, we can use most of the
information from the example above, with the exception that we'll also
specify a password for the client in the `SecurityOptions` list from
above. We'll use the password `rosebud` here and in the rest of the
examples.

```erlang
CertDir = "/ssl_dir",
SecurityOptions = [
                   {credentials, "riakuser", "rosebud"},
                   {cacertfile, filename:join([CertDir, "cacertfile.pem"])}
                  ],
{ok, Pid} = riakc_pb_socket:start("127.0.0.1", 8087, SecurityOptions).
```

## Certificate-based Authentication

Using certificate-based authentication requires us to specify the
location of a general CA (as with all security soruces), a username, a
CA-generated cert, and a private key. We'll assume that all certs are
stored in `/ssl_dir`, as in the previous examples.

```erlang
CertDir = "/ssl_dir",
SecurityOptions = [
                   {credentials, "riakuser", "rosebud"},
                   {cacertfile, filename:join([CertDir, "cacertfile.pem"])},
                   {certfile, filename:join([CertDir, "cert.pem"])},
                   {keyfile, filename:join([CertDir, "key.pem"])}
                  ],
{ok, Pid} = riakc_pb_socket:start("127.0.0.1", 8087, SecurityOptions).
```
