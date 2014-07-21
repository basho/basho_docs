---
title: "Multi Data Center Replication: SSL"
project: riak
header: riakee
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, ssl]
moved: {
    '2.0.0-': 'riakee:/cookbooks/Multi-Data-Center-Replication-SSL'
}
---

## Features

Riak REPL SSL support consists of the following items:

  * Encryption of replication data
  * SSL certificate chain validation
  * SSL common name whitelisting support

## SSL Configuration

To configure SSL, you will need to include the following 4 settings in the
`riak-repl` section of your `app.config`:

```erlang
{ssl_enabled, true},
{certfile, "/full/path/to/site1-cert.pem"},
{keyfile, "/full/path/to/site1-key.pem"},
{cacertdir, "/full/path/to/cacertsdir"}
```

The `cacertdir` is a directory containing all of the CA certificates needed to
verify the CA chain back to the root.

## Verifying Peer Certificates

Verification of a peer's certificate common name is enabled by using the 
`peer_common_name_acl` property in the `riak_repl` section of your `app.config`.

You can provide multiple ACLs, separated by commas, and you can wildcard
the leftmost part of the common name. For example, `*.corp.com` would match
`site3.corp.com` but not `foo.bar.corp.com` or `corp.com`. If the ACL is
specified and not the special value `"*"`, certificates not matching any
of the rules will not be allowed to connect.

If no ACLs are configured, no checks on the common name are done.

### Examples

The following example will only allow connections from peer certificate names like `db.bashosamplecorp.com` and `security.bashosamplecorp.com`:

```erlang
{peer_common_name_acl, ["db.bashosamplecorp.com", "security.bashosamplecorp.com"]}
```

The following example will allow connections from peer certificate names like `foo.bashosamplecorp.com` or `db.bashosamplecorp.com`, but not a peer certificate name like `db.backup.bashosamplecorp.com`:

```erlang
{peer_common_name_acl, ["*.bashosamplecorp.com"]}
```

This example will match any peer certificate name (and is the default):

```erlang
{peer_common_name_acl, "*"}
```

## SSL CA Validation

You can adjust the way CA certificates are validated by adding the following to the `riak_repl` section of your `app.config`:

```erlang
{ssl_depth, ...}
```

**Note**: `ssl_depth` takes an integer parameter.

The depth specifies the maximum number of intermediate certificates that may follow the peer certificate in a valid certification path. The intermediate certificates must not be self signed.

For example:

  * a depth of 0 indicates that the certificate must be signed directly by a root certificate authority (CA).
  * a depth of 1 indicates that the certificate may be signed by at most 1 intermediate CA's, followed by a root CA.
  * a depth of 2 indicates that the certificate may be signed by at most 2 intermediate CA's, followed by a root CA.

## Compatibility

Replication SSL is ONLY available in Riak 1.2+.

If SSL is enabled and a connection is made to a Riak Enterprise 1.0 or 1.1 node, the connection will be denied and an error will be logged.

### Self-Signed Certificates

You can generate your own CA and keys by using [this guide](http://www.debian-administration.org/articles/618).

Make sure that you remove the password protection from the keys you generate.
