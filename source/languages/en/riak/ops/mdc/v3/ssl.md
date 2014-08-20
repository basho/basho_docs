---
title: "Multi Data Center Replication v3 SSL"
project: riak
header: riakee
version: 1.3.2+
document: cookbook
toc: true
audience: intermediate
keywords: [mdc, repl, ssl]
moved: {
    '2.0.0-': 'riakee:/cookbooks/Multi-Data-Center-Replication-v3-SSL'
}
---

## Features

Riak Multi-Datacenter (MDC) Replication SSL consists of the following
items:

  * Encryption of replication data
  * SSL certificate chain validation
  * SSL common name whitelisting support

## SSL Configuration

To configure SSL, you will need to include the following 4 settings in
the `riak-core` section of `[[app.config|Configuration Files#app.config]]`:

```appconfig
{riak_core, [
             % ...
             {ssl_enabled, true},
             {certfile, "/full/path/to/site1-cert.pem"},
             {keyfile, "/full/path/to/site1-key.pem"},
             {cacertdir, "/full/path/to/cacertsdir"}
             % ...
            ]}

```

The `cacertsdir` is a directory containing all the CA certificates
needed to verify the CA chain back to the root.

<div class="note">
<div class="title">Note on configuration</div>
In Version 3 replication, the SSL settings need to be placed in the
<code>riak-core</code> section of <code>app.config</code> as opposed to
the <code>riak-repl</code> section used by Version 2 replication.
</div>

## Verifying peer certificates

Verification of a peer's certificate common name is enabled using the
`peer_common_name_acl` property in the `riak_repl` section of
`app.config`.

You can provide multiple ACLs, separated by commas, and you can wildcard
the leftmost part of the common name. For example, `*.corp.com` would
match `site3.corp.com` but not `foo.bar.corp.com` or `corp.com`. If the
ACL is specified and not the special value `"*"`, certificates not
matching any of the rules will not be allowed to connect.

If no ACLs are configured, no checks on the common name are done.

### Examples

The following example will only allow connections from peer certificate
names like `db.bashosamplecorp.com` and `security.bashosamplecorp.com`:

```appconfig
{riak_core, [
             % ...
             {peer_common_name_acl, ["db.bashosamplecorp.com", "security.bashosamplecorp.com"]}
             % ...
            ]}

```

The following example will allow connections from peer certificate names
like `foo.bashosamplecorp.com` or `db.bashosamplecorp.com`, but not a
peer certificate name like `db.backup.bashosamplecorp.com`.

```appconfig
{riak_core, [
             % ...
             {peer_common_name_acl, ["*.bashosamplecorp.com"]}
             % ...
            ]}

```

This example will match any peer certificate name (and is the default):

```appconfig
{riak_core, [
             % ...
             {peer_common_name_acl, "*"}
             % ...
            ]}

```

## SSL CA Validation

You can adjust the way CA certificates are validated by adding the
following to the `riak_repl` section of `app.config`:

```appconfig
{riak_core, [
             % ...
             {ssl_depth, 3} % Sets the depth to 3
             % ...
            ]}

```

**Note**: `ssl_depth` takes an integer parameter.

The depth specifies the maximum number of intermediate certificates that
may follow the peer certificate in a valid certification path. The
intermediate certificates must not be self signed.

The following example depths illustrate this:

  * a depth of `0` indicates that the certificate must be signed
    directly by a root certificate authority (CA)
  * a depth of `1` indicates that the certificate may be signed by at
    most 1 intermediate CA's, followed by a root CA
  * a depth of `2` indicates that the certificate may be signed by at
    most 2 intermediate CA's, followed by a root CA

## Compatibility

{{#1.2.0-1.3.9}}
Replication SSL for *Version 2* is available in *Riak 1.2+*.
{{/1.2.0-1.3.9}}
{{#1.4.0+}}
Replication SSL for *Version 3* is available in *Riak 1.4+*.
{{/1.4.0+}}

If SSL is enabled and a connection is made to a Riak Enterprise 1.0 or
1.1 node, the connection will be denied and an error will be logged.

### Self-Signed Certificates

Read how to [generate your own CA and
keys](http://www.debian-administration.org/articles/618). Ensure that
you remove the password protection from the keys you generate.
