---
title_supertext: "V3 Multi-Datacenter Replication:"
title: "SSL"
description: ""
project: "riak_kv"
project_version: 2.9.8
menu:
  riak_kv-2.9.8:
    name: "SSL"
    identifier: "configuring_v3_replication_ssl"
    weight: 103
    parent: "configuring_v3"
toc: true
commercial_offering: true
aliases:
  - /riak/2.9.8/ops/mdc/v3/ssl
  - /riak/kv/2.9.8/ops/mdc/v3/ssl
---

[config reference#advanced.config]: {{<baseurl>}}riak/kv/2.9.8/configuring/reference/#the-advanced-config-file

## Features

Riak Multi-Datacenter (MDC) Replication SSL consists of the following
items:

  * Encryption of replication data
  * SSL certificate chain validation
  * SSL common name whitelisting support

> **Note on cross-internet traffic**
>
> As an alternative to Riak's built-in SSL capabilities, we
recommend using [stunnel](https://www.stunnel.org/index.html) or a
virtual private network (VPM) for inter-datacenter connections.

## SSL Configuration

To configure SSL, you will need to include the following 4 settings in
the `riak-core` section of [`advanced.confg`][config reference#advanced.config]:

```advancedconfig
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

{{% note title="Note on configuration" %}}
In Version 3 replication, the SSL settings need to be placed in the
`riak-core` section of `advanced.config` as opposed to the `riak-repl` section
used by Version 2 replication.
{{% /note %}}

## Verifying Peer Certificates

Verification of a peer's certificate common name *(CN)* is enabled by using
the `peer_common_name_acl` property in the `riak_core` section of your
`advanced.config` to specify an Access Control List *(ACL)*.

The ACL is a list of one or more *patterns*, separated by commas. Each
pattern may be either the exact CN of a certificate to allow, or a
wildcard in the form `*.some.domain.name`. Pattern comparison is
case-insensitive, and a CN matching any of the patterns is allowed to connect.

For example, `["*.corp.com"]` would match `site3.corp.com` but not
`foo.bar.corp.com` or `corp.com`. If the ACL were
`["*.corp.com", "foo.bar.corp.com"]`, `site3.corp.com` and `foo.bar.corp.com`
would be allowed to connect, but `corp.com` still would not.

If no ACL (or only the special value `"*"`) is specified, no CN filtering
is performed, except as described below.

{{% note title="Identical Local and Peer Common Names" %}}
As a special case supporting the view that a host's CN is a fully-qualified
domain name that uniquely identifies a single network device, if the CNs of
the local and peer certificates are the same, the nodes will *NOT* be allowed
to connect.

This evaluation supercedes ACL checks, so it cannot be overridden with any
setting of the `peer_common_name_acl` property.
{{% /note %}}

### Examples

The following example will only allow connections from peer certificate
names like `db.bashosamplecorp.com` and `security.bashosamplecorp.com`:

```advancedconfig
{riak_core, [
             % ...
             {peer_common_name_acl, ["db.bashosamplecorp.com", "security.bashosamplecorp.com"]}
             % ...
            ]}

```

The following example will allow connections from peer certificate names
like `foo.bashosamplecorp.com` or `db.bashosamplecorp.com`, but not a
peer certificate name like `db.backup.bashosamplecorp.com`.

```advancedconfig
{riak_core, [
             % ...
             {peer_common_name_acl, ["*.bashosamplecorp.com"]}
             % ...
            ]}

```

This example will match any peer certificate name (and is the default):

```advancedconfig
{riak_core, [
             % ...
             {peer_common_name_acl, "*"}
             % ...
            ]}

```

## SSL CA Validation

You can adjust the way CA certificates are validated by adding the
following to the `riak_repl` section of `advanced.config`:

```advancedconfig
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

Replication SSL for *Version 3* is available in *Riak 1.4+*.

If SSL is enabled and a connection is made to a Riak Enterprise 1.0 or
1.1 node, the connection will be denied and an error will be logged.

### Self-Signed Certificates

Read how to [generate your own CA and
keys](http://www.debian-administration.org/articles/618). Ensure that
you remove the password protection from the keys you generate.




