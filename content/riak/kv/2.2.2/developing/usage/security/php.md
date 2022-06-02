---
title_supertext: "Client Security:"
title: "PHP"
description: ""
project: "riak_kv"
project_version: "2.2.2"
menu:
  riak_kv-2.2.2:
    name: "PHP"
    identifier: "usage_security_php"
    weight: 104
    parent: "usage_security"
toc: true
aliases:
  - /riak/2.2.2/dev/advanced/client-security/php
  - /riak/kv/2.2.2/dev/advanced/client-security/php
---

This tutorial shows you how to set up a Riak PHP client to authenticate
itself when connecting to Riak.

If you are using [trust-]({{<baseurl>}}riak/kv/2.2.2/using/security/managing-sources/#trust-based-authentication) or [PAM]({{<baseurl>}}riak/kv/2.2.2/using/security/managing-sources/#pam-based-authentication)-based authentication, you can use the
security setup described [below](#php-client-basics). [Certificate]({{<baseurl>}}riak/kv/2.2.2/using/security/managing-sources/#certificate-based-authentication)-based authentication is not
yet supported in the PHP client due to limitations of the HTTP interface of Riak.

## PHP Client Basics

When connecting to Riak using a PHP-based client, you typically do so
by instantiating separate `\Basho\Riak\Node` objects for each node in your
cluster and passing those `\Basho\Riak\Node` objects as an array to a
`\Basho\Riak` object as a dependency. In this document, we will be working with
only one node.

If you are using Riak security, _all_ connecting clients should have
access to the same Certificate Authority (CA) used on the server side,
regardless of which [security source]({{<baseurl>}}riak/kv/2.2.2/using/security/managing-sources/) you choose. All clients should also provide a username, regardless of
security source. The example below sets up a single node object (we'll
simply call it `node`) that connects to Riak on `localhost` and on port
8087 and specifies `riakuser` as a username. That object will be used to
create a Riak object. The setup below does not specify a CA and will throw
an `\Basho\Riak\Node\Builder\Exception`:

```php
use \Basho\Riak;
use \Basho\Riak\Node;

$node = (new Node\Builder())
    ->atHost('127.0.0.1')
    ->onPort('8087')
    ->usingPasswordAuthentication('riakuser')
    ->build();

// since we are using a single node, it needs to be wrapped in array brackets
$riak = new Riak([$node]);
```

This client object is not currently set up to use any of the available
security sources. This will change in the sections below.

## Password-based Authentication

To enable our client to use password-based auth, we can use most of the
setup from the example above, with the exception that we will specify a
password for the client in the `usingPasswordAuthentication` method in
the `node` object's builder rather than omitting it. We will also
pass the path of the CA file relative to the current working directory into
the `withCertificateAuthorityFile` method.

```php
use \Basho\Riak;
use \Basho\Riak\Node;

$node = (new Node\Builder())
    ->atHost('127.0.0.1')
    ->onPort('8087')
    ->usingPasswordAuthentication('riakuser', 'rosebud')
    ->withCertificateAuthorityFile(getcwd() . '/ssl_dir/cacertfile.pem')
    ->build();

// since we are using a single node, it needs to be wrapped in array brackets
$riak = new Riak([$node]);
```

## PAM- and Trust-based Authentication

If you are using PAM- or trust-based authentication, the only difference
from password-based authentication is that you do not need to specify a
password. There are helper methods that handle this for you, 
`usingPamAuthentication` and `usingTrustAuthentication`.

```php
use \Basho\Riak;
use \Basho\Riak\Node;

// PAM Example
$node = (new Node\Builder())
    ->atHost('127.0.0.1')
    ->onPort('8087')
    ->usingPamAuthentication('riakuser')
    ->withCertificateAuthorityFile(getcwd() . '/ssl_dir/cacertfile.pem')
    ->build();

// Trust Example
$node = (new Node\Builder())
    ->atHost('127.0.0.1')
    ->onPort('8087')
    ->usingTrustAuthentication('riakuser')
    ->withCertificateAuthorityFile(getcwd() . '/ssl_dir/cacertfile.pem')
    ->build();

// since we are using a single node, it needs to be wrapped in array brackets
$riak = new Riak([$node]);
```

## Certificate-based Authentication

Certificate-based authentication is not currently supported in the
official Riak PHP client due to limitations in the HTTP interface.
