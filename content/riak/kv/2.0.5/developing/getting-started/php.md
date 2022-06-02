---
title: "Getting Started with PHP"
description: ""
project: "riak_kv"
project_version: "2.0.5"
menu:
  riak_kv-2.0.5:
    name: "PHP"
    identifier: "getting_started_php"
    weight: 107
    parent: "developing_getting_started"
toc: true
aliases:
  - /riak/2.0.5/dev/taste-of-riak/php
  - /riak/kv/2.0.5/dev/taste-of-riak/php
---

If you haven't set up a Riak Node and started it, please visit [Running A Cluster]({{<baseurl>}}riak/kv/2.0.5/using/running-a-cluster) first.

To try this flavor of Riak, a working installation of PHP is required, and [Composer](https://getcomposer.org/) is required to be installed to fetch the client library package. 

## Client Setup
Download and unzip, or clone the Taste of Riak Sample Code Repository from GitHub ([zip](https://github.com/basho/taste-of-riak/archive/master.zip), [github repository](https://github.com/basho/taste-of-riak)).

From the `taste-of-riak` directory, use composer to install the Riak PHP 2.0 Client`.

```bash
php path/to/your/composer.phar install

# If you did a global install of composer, run this instead:
composer install
```

If you set up a local Riak cluster using the [[five minute install]] method, change line 11 from `->onPort(8098)` to `->onPort(10018)`.

Next, run `php Ch01-CRUD/taste-of-riak.php` to run this chapter's example code. It should output:

```json
Reading Objects From Riak...
Updating Objects In Riak...
Deleting Objects From Riak...
Working With Complex Objects...
Serialized Object:
{"title":"Moby Dick","author":"Herman Melville","body":"Call me Ishmael. Some years ago...","isbn":"1111979723","copiesOwned":3}
```

Yay, success!

Since we didn't use PHP's REPL environment, let's walk through the code
to see what it actually did at each step.

## Setting up the PHP Client and connections

```php
include_once 'vendor/autoload.php';

use Basho\Riak;
use Basho\Riak\Node;
use Basho\Riak\Command;

$node = (new Node\Builder)
        ->atHost('127.0.0.1')
        ->onPort(8098)
        ->build();

$riak = new Riak([$node]);
```

This code will load the library, declare the necessary `use` statements for our code, and then initialize and configure a [Node Builder](http://basho.github.io/riak-php-client/class-Basho.Riak.Node.Builder.html).
Once we call `build()` on the builder, it will return to us a [Node](http://basho.github.io/riak-php-client/class-Basho.Riak.Node.html) object, which we use when building our Riak commands. 

We are now ready to start interacting with Riak.

## Next Steps

[CRUD Operations]({{<baseurl>}}riak/kv/2.0.5/developing/getting-started/php/crud-operations)
