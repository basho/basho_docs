---
title: "Conflict Resolution: PHP"
project: riak
version: 2.0.0+
document: tutorials
audience: intermediate
keywords: [developers, conflict-resolution, php]
---

For reasons explained in the [[introduction to conflict
resolution|Conflict Resolution]], we strongly recommend adopting a
conflict resolution strategy that requires applications to resolve
siblings according to use-case-specific criteria. Here, we'll provide a
brief guide to conflict resolution using the official [Riak PHP
client](https://github.com/basho/riak-php-client).

## How the PHP Client Handles Conflict Resolution

In the official PHP client, every Riak object has a `getSiblings()`
method that provides access to an array of that object's sibling values.
If there are no siblings, that property will return an array with only
one item. Here's an example of an object with siblings:

```php
$bucket = $client->bucket('seahawks');
$obj = $bucket->get('coach');
print $obj->siblings();

// The output:
TODO
```

So what happens if the length of `$obj->siblings` is greater than 1, as
in the case above? In order to resolve siblings, you need to replace the
multi-item `siblings` array with a single-item array. An example is
provided in the section below.

## PHP
