---
title_supertext: "Conflict Resolution:"
title: "PHP"
description: ""
project: "riak_kv"
project_version: "2.1.1"
menu:
  riak_kv-2.1.1:
    name: "PHP"
    identifier: "usage_conflict_resolution_php"
    weight: 105
    parent: "usage_conflict_resolution"
toc: true
aliases:
  - /riak/2.1.1/dev/using/conflict-resolution/php
  - /riak/kv/2.1.1/dev/using/conflict-resolution/php
canonical_link: "https://docs.basho.com/riak/kv/latest/developing/usage/conflict-resolution/php"
---

For reasons explained in the [Introduction to conflict resolution](/riak/kv/2.1.1/developing/usage/conflict-resolution), we strongly recommend adopting a
conflict resolution strategy that requires applications to resolve
siblings according to use-case-specific criteria. Here, we'll provide a
brief guide to conflict resolution using the official [Riak PHP
client](https://github.com/basho/riak-php-client).

## How the PHP Client Handles Conflict Resolution

Every `\Basho\Riak\Object` command returns a `\Basho\Riak\Command\Object\Response`
object, which provides what is needed to handle object conflicts. If siblings exist
and have been returned from the server within the response body, they will be
available within the response object. See below:

```php
$response = (new \Basho\Riak\Command\Builder\FetchObject($riak))
    ->buildLocation('conflicted_key', 'bucket_name', 'bucket_type')
    ->build()
    ->execute();

echo $response->getStatusCode(); // 300
echo $response->hasSiblings(); // 1
echo $response->getSiblings(); // \Basho\Riak\Object[]
```

## Basic Conflict Resolution Example

Let's say that we're building a social network application and storing
lists of usernames representing each user's "friends" in the network.
Each user will bear the class `User`, which we'll create below. All of
the data for our application will be stored in buckets that bear the
[bucket type](/riak/kv/2.1.1/developing/usage/bucket-types) `siblings`, and for this bucket type
`allow_mult` is set to `true`, which means that Riak will generate
siblings in certain cases---siblings that our application will need to
be equipped to resolve when they arise.

The question that we need to ask ourselves now is this: if a given user
has sibling values, i.e. if there are multiple `friends` lists and Riak
can't decide which one is most causally recent, which list should be
deemed "correct" from the standpoint of the application? What criteria
should be applied in making that decision? Should the lists be merged?
Should we pick a `User` object at random?

This decision will always be yours to make. Here, though, we'll keep it
simple and say that the following criterion will hold: if conflicting
lists exist, _the longer list will be the one that our application deems
correct_. So if the user `user1234` has a sibling conflict where one
possible value has `friends` lists with 100, 75, and 10 friends,
respectively, the list of 100 friends will win out.  While this might
not make sense in real-world applications, it's a good jumping-off
point. We'll explore the drawbacks of this approach, as well as a better
alternative, in this document as well.

### Creating Our Data Class

We'll start by creating a `User` class for each user's data. Each `User`
object will consist of a `username` as well as a `friends` property that
lists the usernames, as strings, of the user's friends. We'll use a
`Set` for the `friends` property to avoid duplicates.

```php
class User {
    public $username;
    public $friends;

    public function __construct($username, array $friends = [])
    {
        $this->username = $username;
        $this->friends = $friends;
    }

    public function __toString()
    {
        return json_encode([
            'username' => $this->username,
            'friends' => $this->friends,
            'friends_count' => count($this->friends)
        ]);
    }
}
```

Here's an example of instantiating a new `User` object:

```php
$bashobunny = new User('bashobunny', ['fred', 'barney']);
```

### Implementing a Conflict Resolution Function

Let's say that we've stored a bunch of `User` objects in Riak and that a
few concurrent writes have led to siblings. How is our application going
to deal with that? First, let's say that there's a `User` object stored
in the bucket `users` (which is of the bucket type `siblings`, as
explained above) under the key `bashobunny`. We can fetch the object
that is stored there and see if it has siblings:

```php
$response = (new \Basho\Riak\Command\Builder\FetchObject($riak))
    ->buildLocation('bashobunny', 'users', 'siblings')
    ->build()
    ->execute();

echo $response->hasSiblings(); // 1
```

If we get `true`, then there are siblings. So what do we do in that
case? At this point, we need to write a function that resolves the list
of siblings, i.e. reduces the `$response->getSiblings()` array down to one member.
In our case, we need a function that takes a Riak response object as its argument,
applies some logic to the list of values contained in the `siblings` property
of the object, and returns a single value. For our example use case here, we'll
return the sibling with the longest `friends` list:

```php
use \Basho\Riak;
use \Basho\Riak\Command;

function longest_friends_list_resolver(Command\Object\Response $response)
{
    if ($response->hasSiblings()) {
        $siblings = $response->getSiblings();
        $max_key = 0;
        foreach ($siblings as $key => $sibling) {
            if ($sibling->getData()['friends_count'] > $siblings[$max_key]->getData()['friends_count']) {
                $max_key = $key;
            }
        }
    }

    return $siblings[$max_key];
}
```

We can then embed this function into a more general function for fetching 
objects from the users bucket:

```php
function fetch_user_by_username($username, Riak $riak)
{
    $response = (new Command\Builder\FetchObject($riak))
      ->buildLocation($username, 'users', 'siblings')
      ->build()
      ->execute();

    return longest_friends_list_resolver($response);
}

bashobunny = fetch_user_by_username('bashobunny', $riak);
```

Now, when a `User` object is fetched (assuming that the username acts as
a key for the object), a single value is returned for the `friends`
list. This means that our application can now use a "correct" value
instead of having to deal with multiple values.

## Conflict Resolution and Writes

In the above example, we created a conflict resolver that resolves a
list of discrepant `User` objects and returns a single `User`. It's
important to note, however, that this resolver will only provide the
application with a single "correct" value; it will _not_ write that
value back to Riak. That requires a separate step. When this step should
be undertaken depends on your application. In general, though, we
recommend writing objects to Riak only when the application is ready to
commit them, i.e. when all of the changes that need to be made to the
object have been made and the application is ready to persist the state
of the object in Riak.

Correspondingly, we recommend that updates to objects in Riak follow
these steps:

1. **Read** the object from Riak
2. **Resolving sibling conflicts** if they exist, allowing the
application to reason about one "correct" value for the object (this
step is the subject of this tutorial)
3. **Modify** the object
4. **Write** the object to Riak once the necessary changes have been
made

You can find more on writing objects to Riak, including examples from
the official PHP client library, in the [Developing with Riak KV: Usage](/riak/kv/2.1.1/developing/usage) section.

## More Advanced Example

Resolving sibling `User` values on the basis of which user has the longest
friends list has the benefit of being simple but it's probably not a
good resolution strategy for our social networking application because
it means that unwanted data loss is inevitable. If one friend list
contains `A`, `B`, and `C` and the other contains `D` and `E`, the list
containing `A`, `B`, and `C` will be chosen. So what about friends `D`
and `E`? Those usernames are essentially lost. In the sections below,
we'll implement an alternative strategy as an example.

### Merging the Lists

To avoid losing data like this, a better strategy would be to merge the
lists. We can modify our original resolver function to accomplish
precisely that and will also store the resulting `User` object.

The drawback to this approach is that it's more or less inevitable that a user
will remove a friend from their friends list, and then that friend will
end up back on the list during a conflict resolution operation. While
that's certainly not desirable, that is likely better than the
alternative proposed in the first example, which entails usernames being
simply dropped from friends lists. Sibling resolution strategies almost
always carry potential drawbacks of this sort.

## Riak Data Types

An important thing to always bear in mind when working with conflict
resolution is that Riak offers a variety of [Data Types](/riak/kv/2.1.1/developing/data-types/) that have
specific conflict resolution mechanics built in. If you have data that
can be modeled as a [counter](/riak/kv/2.1.1/developing/data-types/counters), [set](/riak/kv/2.1.1/developing/data-types/sets), or [map](/riak/kv/2.1.1/developing/data-types/maps), then you should seriously
consider using those Data Types instead of creating your own
application-side resolution logic.

In the example above, we were dealing with conflict resolution within a
set, in particular the `friends` list associated with each `User`
object. The merge operation that we built to handle conflict resolution
is analogous to the resolution logic that is built into Riak sets. For
more information on how you could potentially replace the client-side
resolution that we implemented above, see our [tutorial on Riak sets](/riak/kv/2.1.1/developing/data-types/sets).
