---
title_supertext: "Developing with Riak KV"
title: "Key/Value Modeling"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Key/Value Modeling"
    identifier: "developing_kv_model"
    weight: 104
    parent: "developing"
toc: true
aliases:
  - /riak/2.1.4/dev/data-modeling/key-value/
  - /riak/kv/2.1.4/dev/data-modeling/key-value/
canonical_link: "https://docs.basho.com/riak/kv/latest/developing/key-value-modeling"
---

While Riak enables you to take advantage of a wide variety of features
that can be useful in application development, such as [Search](/riak/kv/2.1.4/developing/usage/search), [secondary indexes (2i)](/riak/kv/2.1.4/developing/usage/secondary-indexes/), and [Riak Data Types](/riak/kv/2.1.4/developing/data-types/), Riak almost always performs best when you
build your application around basic CRUD operations (create, read,
update, and delete) on objects, i.e. when you use Riak as a "pure"
key/value store.

In this tutorial, we'll suggest some strategies for naming and modeling
for key/value object interactions with Riak. If you'd like to use some
of Riak's other features, we recommend checking out the documentation
for each of them or consulting our guide to [building applications with Riak](/riak/kv/2.1.4/developing/app-guide/) for a better sense of which features you might need.

## Advantages of Key/Value Operations

Riak's key/value architecture enables it to be more performant than
relational databases in many scenarios because Riak doesn't need to
perform lock, join, union, or other operations when working with
objects. Instead, it interacts with objects on a one-by-one basis, using
**primary key lookups**.

Primary key lookups store and fetch objects in Riak on the basis of
three basic locators:

* The object's [key](/riak/kv/2.1.4/learn/concepts/keys-and-objects#keys), which can be anything you
  want as long as it is [Unicode compliant](http://www.unicode.org/)
* The [bucket](/riak/kv/2.1.4/learn/concepts/buckets) which houses the object and its key (bucket
  names are also Unicode compliant)
* The [bucket type](/riak/kv/2.1.4/developing/usage/bucket-types) that determines the bucket's
  [replication](/riak/kv/2.1.4/developing/app-guide/replication-properties) and other properties

It may be useful to think of this system as analogous to a nested
key/value [hash](http://en.wikipedia.org/wiki/Hash_function) as you
would find in most programming languages. Below is an example from
[Ruby](http://www.ruby-doc.org/core-2.1.2/Hash.html). The hash
`simpsons` contains keys for all of the available seasons, while each
key houses a hash for each episode of that season:

```ruby
simpsons = {
  'season 1': {
    { 'episode 1': 'Simpsons Roasting on an Open Fire' },
    { 'episode 2': 'Bart the Genius' },
    # ...
  },
  'season 2': {
    { 'episode 1': 'Bart Gets an "F"' },
    # ...
  },
  # ...
}
```

If we want to find out the title of an episode, we can retrieve it based
on hash keys:

```ruby
simpsons['season 4']['episode 12']

# => "Marge vs. the Monorail"
```

Storing data in Riak is a lot like this. Let's say that we want to store
JSON objects with a variety of information about every episode of the
Simpsons. We could store each season in its own bucket and each episode
in its own key within that bucket. Here's what the URL structure would
look like (for the [HTTP API](/riak/kv/2.1.4/developing/api/http)):

```
GET/PUT/DELETE /bucket/<season>/keys/<episode number>
```

The most important benefit of sorting Riak objects this way is that
these types of lookup operations are extremely fast. Riak doesn't need
to search through columns or tables to find an object. If it knows the
bucket/key "address" of the object, so to speak, it can locate that
object just about as quickly with billions of objects in a cluster as
when the cluster holds only a handful of objects.

## Overcoming the Limitations of Key/Value Operations

Using any key/value store can be tricky at first, especially if you're
used to relational databases. The central difficulty is that your
application cannot run arbitrary selection queries like `SELECT * FROM
table`, and so it needs to know where to look for objects in advance.

One of the best ways to enable applications to discover objects in Riak
more easily is to provide **structured bucket and key names** for
objects. This approach often involves wrapping information about the
object _in the object's location data itself_.

Here are some example sources for bucket or key names:

* Timestamps, e.g. `2013-11-05T08:15:30-05:00`
* [UUID](http://en.wikipedia.org/wiki/Universally_unique_identifier)s,
  e.g. `9b1899b5-eb8c-47e4-83c9-2c62f0300596`
* Geographical coordinates, e.g. `40.172N-21.273E`

We could use these markers by themselves or in combination with other
markers. For example, sensor data keys could be prefaced by `sensor_` or
`temp_sensor1_` followed by a timestamp (e.g.
`sensor1_2013-11-05T08:15:30-05:00`), or user data keys could be
prefaced with `user_` followed by a UUID (e.g.
`user_9b1899b5-eb8c-47e4-83c9-2c62f0300596`).

Any of the above suggestions could apply to bucket names as well as key
names. If you were building Twitter using Riak, for example, you could
store tweets from each user in a different bucket and then construct key
names using a combination of the prefix `tweet_` and then a timestamp.
In that case, all the tweets from the user BashoWhisperer123 could be
housed in a bucket named `BashoWhisperer123`, and keys for tweets would
look like `tweet_<timestamp>`.

The possibilities are essentially endless and, as always, defined by the
use case at hand.

## Object Discovery with Riak Sets

Let's say that we've created a solid bucket/key naming scheme for a user
information store that enables your application to easily fetch user
records, which are all stored in the bucket `users` with each user's
username acting as the key. The problem at this point is this: how can
Riak know which user records actually exist?

One way to determine this is to [list all keys](/riak/kv/2.1.4/developing/api/protocol-buffers/list-keys) in the
bucket `users`. This approach, however, is _not_ recommended, because
listing all keys in a bucket is a very expensive operation that should
not be used in production. And so another strategy must be employed.

A better possibility is to use [Riak sets](/riak/kv/2.1.4/developing/data-types/sets) to
store lists of keys in a bucket. Riak sets are a [Riak Data Type](/riak/kv/2.1.4/developing/data-types) that enable you to store lists of binaries or strings in Riak.
Unlike normal Riak objects, you can interact with Riak sets much like
you interact with sets in most programming languages, i.e. you can add
and remove elements at will.

Going back to our user data example, instead of simply storing user
records in our `users` bucket, we could set up our application to store
each key in a set when a new record is created. We'll store this set in
the bucket `user_info_sets` (we'll keep it simple) and in the key
`usernames`. The following will also assume that we've [set up a bucket type](/riak/kv/2.1.4/developing/data-types/#setting-up-buckets-to-use-riak-data-types) called
`sets`.

We can interact with that set on the basis of its location:

```java
Location userIdSet = new Location(new Namespace("sets", "user_info_sets"), "usernames");

// With this Location, we can construct fetch operations like this:
FetchSet fetchUserIdSet = new FetchSet.Builder(userIdSet).build();
```

```ruby
require 'riak'

set_bucket = client.bucket('user_info_sets')

# We'll make this set global because we'll use it
# inside of a function later on

$user_id_set = Riak::Crdt::Set.new(set_bucket, 'usernames', 'sets')
```

```php
$command = (new \Basho\Riak\Command\Builder\FetchSet($riak))
    ->buildLocation('usernames', 'user_info_sets', 'sets')
    ->build();
```

```python
from riak.datatypes import Set

bucket = client.bucket_type('sets').bucket('user_info_sets')
user_id_set = Set(bucket, 'usernames')
```

> **Getting started with Riak clients**
>
> If you are connecting to Riak using one of Basho's official [client libraries](/riak/kv/2.1.4/developing/client-libraries), you can find more information about getting started with your client in [Developing with Riak KV: Getting Started](/riak/kv/2.1.4/developing/getting-started).

Then, we can create a function that stores a user record's key in that
set every time a record is created:

```java
// A User class for constructing user records
class User {
  public String username;
  public String info;

  public User(String username, String info) {
    this.username = username;
    this.info = info;
  }
}

// A function for storing a user record that has been created
public void storeUserRecord(User user) throws Exception {
  // User records themselves will be stored in the bucket "users"
  Location userObjectLocation =
    new Location(new Namespace("users"), user.username);
  RiakObject userObject = new RiakObject()
      // We'll keep it simple and store User object data as plain text
      .setContentType("text/plain")
      .setValue(user.info);
  StoreValue store = new StoreValue.Builder(userObjectLocation, userObject)
      .build();
  client.execute(store);

  Location userIdSet =
    new Location(new Namespace("sets", "user_info_sets"), "usernames");
  SetUpdate su = new SetUpdate()
      .add(BinaryValue.create(user.username));
  UpdateSet update = new UpdateSet.Builder(su, update)
      .build();
  client.execute(update);
}
```

```ruby
class User
  attr_accessor :username, :info
end

def store_record(user)
  # First we create an empty object and specify its bucket and key
  obj = Riak::RObject.new(client.bucket('users'), user.username)

  # We'll keep it simple by storing plain text for each user's info
  obj.content_type = 'text/plain'
  obj.raw_data = user.info
  obj.store

  # Finally, we'll add the user's username to the set
  user_id_set.add(user.username)
end
```

```php
class User
{
  public $user_name;
  public $info;

  public function __construct($user_name, $info)
  {
    $this->user_name = $user_name;
    $this->info = $info;
  }
}

function store_user(User $user)
{
  (new \Basho\Riak\Command\Builder\StoreObject)
    ->buildLocation($user->user_name, 'users')
    ->buildJsonObject($user)
    ->build()
    ->execute();

  (new \Basho\Riak\Command\Builder\UpdateSet)
    ->buildLocation('usernames', 'user_info_sets', 'sets')
    ->add($user->user_name)
    ->build()
    ->execute();
}
```

```python
class User:
    def __init__(self, username, info):
        this.username = username
        this.info = info

# Using the "user_id_set" object from above
def store_record(user):
  # First we create an empty object and specify its bucket and key
    obj = RiakObject(client, 'users', user.username)

    # We'll keep it simple by storing plain text for each user's info
    obj.content_type = 'text/plain'
    obj.data = user.info
    obj.store()

    # Finally, we'll add the user's username to the set
    user_id_set.add(username)
    user_id_set.store()
```

Now, let's say that we want to be able to pull up all user records in
the bucket at once. We could do so by iterating through the usernames
stored in our set and then fetching the object corresponding to each
username:

```java
public Set<User> fetchAllUserRecords() {
    // Empty builder sets for usernames and User objects
    Set<String> userIdSet = new HashSet<String>();
    Set<User> userSet = new HashSet<User>();

    // Turn the Riak username set into a set of Strings
    Location userIdSet =
        new Location(new Namespace("sets", "sets"), "usernames");
    FetchSet fetchUserIdSet = new FetchSet.Builder(userIdSet).build();
    RiakSet set = client.execute(fetchUserIdSet).getDatatype();
    set.viewAsSet().forEach((BinaryValue username) -> {
        userIdSet.add(username.toString());
    });

    // Fetch User objects for each of the usernames stored in the set
    userIdSet.forEach((String username) -> {
        Location userLocation = new Location(new Namespace("users"), username);
        FetchValue fetch = new FetchValue.Builder(userLocation).build();
        User user = client.execute(fetch).getValue(User.class);
        userSet.add(user);
    });
    return userSet;
}
```

```ruby
# Using the "user_id_set" set from above

def fetch_all_user_records
  users_bucket = $client.bucket('users')
  user_records = Array.new
  $user_id_set.members.each do |user_id|
    user_record = users_bucket.get(user_id).data
    user_records.push(user_record)
  end
  user_records
end
```

```php
function fetch_users()
{
  $users = [];

  $response = (new \Basho\Riak\Command\Builder\UpdateSet)
    ->buildLocation('usernames', 'user_info_sets', 'sets')
    ->build()
    ->execute();

  $user_names = $response->getSet()->getData();
  foreach($user_names as $user_name) {
    $response = (new \Basho\Riak\Command\Builder\FetchObject)
      ->buildLocation($user_name, 'users')
      ->build()
      ->execute();

    $users[$user_name] = $response->getObject()->getData();
  }

  return $users;
}
```

```python
# We'll create a generator object that will yield a list of Riak objects
def fetch_all_user_records():
  users_bucket = client.bucket('users')
    user_id_list = list(user_id_set.reload().value)
    for user_id in user_id_list:
      yield users_bucket.get(user_id)

# We can retrieve that list of Riak objects later on
list(fetch_all_user_records())
```

## Naming and Object Verification

Another advantage of structured naming is that you can prevent queries
for objects that don't exist or that don't conform to how your
application has named them. For example, you could store all user data
in the bucket `users` with keys beginning with the fragment `user_`
followed by a username, e.g. `user_coderoshi` or `user_macintux`. If an
object with an inappropriate key is stored in that bucket, it won't even
be seen by your application because it will only ever query keys that
begin with `user_`:

```java
// Assuming that we've created a class User:

public User getUserByUsername(String username) {
    String usernameKey = String.format("user_%s", username)
    Location loc = new Location("users")
            .setKey(usernameKey);
    FetchValue fetchUser = new FetchValue.Builder(loc).build();
    FetchValue.Response res = client.execute(fetchUser);
    User userObject = res.getValue(User.class);
    return userObject;
}
```

```ruby
def get_user_by_username(username)
  bucket = client.bucket('users')
  obj = bucket.get('user_#{username}')
  return obj.raw_data
end
```

```php
function fetchUser($user_name)
{
    $response = (new \Basho\Riak\Command\Builder\FetchObject)
      ->buildLocation($user_name, 'users')
      ->build()
      ->execute();

    return $response->getObject()->getData();
}
```

```python
def get_user_by_username(username):
  bucket = client.bucket('users')
  obj = bucket.get('user_{}'.format(username))
  return obj.data
```

## Bucket Types as Additional Namespaces

Riak [bucket types](/riak/kv/2.1.4/developing/usage/bucket-types) have two essential functions:
they enable you to manage [bucket configurations](/riak/kv/2.1.4/learn/concepts/buckets) in an
efficient and streamlined way and, more importantly for our purposes
here, they act as a third namespace in Riak in addition to buckets and
keys. Thus, in Riak versions 2.0 and later you have access to a third
layer of information for locating objects if you wish.

While bucket types are typically used to assign different bucket
properties to groups of buckets, you can also create named bucket types
that simply extend Riak's [defaults](/riak/kv/2.1.4/developing/usage/bucket-types/#bucket-types-as-namespaces) or multiple bucket types that have
the same configuration but have different names.

Here's an example of creating four bucket types that only extend Riak's
defaults:

```bash
riak-admin bucket-type create john
riak-admin bucket-type create robert
riak-admin bucket-type create jimmy
riak-admin bucket-type create john-paul
```

Or you can create five different bucket types that all set `n_val` to 2
but have different names:

```bash
riak-admin bucket-type create earth '{"props":{"n_val":2}}'
riak-admin bucket-type create fire '{"props":{"n_val":2}}'
riak-admin bucket-type create wind '{"props":{"n_val":2}}'
riak-admin bucket-type create water '{"props":{"n_val":2}}'
riak-admin bucket-type create heart '{"props":{"n_val":2}}'
```

### Bucket Types Example

To extend our Simpsons example from above, imagine that we become
dissatisfied with our storage scheme because we want to separate the
seasons into good seasons and bad seasons (we'll leave it up to you to
make that determination).

One way to improve our scheme might be to change our bucket naming
system and preface each bucket name with `good` or `bad`, but a more
elegant way would be to use bucket types instead. So instead of this URL
structure...

```
GET/PUT/DELETE /bucket/<season>/keys/<episode number>
```

...we can use this structure:

```
GET/PUT/DELETE /types/<good or bad>/buckets/<season>/keys/<episode number>
```

That adds an additional layer of namespacing and enables us to think
about our data in terms of a deeper hash than in the example above:

```ruby
simpsons = {
  'good': {
    'season X': {
      { 'episode 1': '<title>' },
      # ...
    }
  },
  'bad': {
    'season Y': {
      { 'episode 1': '<title>' },
      # ...
    }
  }
}
```

We can fetch the title of season 8, episode 6:

```ruby
# For the sake of example, we'll classify season 8 as good:

simpsons['good']['season 8']['episode 6']

# => "A Milhouse Divided"
```

If your data is best modeled as a three-layered hash, you may want to
consider using bucket types in the way shown above.

## Resources

More on key/value modeling in Riak can be found in [this
presentation](http://www.youtube.com/watch?v=-_3Us7Ystyg#aid=P-4heI_bFwo)
by Basho evangelist [Hector Castro](https://github.com/hectcastro), with
the presentation slides available [on Speaker
Deck](https://speakerdeck.com/hectcastro/throw-some-keys-on-it-data-modeling-for-key-value-data-stores-by-example).
