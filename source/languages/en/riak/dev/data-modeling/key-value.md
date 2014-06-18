---
title: Key/Value Modeling
project: riak
version: 2.0.0+
document: guide
audience: intermediate
keywords: [keys, values, data-types]
---

While Riak enables you to take advantage of a wide variety of features that can be useful in application development, such as [[Search|Using Search]], [[secondary indexes (2i)|Using Secondary Indexes]], and [[Riak Data Types|Using Data Types]], Riak almost always performs best when you limit your application to basic CRUD operations (create, read, update, and delete) on objects, i.e. when you use Riak as a "pure" key/value store.

If you'd like to use those features, we recommend checking out the documentation for each of them. In this tutorial, we'll suggest some strategies for object naming and modeling for "pure" key/vsalue interactions with Riak.

## Advantages of Key/Value Operations

Riak's key/value architecture enables it to be more performant than relational databases in many scenarios because Riak doesn't need to perform any lock, join, union, or other operations when working with objects. Instead, Riak object are stored as in Riak on the basis of three locators:

* The object's [[key|Keys and Objects#keys]], which can be anything you want as long as it is [Unicode compliant](http://www.unicode.org/)
* The [[bucket|Buckets]] which houses the object and its key (bucket names are also Unicode compliant)
* The [[bucket type|Using Bucket Types]] that determines the bucket's [[replication|Replication Properties]] and other properties

The most important benefit of this setup is that basic lookup operations are extremely fast. Riak doesn't need to search through columns or tables to find an object. Instead, it stores them on the basis of a concrete "address," and when given an explicit address, Riak can locate an object just about as quickly with billions of keys in a cluster as when there are only a handful of keys.

## Overcoming the Limitations of Key/Value Operations

Using any key/value store can be tricky at first, especially if you're used to relational databases. The central difficulty is that your application cannot run arbitrary selection queries and thus it needs to know where to look for objects in advance.

One of the best ways to enable applications to discover objects in Riak more easily is to provide **structured bucket and key names** for objects. This approach often involves wrapping information about the object _in the object's location data_.

Here are some example sources for bucket or key names:

* Timestamps, e.g. `2013-11-05T08:15:30-05:00`
* [UUID](http://en.wikipedia.org/wiki/Universally_unique_identifier)s, e.g. `9b1899b5-eb8c-47e4-83c9-2c62f0300596`
* Geographical coordinates, e.g. `40.172N-21.273E`

You could use these markers by themselves or in combination with other markers. For example, sensor data keys could be prefaced by `sensor_` or `temp_sensor1_` followed by a timestamp (e.g. `sensor1_2013-11-05T08:15:30-05:00`), or user data keys could be prefaced with `user_` followed by a UUID (e.g. `user_9b1899b5-eb8c-47e4-83c9-2c62f0300596`).

Any of the above suggestions could apply to bucket names as well as key names. If you were building Twitter using Riak, for example, you could store tweets from each user in a different bucket and then construct key names using a combination of the prefix `tweet_` and then a timestamp. In that case, all the tweets from the user BashoWhisperer123 could be housed in a bucket named `BashoWhisperer123`, and keys for tweets would look like `tweet_<timestamp>`.

The possibilities are essentially endless and, as always, defined by the use case at hand.

## Object Discovery with Riak Sets

Let's say that you've created a solid bucket/key naming scheme for a user information store that enables your application. User records are stored in the bucket `users` with each user's username acting as the key. The problem at this point is this: how do we know which user records actually exist?

One way to determine this is to [[list all keys|PBC List Keys]] in the bucket `users`. This approach, however, is _not_ recommended, because listing all keys in a bucket is a very expensive operation that should not be used in production. And so another strategy must be employed.

One possibility is to use Riak [[Sets|Using Data Types#sets]] to store lists of keys in a bucket. [[Riak sets|Using Data Types#sets]] are a [[Riak Data Type|Data Types]] that enable you to store lists of binaries in Riak. Unlike normal Riak objects, you can interact with Riak sets much like you interact with sets in most programming languages, i.e. you can add and remove elements.

Going back to our user data example, instead of simply storing user records in our `users` bucket, we could set up our application to store each user record key in a set when a new record is created. We'll store our set in the bucket `sets` (we'll keep it simple) and in the key `usernames`.

We can interact with that set on the basis of its location:

```python
from riak.datatypes import Set

bucket = client.bucket_type('sets').bucket('sets')
set = Set(bucket, 'usernames')
```

Then, we can create a function that stores a user record's key in that set every time a record is created:

```python
# using the "set" object from above

class User:
    def __init__(self, username, info):
        this.username = username
        this.info = info

def store_record(user):
    obj = RiakObject(client, 'users', user.username)
    obj.content_type = 'text/plain'
    obj.data = user.info
    obj.store()
    set.add(username)
    set.store()
```

Now, let's say that we want to be able to pull up all user records in the bucket at once. We could do so by iterating through the usernames stored in our set and then fetching the object corresponding to each username:

```python
# We'll create a generator object that will yield a list of Riak objects
def fetch_all_user_records():
	users_bucket = client.bucket('users')
    user_id_list = list(set.reload().value)
    for user_id in user_id_list:
    	yield users_bucket.get(user_id)

# We can then retrieve that list of Riak objects at any point:
list(fetch_all_user_records())
```

## Naming and Object Verification

Means of verification can be contained in the key itself. For example, you could store all user data in the bucket `users` with keys beginning with the fragment `user_` followed by a username. For example `user_coderoshi` or `user_macintux`. If an object with an inappropriate key is stored in that bucket, it won't even be seen by your application because it will only ever query keys that begin with `user_`:

```ruby
def get_user_by_username(username)
  bucket = client.bucket('users')
  obj = bucket.get('user_#{username}')
  return obj
end
```

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

```python
def get_user_by_username(username):
  bucket = client.bucket('users')
  obj = bucket.get('user_{}'.format(username))
  return obj
```

## Bucket Types as Additional Namespaces

Riak [[bucket types|Using Bucket Types]] have two essential functions: they enable you to manage [[bucket configurations|Buckets]] in an efficient and streamlined way and, more importantly for our purposes here, they act as a third namespace in Riak in addition to buckets and keys. Thus, in Riak versions 2.0 and later you have access to a third layer of information for locating objects if you wish.

While bucket types are typically used to assign different bucket properties to groups of buckets, you can also create named bucket types that simply extend Riak's [[defaults|Using Bucket Types#bucket-types-as-namespaces]] or multiple bucket types that have the same configuration but have different names.

Here's an example of creating four bucket types that only extend Riak's defaults:

```bash
riak-admin bucket-type create john
riak-admin bucket-type create robert
riak-admin bucket-type create jimmy
riak-admin bucket-type create john-paul
```

Or you can create five different bucket types that all set `n_val` to 2 but have different names:

```bash
riak-admin bucket-type create earth '{"props":{"n_val":2}}'
riak-admin bucket-type create fire '{"props":{"n_val":2}}'
riak-admin bucket-type create wind '{"props":{"n_val":2}}'
riak-admin bucket-type create water '{"props":{"n_val":2}}'
riak-admin bucket-type create heart '{"props":{"n_val":2}}'
```

## Resources

More on key/value modeling in Riak can be found in [this presentation](http://www.youtube.com/watch?v=-_3Us7Ystyg#aid=P-4heI_bFwo) by Basho evangelist [Hector Castro](https://github.com/hectcastro),s with the presentation slides available [on Speaker Deck](https://speakerdeck.com/hectcastro/throw-some-keys-on-it-data-modeling-for-key-value-data-stores-by-example).
