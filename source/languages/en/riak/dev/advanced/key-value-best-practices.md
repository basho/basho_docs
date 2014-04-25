---
title: Key/Value Best Practices
project: riak
version: 2.0.0+
document: guide
audience: intermediate
keywords: [keys, values, data-types]
---

While Riak enables you to take advantage of a wide variety of features that can be useful in application development, such as [[Search|Using Search]], [[secondary indexes (2i)|Using Secondary Indexes]], and [[Riak Data Types|Using Data Types]], Riak almost always performs best when you limit your application to basic CRUD operations (create, read, update, and delete) on objects, i.e. when you use Riak as a "pure" key/value store.

## Advantages of Key/Value Operations

Riak's key/value architecture enables it to be more performant than relational databases in most scenarios because there are no lock, join, union, or other operations that need to be performed when working with objects. Objects are
stored as opaque binaries, i.e. Riak doesn't usually care about their content type, and are stored on the basis of three locators:

* The object's [[key|Keys and Objects#keys]], which can anything you want and are [Unicode compliant](http://www.unicode.org/)
* The [[bucket|Buckets]] which houses the object and its key
* The [[bucket type|Using Bucket Types]] that determines the bucket's [[replication|Replication Properties]] and other properties

The most important benefit of this setup is that basic lookup operations are extremely fast. Riak doesn't need to search through columns or tables to find an object. Instead, it stores them on the basis of a concrete "address," so to speak, and when given an explicit address, Riak can locate an object just about as quickly with billions of keys in a cluster as when there are only a handful of keys.

## Overcoming the Limitations of Key/Value Operations

Working with a key/value store can be tricky at first, especially if you're used to relational databases. The central difficulty is that **your application already needs to know where to look** when it needs to find an object. It can't run a `SELECT * FROM table`-style operation that locates objects according to criteria. It has to know the object's location in advance.

The best way to achieve this is to provide **structured bucket and/or key names** for objects that make them more easily discoverable. Here are some example sources for bucket or key names:

* Timestamps, e.g. `2013-11-05T08:15:30-05:00`
* [UUID](http://en.wikipedia.org/wiki/Universally_unique_identifier)s, e.g. `9b1899b5-eb8c-47e4-83c9-2c62f0300596`
* Geographical coordinates, e.g. `40.172N-21.273E`

You could use these names by themselves or in combination with other markers. Sensor data keys could be prefaced by `sensor_` or `temp_sensor1_` followed by timestamp (e.g. ). User data keys could be prefaced with `user_` followed by a UUID (e.g. `user_9b1899b5-eb8c-47e4-83c9-2c62f0300596`).

Any of the above suggestions could apply to bucket names as well as key names. If you were building Twitter using Riak, for example, you could store tweets from each user in a different bucket and then construct key names using a combination of the prefix `tweet_` and then a timestamp. In that case, all the tweets from the user ``

The possibilities are essentially endless and, as always, defined by the use case at hand. The most important thing is to ensure consistency. If you structure key or bucket names a certain way, your application may not be able to access that information later.

For example, if you've been storing sensor data in a bucket with keys starting with `sensor1_` and you store an object in that bucket with the key `a1b2c3d4`, that object may be invisible to your application. The only way to see _all_ buckets available in a cluster is to run a [[list buckets|HTTP List Buckets]] operation, and the only way to see all keys available in a bucket is through a [[list keys|HTTP List Keys]] operation. Both of these operations, however, are extremely expensive, and we do not recommend relying on them in production environments.

## Knowing How to Find Keys

If you've developed a use case-specific naming strategy for keys, this often leaves an application with the following question: how do I know which keys are in a bucket? How do I knew which keys in a bucket correspond to a certain naming scheme? How can I determine

There are two Riak features that can assist you with this problem, each with pros and cons: [[secondary indexes|Using Secondary Indexes]] and [[Riak Data Types|Using Data Types]].

#### Object Discovery with Secondary Indexes

If you are using the [[LevelDB]] storage backend for Riak, you can attach metadata to objects that enable you to find them more easily later. 

#### Object Discovery with Riak Sets

[[Sets|Using Data Types#sets]] are essentially lists of binaries that you can store in Riak a bucket/key location. While Riak sets have many possible uses, they can be useful for storing lists of keys (or lists of bucket names, for that matter).

Let's say that you're developing a human resources application that stores employee data for several companies. Each employee's data is stored in keys labeled `emp_` followed by the employee's UUID, e.g. `emp_1a2b3c4d5e`. Each company has its own storage bucket, corresponding to the company's name, e.g. `acme`.

If we wanted to know which employee data objects are stored in a given bucket, we could keep a list of those keys in a Riak set. We could store that set in a bucket called `employee_lists`

Notes
=====

Non-relational data modeling is a completely different kind of enterprise
Problem of schemas; resolution needs to be done on the client side
Retrieving data can't be ad hoc; it can in a limited sense, i.e. using 2i, but not in a granular sense; and if you try too hard, you will pay a performance penalty
NoSQL => write-heavy workloads, scalability (no penalty as number of objects grows)
Best way to think of KV stores is as a data hash:

```ruby
bucket[key]

luc_perkins = client['people']['lucperkins']
```

Basic query structure:

```
/types/<type>/buckets/<bucket>/keys/<key>
```

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

## The Hash Analogy

Most modern programming languages supported data structures called [hashes](http://en.wikipedia.org/wiki/Hash_table) (in some languages they're known as maps or dicts or hashmaps). A useful way to think about key/value operations in Riak is to imagine object lookup as fetching values in a hash. 

## Resources

More on key/value modeling in Riak can be found in [this presentation](http://www.youtube.com/watch?v=-_3Us7Ystyg#aid=P-4heI_bFwo) by Basho evangelist [Hector Castro](https://github.com/hectcastro) with the presentation slides available [on Speaker Deck](https://speakerdeck.com/hectcastro/throw-some-keys-on-it-data-modeling-for-key-value-data-stores-by-example).
