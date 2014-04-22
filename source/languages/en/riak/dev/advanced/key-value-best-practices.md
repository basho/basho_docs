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

The best way to achieve this is to provide **structured bucket and/or key names** for objects that make them easily discoverable. Here are some example sources for bucket and key names:

* Timestamps, e.g. `2013-11-05T08:15:30-05:00`
* [UUID](http://en.wikipedia.org/wiki/Universally_unique_identifier)s, e.g. `9b1899b5-eb8c-47e4-83c9-2c62f0300596`
* Geographical coordinates, e.g. `40.172N-21.273E`


More in [this video](http://www.youtube.com/watch?v=-_3Us7Ystyg#aid=P-4heI_bFwo), with the presentation slides available [on Speaker Deck](https://speakerdeck.com/hectcastro/throw-some-keys-on-it-data-modeling-for-key-value-data-stores-by-example)

## Example

Possible sources for "natural" keys:



#### Combination

For example, the name of the data type in your application, plus an identifier: `user_17711`. If you're using Riak as a CMS and you want to manage multiple site domains, you could create a separate bucket for each, e.g. `main-site`, `login-sites`, and `user-homepages`, and store `user_<USER_ID>` keys in each.

The problem that emerges here, though, is that 

## Riak Data Types

[[Riak Data Types|Using Data Types]] are special in Riak 

You can use [[sets|Data Types#sets]] to keep track of which user keys are in the bucket

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
