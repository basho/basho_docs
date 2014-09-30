---
title: "Conflict Resolution: Ruby"
project: riak
version: 2.0.0+
document: tutorials
audience: intermediate
keywords: [developers, conflict-resolution, ruby]
---

For reasons explained in the [[introduction to conflict
resolution|Conflict Resolution]], we strongly recommend adopting a
conflict resolution strategy that requires applications to resolve
siblings according to use-case-specific criteria. Here, we'll provide a
brief guide to conflict resolution using the official [Riak Ruby
client](https://github.com/basho/riak-ruby-client).

## How the Ruby Client Handles Conflict Resolution

In the official Ruby client, every object of the class `Riak::RObject`
has a `siblings` property that provides access to a list of an object's
sibling values. If there are no siblings, that property will return an
array with only one item. Here's an example of an object with siblings:

```ruby
bucket = client.bucket('seahawks')
obj = bucket.get('coach')
obj.siblings

# The output:
[#<Riak::RContent [content/type]: "object value">, #<Riak::RContent [content/type]: "other object value">]
```

So what happens if the length of `obj.siblings` is greater than 1, as in
the case above? The easiest way to resolve siblings is to create a
conflict-resolving function that takes a list of sibling values and
returns a single value. An example is provided in the section below.

## Basic Conflict Resolution Example

Let's say that we're building a social network application and storing
lists of usernames representing each user's "friends." Each user will
bear the class `User`, which we'll create below. All of the data for our
application will be stored in the buckets that bear the [[bucket
type|Using Bucket Types]] `siblings`, and for this bucket type
`allow_mult` is set to `true`, which means that Rial will generate
siblings in certain cases---siblings that our application will need to
be equipped to resolve when necessary.

The question that we need to ask ourselves at this point is the
following: if a given user has conflict lists, which list should be
deemed more "correct?" What criteria should be applied? Should the lists
be merged? Should we pick a list at random and deem that correct? We'll
keep it simple here and say that the following criterion will hold: if
multiple conflict lists exist, _the longer list will be the one that our
application deems correct_. While this might not make sense in
real-world applications, it's a good jumping-off point.

### Creating Our Data Class

We'll start by creating a `User` class for each user's data. Each `User`
object will consist of a `friends` property that lists the usernames, as
strings, of the user's friends. We will also create a `to_json` method,
as we'll be storing each `User` object as JSON:

```ruby
class User
  def initialize(friends)
    @friends = friends
  end

  def to_json
    return { :friends => @friends }
  end
end
```

Now, we can create `User` objects and see what they look like as JSON:

```python
new_user = User.new(['captheorem238', 'siblingsrule572'])

new_user.to_json
# {'friends': ['captheorem238', 'siblingsrule572']}
```

### Implementing a Conflict Resolution Function

Let's say that we've stored a bunch of `User` objects in Riak and that a
few concurrent writes have led to siblings. How is our application going
to deal with that? First, let's say that there's a `User` object stored
in the bucket `users` (which is of the bucket type `siblings`, as
explained above) under the key `bashobunny`. We can fetch the object
that is stored there and see if it has siblings:

```ruby
bucket = client.bucket('users')
obj = bucket.get('bashobunny', type: 'siblings')
```
