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
p obj.siblings.length > 1
```

If we get `true`, then there are siblings. So what do we do in that
case? At this point, we need to write a function that resolves the list
of siblings, i.e. the `obj.siblings` array down to one. In our case, we
need a function that takes a single Riak object (or `RObject` in the
Ruby client) as its argument, applies some logic to the list of values
contained in the `siblings` property of the object, and returns a single
`RObject` in which `siblings` is an array of only one value. For our
example use case here, we'll return the sibling with the longest
`friends` list:

```ruby
def longest_friends_list_resolver(riak_object)
  # The "conflict?" method is built into the Ruby client
  if riak_object.conflict?
    riak_object.siblings.max_by{ |user| user.data['friends'].length }
  else
    # If there are no siblings, simply return the object's "content" as is
    riak_object.content
  end
end
```

We can then embed this function into a more general function for
fetching objects from the `users` bucket:

```ruby
def fetch_user_by_username(username)
  bucket = client.bucket('users')
  user_object = bucket.get(username)
  longest_friends_list_resolve(user_object)
  user_object
end
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
value back to Riak. That requires a separate step. One way to do that
would be to modify our `longest_friends_list_resolver` function to
include a step that stores the resolved object:

```ruby
def longest_friends_list_resolver(riak_object)
  if riak_object.conflict?
    riak_object.siblings.max_by{ |user| user.data['friends'].length }
    riak_object.store
    riak_object
  else
    riak_object.content
  end
end
```

Now our resolver function will both return a single `User` object to the
application for further use _and_ notify Riak which value the
application takes to be correct.

The bad news is that this operation may still create siblings, for
example if the write is performed concurrently with another write to the
same key. The good news, however, is that that is perfectly okay. Our
application is now designed to gracefully handle siblings whenever they
are encountered, and the resolution logic we chose will now be applied
automatically every time.

## More Advanced Example

Resolving sibling User values on the basis of which user has the longest
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
precisely that and will also store the resulting `User` object:

```ruby
def longest_friends_list_resolver(riak_object)
  friends_list = []
  if riak_object.conflict?
    riak_object.siblings.each do |sibling|
      friends_list.push(sibling.data['friends'])
    end
    riak_object.content.data = friends_list.uniq
  else
    riak_object.content
  end
end
```



## Riak Data Types

An important thing to always bear in mind when working with conflict
resolution is that Riak offers a variety of [[Data Types]] that have
specific conflict resolution mechanics built in. If you have data that
can be modeled as a [[counter|Data Types#Counters]], [[set|Data
Types#Sets]], or [[map|Data Types#Maps]], then you should seriously
consider using those Data Types instead of creating your own
application-side resolution logic.

In the example above, we were dealing with conflict resolution within a
set, in particular the `friends` list associated with each `User`
object. The merge operation that we built to handle conflict resolution
is analogous to the resolution logic that is built into Riak sets. For
more information on how you could potentially replace the client-side
resolution that we implemented above, see our [[tutorial on Riak
sets|Using Data Types#Sets]].
