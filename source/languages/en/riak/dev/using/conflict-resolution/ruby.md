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

In the official Ruby client, every Riak object has a `siblings` property
that provides access to a list of that object's sibling values. If there
are no siblings, that property will return an array with only one item.
Here's an example of an object with siblings:

```ruby
bucket = client.bucket('seahawks')
obj = bucket.get('coach')
obj.siblings

# The output:
[#<Riak::RContent [content/type]: "Jim Mora">, #<Riak::RContent [content/type]: "Pete Carroll">]
```

So what happens if the length of `obj.siblings` is greater than 1, as in
the case above? In order to resolve siblings, you need to create a
resolution function that takes a Riak object and reduces the `siblings`
array down to a single value. An example is provided in the section
below.

## Basic Conflict Resolution Example

Let's say that we're building a social network application and storing
lists of usernames representing each user's "friends." Each user will be
of the class `User`, which we'll create below. All of the data for our
application will be stored in buckets that bear the [[bucket type|Using
Bucket Types]] `siblings`, and for this bucket type `allow_mult` is set
to `true`, which means that Riak will generate siblings in certain
cases---siblings that our application will need to be equipped to
resolve when necessary.

The question that we need to ask ourselves at this point is the
following: if a given user has conflicting lists, which list should be
deemed more "correct?" What criteria should be applied? Should the lists
be merged? Should we pick a list at random and deem that list correct?
We'll keep it simple here and say that the following criterion will
hold: if multiple conflict lists exist, _the longer list will be the one
that our application deems correct_. While this might not make sense in
real-world applications, it's a good jumping-off point.

### Creating Our Data Class

We'll start by creating a `User` class for each user's data. Each `User`
object will consist of a `username` and a `friends` property that lists
the usernames, as strings, of the user's friends. We will also create a
`to_json` method, as we'll be storing each `User` object as JSON:

```ruby
class User
  def initialize(username, friends)
    @username = username
    @friends = friends
  end

  def to_json
    { :username => @username, :friends => @friends }
  end
end
```

Now, we can create `User` objects and see what they look like as JSON:

```python
new_user = User.new('riakuser127', ['captheorem238', 'siblingsrule572'])

new_user.to_json
# {'username': 'riakuser127', 'friends': ['captheorem238', 'siblingsrule572']}
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
of siblings, i.e. reduces the `obj.siblings` array down to one member.
In our case, we need a function that takes a single Riak object (or
`RObject` in the Ruby client) as its argument, applies some logic to the
list of values contained in the `siblings` property of the object, and
returns a single value. For our example use case here, we'll return the
sibling with the longest `friends` list:

```ruby
def longest_friends_list_resolver(riak_object)
  # The "conflict?" method is built into the Ruby client
  if riak_object.conflict?
    # The "max_by" method enables us to select the sibling with the
    # longest "friends" list
    riak_object.siblings.max_by{ |user| user.data['friends'].length }
  else
    # If there are no siblings, we can simply return the object's
    # "content" as is
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
value back to Riak. That requires a separate step. When this step should
be undertaken depends on your application. In general, though, we
recommend writing objects to Riak only when the application is ready to
commit them, i.e. when all of the changes that need to be made to the
object have been made and the application is ready to persist the state
of the object in Riak.

Correspondingly, we recommend that updates to objects in Riak follow
these steps:

1. **Read** the object from Riak
2. **Resolving sibling conflicts** if the exist, allowing the
application to reason about one "correct" value for the object (this
step is the subject of this tutorial)
3. **Modify** the object
4. **Write** the object to Riak once the necessary changes have been
made

You can find more on writing objects to Riak, including examples from
the official Ruby client library, in [[The Basics|The
Basics#Object-Key-Operations]].

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
  # An empty array for use later on
  friends_list = []
  if riak_object.conflict?
    # The "friends" arrays for all siblings will be merged into one
    # array
    riak_object.siblings.each do |sibling|
      friends_list.push(sibling.data['friends'])
    end
    # We'll invoke the "uniq" method to ensure that there are no
    # duplicates in the "friends" array and then set that array
    riak_object.content.data = friends_list.uniq
  else
    riak_object.content
  end
end
```

Here, we've essentially


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
