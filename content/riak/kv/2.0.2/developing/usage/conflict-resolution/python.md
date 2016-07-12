---
title_supertext: "Conflict Resolution:"
title: "Python"
description: ""
project: "riak_kv"
project_version: "2.0.2"
menu:
  riak_kv-2.0.2:
    name: "Python"
    identifier: "usage_conflict_resolution_python"
    weight: 102
    parent: "usage_conflict_resolution"
toc: true
aliases:
  - /riak/2.0.2/dev/using/conflict-resolution/python
canonical_link: "https://docs.basho.com/riak/kv/latest/developing/usage/conflict-resolution/python"
---

For reasons explained in the [Introduction to conflict resolution](/riak/kv/2.0.2/developing/usage/conflict-resolution), we strongly recommend adopting a
conflict resolution strategy that requires applications to resolve
siblings according to use-case-specific criteria. Here, we'll provide a
brief guide to conflict resolution using the official [Riak Python
client](https://github.com/basho/riak-python-client).

## How the Python Client Handles Conflict Resolution

In the official Python client, every object of the `RiakObject` class
has a `siblings` property that provides access to a list of an object's
sibling values. If there are no siblings, that property will return a
list with only one item. Here's an example of an object with siblings:

```python
bucket = client.bucket('seahawks')
obj = bucket.get('coach')
obj.siblings

# The output:
[<riak.content.RiakContent object at 0x106cc51d0>, <riak.content.RiakContent object at 0x108x1da62c1>]
```

So what happens if the length of `obj.siblings` is greater than 1, as in
the case above? The easiest way to resolve siblings automatically with
the Python client is to create a conflict-resolving function that takes
a list of sibling values and returns a single value. Such resolution
functions can be registered either at the object level or the bucket
level. A more complete explanation can be found in the section directly
below.

## Basic Conflict Resolution Example

Let's say that we're building a social network application and storing
lists of usernames representing each user's "friends." Each user will
be of the class `User`, which we'll create below. All of the data for our
application will be stored in buckets that bear the [bucket type](/riak/kv/2.0.2/developing/usage/bucket-types) `siblings`, and for this bucket type `allow_mult` is set
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
object will consist of a `friends` property that lists the usernames, as
strings, of the user's friends. We will also create a `to_json` method,
as we'll be storing each `User` object as JSON:

```python
class User(object):
    def __init__(self, username, friends):
        self.username = username
        self.friends = friends

    def to_json(self):
        return vars(self)
```

Now, we can create `User` objects and see what they look like as JSON:

```python
new_user = User('riakuser127', ['captheorem', 'siblingsrule572'])

new_user.to_json()
# {'username': 'riakuser127', 'friends': ['captheorem238', 'siblingsrule572']}
```

### Implementing and Registering a Conflict Resolution Function

Let's say that we've stored a bunch of `User` objects in Riak and that a
few concurrent writes have led to siblings. How is our application going
to deal with that? First, let's say that there's a `User` object stored
in the bucket `users` (which is of the bucket type `siblings`, as
explained above) under the key `bashobunny`. We can fetch the object
that is stored there and see if it has siblings:

```python
bucket = client.bucket_type('siblings').bucket('users')
obj = bucket.get('bashobunny')

print len(obj.siblings) > 1
```

If we get `True`, then there are siblings. So what do we do in that
case? The Python client allows us to write a conflict resolution hook
function that will be triggered any time siblings are found, i.e. any
time `len(obj.siblings) > 1`. A hook function like this needs to take a
single `RiakObject` object as its argument, apply some sort of logic to
the list of values contained in the `siblings` property, and ultimately
return a list with a single "correct" value. For our example case, we'll
return the value with the longest `friends` list:

```python
def longest_friends_list_resolver(riak_object):
    # We'll specify a lambda function that operates on the length of
    # each sibling's "friends" list:
    lm = lambda sibling: len(sibling.data['friends'])
    # Then we'll return a list that contains only the object with the
    # maximum value for the length of the "friends" list:
    riak_object.siblings = [max(riak_object.siblings, key=lm), ]
```

### Registering a Conflict Resolver Function

In the Python client, resolver functions can be registered at the object
level, as in this example:

```python
bucket = client.bucket_type('siblings').bucket('users')
obj = RiakObject(client, bucket, 'bashobunny')
obj.resolver = longest_friends_list_resolver

# Now, when the object is loaded from Riak, it will resolve to a single
# value instead of multiple values:
obj.reload()
```

Alternatively, resolvers can be registered at the bucket level, so that
the resolution is applied to all objects in the bucket:

```python
bucket = client.bucket_type('siblings').bucket('users')
bucket.resolver = longest_friends_list_resolver

obj = RiakObject(client, bucket, 'bashobunny')
obj.reload()

# The resolver will also be applied if you perform operations using the
# bucket object:

bucket.get('bashobunny')
bucket.get('some_other_user')
```

## Conflict Resolution and Writes

In the above example, we created a conflict resolver that resolves a
list of discrepant `User` object values and returns a single value. It's
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

You can find more on writing objects to Riak, including code examples
from the official Python client library, in the [Developing with Riak KV: Usage](/riak/kv/2.0.2/developing/usage) section.

## More Advanced Example

Resolving sibling `User` values on the basis of which user has the
longest `friends` list has the benefit of being simple but it's probably
not a good resolution strategy for our social networking application
because it means that unwanted data loss is inevitable. If one friend
list contains `A`, `B`, and `C` and the other contains `D` and `E`, the
list containing `A`, `B`, and `C` will be chosen. So what about friends
`D` and `E`? Those usernames are essentially lost. In the sections
below, we'll implement an alternative strategy as an example.

### Merging the Lists

To avoid losing data like this, a better strategy would be to merge the
lists. We can modify our original resolver function to accomplish
precisely that and will also store the resulting `User` object:

```python
from riak.content import RiakContent

def longest_friends_list_resolver(riak_object):
    # We start with an empty set
    friends_list = set()

    # Then we add all the friends from all siblings to the set
    for user in riak_object.siblings:
        friends_list.update(user.data['friends'])

    # Then we make a new User object. First, we fetch the username from
    # any one of the siblings, then we pass in our new friends list.
    username = riak_object.siblings[0].data['username']
    new_user = User(username, list(friends_list))

    # Now we reuse the first sibling as a container for the merged data
    riak_object.siblings[0].data = new_user.to_json()

    # And finally we set the siblings property to include just the
    # single, resolved sibling
    riak_object.siblings = [riak_object.siblings[0]]
```

The drawback to this approach is the following: with a conflict
resolution strategy like this, it's more or less inevitable that a user
will remove a friend from their friends list, and that that friend will
end up back on the list during a conflict resolution operation. While
that's certainly not desirable, that is likely better than the
alternative proposed in the first example, which entails usernames being
simply dropped from friends lists. Sibling resolution strategies almost
always carry potential drawbacks of this sort.

## Riak Data Types

An important thing to always bear in mind when working with conflict
resolution is that Riak offers a variety of [Data Types](/riak/kv/2.0.2/developing/data-types/) that have
specific conflict resolution mechanics built in. If you have data that
can be modeled as a [counter](/riak/kv/2.0.2/developing/data-types/counters), [set](/riak/kv/2.0.2/developing/data-types/sets), or [map](/riak/kv/2.0.2/developing/data-types/maps), then you should seriously
consider using those Data Types instead of creating your own
application-side resolution logic.

In the example above, we were dealing with conflict resolution within a
set, in particular the `friends` list associated with each `User`
object. The merge operation that we built to handle conflict resolution
is analogous to the resolution logic that is built into Riak sets. For
more information on how you could potentially replace the client-side
resolution that we implemented above, see our [tutorial on Riak sets](/riak/kv/2.0.2/developing/data-types/sets).
