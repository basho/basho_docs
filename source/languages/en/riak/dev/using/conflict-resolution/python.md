---
title: "Conflict Resolution: Python"
project: riak
version: 2.0.0+
document: tutorials
audience: intermediate
keywords: [developers, conflict-resolution, python]
---

For reasons explained in the [[introduction to conflict
resolution|Conflict Resolution]], we strongly recommend adopting a
conflict resolution strategy that requires applications to resolve
siblings according to use-case-specific criteria. Here, we'll provide a
brief guide to conflict resolution using the official [Riak Python
client](https://github.com/basho/riak-python-client).

## How the Python Client Handles Conflict Resolution

In the official Python client, every object of the `RiakObject` class
has a `siblings` property that provides access to a list of an object's
sibling values.  If there are no siblings, that property will return a
list with only one item. Here's an example of an object with siblings:

```python
bucket = client.bucket('seahawks')
obj = bucket.get('coach')
obj.siblings

# The ouput:
[<riak.content.RiakContent object at 0x106cc51d0>, <riak.content.RiakContent object at 0x108x1da62c1>]
```

So what happens if the length of `obj.siblings` is greater than 1, as in
the case above? The easiest way to resolve siblings automatically with
the Python client is to create a conflict resolving function that takes
a list of sibling values and returns a single value. Such resolution
functions can be registered either at the object level or the bucket
level. A more complete explanation can be found in the section directly
below.

## Basic Conflict Resolution Example

Let's say that we're building a social network application and storing
lists of usernames representing each user's "friends." Each user will
bear the class `User`, which we'll create below. All of the data for our
application will be stored in buckets that bear the [[bucket type|Using
Bucket Types]] `siblings`, and for this bucket type `allow_mult` is set
to `true`, which means that Riak will generate siblings in certain
cases---siblings that our application will need to be equipped to
resolve when they arise.

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

```python
class User(object):
    def __init__(self, friends):
        self.friends = friends

    def to_json(self):
        return vars(self)
```

Now, we can create `User` objects and see what they look like as JSON:

```python
new_user = User(['captheorem', 'siblingsrule572'])

new_user.to_json()
# {'friends': ['captheorem238', 'siblingsrule572']}
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
list of discrepant `User` objects and returns a single `User`. It's
important to note, however, that this resolver will only provide the
application with a single "correct" value; it will _not_ write that
value back to Riak. That requires a separate step. One way to do that
would be to modify our `longest_friends_list_resolver` function to
include a step that stores the resolved object:

```python
def longest_friends_list_resolver(riak_object):
    lm = lambda sibling: len(sibling.data['friends'])
    riak_object.siblings = [max(riak_object.siblings, key=lm), ]
    riak_object.store()
```

Now our resolver function will both return a single `User` object to the
application for further use _and_ notify Riak which value the
application takes to be correct.

The bad news is that this operation may still create siblings, for
example if the write is performed simultaneously with another write. The
good news, however, is that that is perfectly okay. Our application is
now designed to gracefully handle siblings whenever they are
encountered, and the resolution logic we chose will now be applied
automatically every time.
