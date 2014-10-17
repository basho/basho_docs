---
title: Object Updates
project: riak
version: 2.0.0+
document: tutorials
audience: beginner
keywords: [developers, updating, kv]
---

While Riak supports a variety of querying mechanisms, such as [[Riak
Search|Using Search]] and [[secondary indexes|Using Secondary Indexes]],
we always recommend sticking to basic **C**reate, **R**read, **U**pdate,
and **D**elete (CRUD) operations as much as possible, as these
operations are generally the most performant and reliable operations
that Riak offers. A complete guide to making decisions about Riak
features can be found in our [[Application Guide|Building Applications
with RiakWhich-Features-Should-You-Consider]].

Amongst the four CRUD operations, object updates in Riak tend to be the
least straightforward and to require a bit more subtle reasoning on the
application side than the others. In this document, we'll discuss some
best practices for updating Riak objects and provide code examples for
each of our official [[client libraries]]: Java, Ruby, Python, and
Erlang.

<div class="note">
<div class="title">Note on immutable data</div>
An important thing to bear in mind is that Riak almost always performs
best with immutable data. If your use case allows for it, we suggest
sticking to that. If not, this tutorial shows you how to work with
mutable data in a way that is consistent with Riak's strengths.
</div>

## The Object Update Cycle

If you decide that your application requires mutable data in Riak, we
recommend that you:

* avoid high-frequency object updates to the same key (i.e. multiple
  updates per second for long periods of time), as this will degrade
  Riak performance; and that you
* follow a read-modify-write cycle when performing updates.

That cycle looks something like this:

1. **Read** the object from Riak. This step is important for updates
because this enables you to fetch the object's [[causal context
object]], which is the information that Riak uses to make decisions
about which object values are most recent (this is especially useful
for objects that are frequently updated). This context object needs to
be passed back to Riak when you update the object. This step is handled
for you by Basho's client libraries as long as you perform a read prior
to an update. In addition, if you have chosen to allow Riak to generate
[[siblings|Conflict Resolution#Siblings]] \(which we recommend), you
should **resolve sibling conflicts** upon read if they exist. For more
on this, please see our documentation on [[conflict resolution]], along
with examples from our official client libraries:
  * [[Java|Conflict Resolution: Java]]
  * [[Ruby|Conflict Resolution: Ruby]]
  * [[Python|Conflict Resolution: Python]]
2. **Modify the object** on the application side.
3. **Write** the new, modified object to Riak. Because you read the
object first, Riak will receive the object's causal context metadata.
Remember that this happens automatically.

In general, you should read an object before modifying it. Think of it
as performing a `GET` prior to any `PUT` when interacting with a REST
API.

### Updating Deleted Objects

You should use the read-modify-write cycle explained above at all times,
_even if you're updating deleted objects_. The reasons for that can be
found in our documentation on [[tombstones|Object Deletion#Tombstones]].
There are some modifications that you may need to make if you are
updating objects that may have been deleted previously. If you are using
the Java client, an explanation and examples are given in the
[[Java-specific section below|Object Updates#Java-Client-Example]]. If
you are using the Python client, causal context for deleted objects will
be handled automatically.

<!-- TODO -->

## Example Update

In this section, we'll provide an update example for Basho's official
Ruby, Python, and Erlang clients. Because updates with the official Java
client function somewhat differently, those examples can be found in the
[[section below|Updating Values#Java-Client-Example]].

For our example, imagine that you are storing information about NFL head
coaches in the bucket `coaches`, which will bear the bucket type
`siblings`, which sets `allow_mult` to `true`. The key for each object
is the name of the team, e.g. `giants`, `broncos`, etc. Each object will
consist of the name of the coach in plain text. Here's an example of
creating and storing such an object:

```python
from riak import RiakObject

bucket = client.bucket_type('siblings').bucket('coaches')
obj = RiakObject(client, bucket, 'seahawks')
obj.content_type = 'text/plain'
obj.data = 'Pete Carroll'
obj.store()
```

Every once in a while, though, head coaches change in the NFL, which
means that our data would need to be updated. Below is an example
function for updating such objects:


```python
def update_coach(team, new_coach):
    bucket = client.bucket_type('siblings').bucket('coaches')
    # The read phase
    obj = bucket.get(team)
    # The modify phase
    obj.data = new_coach
    # The write phase
    obj.store()

# Example usage
update_coach('packers', 'Vince Lombardi')
```

In the example above, you can see the three steps in action: first, the
object is read, which automatically fetches the object's causal context;
then the object is modified, i.e. the object's value is set to the name
of the new coach; and finally the object is written back to Riak.

## Java Client Example

As with the other official clients, object updates using the Java client
will automatically fetch the object's causal context metadata.

```java
import com.basho.riak.client.api.commands.kv.UpdateValue.Update;

public class UpdateUserName extends Update<User> {
    @Override
    public User apply(User original) {
        // update logic goes here
    }
}
```

```java
public class UpdateUserName extends Update<User> {
    private String newName;

    public UpdateUserName(String newName) {
        this.newName = newName;
    }

    @Override
    public User apply(User original) {
        original.setName(newName);
        return original;
    }
}
```

```java
import com.basho.riak.client.api.commands.kv.FetchValue;

Location location = new Location(...);
UpdateValue updateOp = new UpdateValue.Builder(location)
        .withFetchOption(FetchValue.Option.DELETED_VCLOCK, true)
        .withUpdate(new UpdateUserName("cliffhuxtable1986"))
        .build();
client.execute(updateOp);
```

You may notice that a fetch option was added to our `UpdateValue`
operation: `FetchValue.Option.DELETED_VCLOCK` was set to `true`.
Remember from the section above that you should always read an object
before modifying and writing it, _even if the object has been deleted_.
Setting this option to `true` ensures that the causal context is fetched
from Riak if the object has been deleted. We recommend always setting
this option to `true` when constructing `UpdateValue` operations.

### Clobber Updates

If you'd like to update an object by simply replacing it with an
entirely new value (unlike in the section above, where only one property
of the object was updated), the Java client provides you with a
"clobber" update that you can use to replace the existing object with a
new object of the same type rather than changing one or more properties
of the object. Imagine that there is a `User` object stored in the
bucket `users` in the key `cliffhuxtable1986`, as in the example above,
and we simply want to replace the object with a brand new object:

```java
Location location = new Location(new Namespace("users"), "cliffhuxtable1986");
User brandNewUser = new User(/* new user info */);
UpdateValue updateOp = new UpdateValue.Builder(Location)
        // As before, we set this option to true
        .withFetchOption(FetchValue.Option.DELETED_VCLOCK, true)
        .withUpdate(new UpdateUserName("cliffhuxtable1986"))
        .build();
client.execute(updateOp);
```
