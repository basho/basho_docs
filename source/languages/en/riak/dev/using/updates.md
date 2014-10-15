---
title: Updating Values
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
features can be found in our [[Application Guide|Application
Guide#Which-Features-Should-You-Consider]].

Amongst the four CRUD operations, object updates in Riak tend to be the
least straightforward and to require a bit more subtle reasoning on the
application side than the others. In this document, we'll discuss some
best practices for updating Riak objects and provide code examples for
each of our official [[client libraries]]: Java, Ruby, Python, and
Erlang.

## The Ideal Object Update Cycle

1. **Read** the object from Riak. This step is important for updates
because this enables you to fetch the object's [[causal context
object]], which is the information that Riak uses to make decisions
about which object values are most recent (this is especially useful
for objects that are frequently updated). This context object needs to
be passed back to Riak when you update the object. This step is handled
for you by Basho's client libraries as long as you perform a read prior
to an update.
2. **Resolve sibling conflicts** if they exist.
3. **Modify the object** on the application side.
4. **Write** the new, modified object to Riak. Because you read the
object first, Riak will receive the object's causal context metadata.
Remember that this happens automatically.

In general, you should read an object before modifying it. Thing of it
as performing a `GET` prior to any `PUT`.

### Updating Deleted Objects

It may seem strange, but if an object existed in a particular bucket
type/bucket/key location and was deleted, you should read the deleted
object prior to updating it.

## Example Update

In this section, we'll provide an update example for Basho's official
Ruby, Python, and Erlang clients. Because updates with the official Java
client function somewhat differently, those examples can be found in the
[[section below|Updating Values#Java-Client-Example]].

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
Location location = new Location(...);
UpdateValue updateOp = new UpdateValue.Builder(location)
        .withUpdate(new UpdateUserName("cliffhuxtable1986"))
        .build();
client.execute(updateOp);
```

### Clobber Updates

If you'd like to update an object by simply replacing it with an
entirely new value (unlike in the section above, where only one property
of the object was updated), the Java client provides you with a
"clobber" update that you can use
