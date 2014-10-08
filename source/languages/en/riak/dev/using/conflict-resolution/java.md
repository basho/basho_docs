---
title: "Conflict Resolution: Java"
project: riak
version: 2.0.0+
document: tutorials
audience: intermediate
keywords: [developers, conflict-resolution, java]
---

For reasons explained in the [[introduction to conflict
resolution|Conflict Resolution]], we strongly recommend adopting a
conflict resolution strategy that requires applications to resolve
siblings according to use-case-specific criteria. Here, we'll provide a
brief guide to conflict resolution using the official [Riak Java
client](https://github.com/basho/riak-java-client).

## How the Java Client Handles Conflict Resolution

The official Riak Java client provides a `ConflictResolver` interface
for handling sibling resolution. This interface requires that you
implement a `resolve` method that takes a Java `List` of objects of a
specific type that are stored in Riak and produces a single object of
that type, i.e. converts a `List<T>` to a single `T`. Once that
interface has been implemented, it can be registered as a singleton and
thereby applied to all read operations on a specific data type. Below is
an example resolver for the class `Foo`:

```java
import com.basho.riak.client.api.cap.ConflictResolver;

public class FooResolver implements ConflictResolver<Foo> {
    @Override
    public Foo resolve(List<Foo> siblings) {
        // Insert your sibling resolution logic here
    }
}
```

What happens within the `resolve` method is up to you and will always
depend on the use case at hand. You can implement a resolver that
selects a random `Foo` from the list, chooses the `Foo` with the most
recent timestamp (if you've set up the class `Foo` to have timestamps),
etc. In this tutorial we'll provide a simple example to get you started.

## Basic Conflict Resolution Example

Let's say that we're building a social network application and storing
lists of usernames representing each user's "friends" in the network.
Each user will bear the class `User`, which we'll create below. All of
the data for our application will be stored in buckets that bear the
[[bucket type|Using Bucket Types]] `siblings`, and for this bucket type
`allow_mult` is set to `true`, which means that Riak will generate
siblings in certain cases---siblings that our application will need to
be equipped to resolve when they arise.

The question that we need to ask ourselves now is this: if a given user
has sibling values, i.e. if there are multiple `friends` lists and Riak
can't decide which one is most causally recent, which list should be
deemed "correct" from the standpoint of the application? What criteria
should be applied in making that decision? Should the lists be merged?
Should we pick a `User` object at random?

This decision will always be yours to make. Here, though, we'll keep it
simple and say that the following criterion will hold: if conflicting
lists exist, _the longer list will be the one that our application deems
correct_. So if the user `user1234` has a sibling conflict where one
possible value has `friends` lists with 100, 75, and 10 friends,
respectively, the list of 100 friends will win out.  While this might
not make sense in real-world applications, it's a good jumping-off
point. We'll explore the drawbacks of this approach, as well as a better
alternative, in this document as well.

### Creating Our Data Class

We'll start by creating a `User` class for each user's data. Each `User`
object will consist of a `username` as well as a `friends` property that
lists the usernames, as strings, of the user's friends. We'll use a
`Set` for the `friends` property to avoid duplicates.

```java
public class User {
    public String username;
    public Set<String> friends;

    public User(String username, Set<String> friends) {
        this.username = username;
        this.friends = friends;
    }
}
```

Here's an example of instantiating a new `User` object:

```java
Set<String> friends = new HashSet<String>();
friends.add("fred");
friends.add("barney");
User bashobunny = new User("bashobunny", friends);
```

### Implementing a Conflict Resolution Interface

So what happens if siblings are present and the user `bashobunny` has
different friend lists in different object replicas? For that we can
implement the `ConflictResolver` class described [[above|Conflict
Resolution: Java#How-the-Java-Client-Handles-Conflict-Resolution]]. We
need to implement that interface in a way that is specific to the need
at hand, i.e. taking a list of `User` objects and returning the `User`
object that has the longest `friends` list:

```java
import com.basho.riak.client.api.cap.ConflictResolver;

public class UserResolver implements ConflictResolver<User> {
    @Override
    public User resolve(List<User> siblings) {
        // If there are no objects present, return null
        if (siblings.size == 0) {
            return null;
        // If there is only one User object present, return that object
        } else if (siblings.size == 1) {
            return siblings.get(0);
        // And if there are multiple User objects, return the object
        // with the longest list
        } else {
            int longestList = 0;
            User userWithLongestList;

            // Iterate through the User objects to check for the longest
            // list
            for (User user : siblings) {
                if (user.friends.size() > longestList) {
                    userWithLongestList = user;
                    longestList = user.friends.size();
                }
            }
            // If all sibling User objects have a friends list with a length
            // of 0, it doesn't matter which sibling is selected, so we'll
            // simply select the first one in the list:
            return userWithLongestList == null ? siblings.get(0) : userWithLongestList;
        }
    }
}
```

### Registering a Conflict Resolver Class

To use a conflict resolver, we must register it:

```java
ConflictResolverFactory factory = ConflictResolverFactory.getInstance();
ConflictResolver<User> userResolver = factory.getConflictResolver(new UserResolver());
```

With the resolver registered, the resolution logic that we have created
will resolve siblings automatically upon read. Registering a custom
conflict resolver can occur at any point in the application's lifecycle
and will be applied on all reads that involve that object type.

## Conflict Resolution and Writes

In the above example, we created a conflict resolver that resolves a list
of discrepant `User` objects and returns a single `User`. It's important
to note, however, that this resolver will only provide the application
with a single "correct" value; it will _not_ write that value back to
Riak. That requires a separate step. One way to do that would be to
modify the `UserResolver` class that we created above to also include a
step that stores the `User` object with the longest `friends` list.
Below is an example:

```java
public class UserResolver implements ConflictResolver<User> throws Exception {
    @Override
    public User resolve(List<User> siblings) throws Exception {
        if (siblings.size == 0) {
            return null;
        } else if (siblings.size == 1) {
            return siblings.get(0);
        } else {
            int longestList = 0;
            User userWithLongestList;

            for (User user : siblings) {
                if (user.friends.size() > longestList) {
                    userWithLongestList = user;
                    longestList = user.friends.size();
                }
            }
            userWithLongestList = userWithLongestList == null ? siblings.get(0) : userWithLongestList;
            Location loc = new Location(new Namespace("siblings", "users"), userWithLongestList.username);
            storeUser(userWithLongestList, loc);
            return userWithLongestList;
        }
    }

    public void storeUser(User user, Location location) throws Exception {
        StoreValue storeOp = new StoreValue.Builder(user)
                .withLocation(location)
                .build();
        client.execute(storeOp.build());
    }
}
```

Now our resolver will both return a single `User` object to the
application for further use _and_ notify Riak which value the
application takes to be correct.

The bad news is that this operation may still create siblings, for
example if the write is performed simultaneously with another write. The
good news, however, is that that is perfectly okay. Our application is
now designed to gracefully handle siblings whenever they are
encountered, and the resolution logic we choose will now be applied
automatically every time.

## More Advanced Example

Resolving sibling `User` values on the basis of which user has the
longest `friends` list has the benefit of being simple but it's probably
not a good resolution strategy for our social networking application
because it means that unwanted data loss is inevitable. If one friends
list contains `A`, `B`, and `C` and the other contains `D` and `E`, the
list containing `A`, `B`, and `C` will be chosen. So what about friends
`D` and `E`? Those usernames are essentially lost. In the sections
below, we'll implement some other conflict resolution strategies as
examples.

### Merging the Lists

To avoid losing data like this, a better strategy may be to merge the
lists. We can modify our original `resolve` function in our
`UserResolver` to accomplish precisely that:

```java
public class UserResolver implements ConflictResolver<User> {
    @Override
    public User resolve(List<User> siblings) {
        // We apply the same logic as before, returning null if the
        // key is empty and returning the one sibling if there is only
        // one User in the siblings list
        if (siblings.size == 0) {
            return null;
        } else if (siblings.size == 1) {
            return siblings.get(0);
        } else {
            // We begin with an empty Set
            Set<String> setBuilder = new HashSet<String>();

            // We know that all User objects in the List will have the
            // same username, since we used the username for the key, so
            // we can fetch the username of any User in the list:
            String username = siblings.get(0).username;

            // Now for each User object in the list we add the friends
            // list to our empty Set
            for (User user : siblings) {
                setBuilder.addAll(user.friends);
            }

            // Then we return a new User object that takes the Set we
            // built as the friends list
            return new User(username, setBuilder);

            // Notice that we do not store the User object in this
            // example. That can be accomplished in similar fashion to
            // the example above
        }
    }
}
```

Since the `friends` list is a Java `Set`, we don't need to worry about
duplicate usernames.

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
