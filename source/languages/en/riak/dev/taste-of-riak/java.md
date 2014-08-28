---
title: "Taste of Riak: Java"
project: riak
version: 1.4.0+
document: guide
toc: true
audience: beginner
keywords: [developers, client, java]
---

If you haven't set up a Riak Node and started it, please visit the
[[Prerequisites|Taste of Riak: Prerequisites]] first.

To try this flavor of Riak, a working installation of Java is required.

### Client Setup

The best way to get started is to add the Riak Java client to your
Maven dependencies:

```xml
<dependency>
  <groupId>com.basho.riak</groupId>
  <artifactId>riak-client</artifactId>
  <version>2.0.0RC2-SNAPSHOT</version>
  <url>https://github.com/basho/riak-java-client</url>
</dependency>
```
If you set up a local Riak cluster using the [[Five-Minute Install]]
method, you will most likely be using `localhost` as a host and port
10017 for all of your nodes. In this case, the easiest way to create a
client object is to create a single `RiakNode` object, add that object
to a `RiakCluster` object, and then pass that into a client:

```java
import com.basho.riak.client.

RiakNode node1 = new RiakNode.Builder()
        .withRemotePort(10017)
        .withRemoteAddress("127.0.0.1")
        .build();
RiakCluster cluster = new RiakCluster.Builder(node1).build();
RiakClient client = new RiakClient(cluster);
```

With out `client` object ready to go, we can start communicating with
Riak. We'll start by creating a new object and storing it.

### Creating Objects In Riak

As a basic example object, we'll store a simple string.


The first thing we do in our code is initialize a new Riak client
through the `RiakFactory` class.  Next we fetch the information for a
bucket named “test”, and then store our first key/value pair.

```java
IRiakClient client = RiakFactory.pbcClient();

// Note: Use this line instead of the former if using a local devrel cluster
// IRiakClient client = RiakFactory.pbcClient("127.0.0.1", 10017);

Bucket myBucket = client.fetchBucket("test").execute();

int val1 = 1;
myBucket.store("one", val1).execute();

```

In this first example we have stored the integer 1 with the lookup key
of ‘one’.  Next let’s store a simple string value of “two” with a
matching key.

```java
String val2 = "two";
myBucket.store("two", val2).execute();
```

That was easy. Finally, let’s store something more complex, an instance
of a class that extends `HashMap<String,Integer>`. You will probably
recognize the pattern by now.

```java
StringIntMap val3 = new StringIntMap();
val3.put("value", 3);
myBucket.store("three", val3).execute();
```

### Reading Objects From Riak

Now that we have a few objects stored, let’s retrieve them and make sure
they contain the values we expect.

```java
Integer fetched1 = myBucket.fetch("one", Integer.class).execute();
IRiakObject fetched2 = myBucket.fetch("two").execute();
StringIntMap fetched3 = myBucket.fetch("three", StringIntMap.class).execute();

assert(fetched1 == val1);
assert(fetched2.getValueAsString().compareTo(val2) == 0);
assert(fetched3.equals(val3));
```

That was easy. We simply request the objects by key, and include a
`Class` object of the type we want it to be cast into. If your value is
a simple string, you can also omit the `Class` and just use
`IRiakObject`’s `getValueAsString()` method.

### Updating Objects In Riak

While some data may be static, other forms of data may need to be
updated. This is also easy to accomplish. Let’s update the value of
myValue Hashmap entry to 42.

```java
fetched3.put("myValue", 42);
myBucket.store("three", fetched3).execute();
```

To update we simply just store the new value with the same key.

### Deleting Objects From Riak

Nothing is complete without a delete.

```java
myBucket.delete("one").execute();
myBucket.delete("two").execute();
myBucket.delete("three").execute();
```

### Working With Complex Objects

Since the world is a little more complicated than simple integers and
bits of strings, let’s see how we can work with more complex objects.
Take for example, this plain old Java object (POJO) that encapsulates
some knowledge about a book.

```java
class Book
{
    public String Title;
    public String Author;
    public String Body;
    public String ISBN;
    public Integer CopiesOwned;
}

Book book = new Book();
book.ISBN = "1111979723";
book.Title = "Moby Dick";
book.Author = "Herman Melville";
book.Body = "Call me Ishmael. Some years ago...";
book.CopiesOwned = 3;
```

Ok, so we have some information about our Moby Dick collection that we
want to save. Storing this to Riak should look familiar by now:

```java
Bucket booksBucket = client.fetchBucket("books").execute();
booksBucket.store(book.ISBN, book).execute();
```

Some of you may be thinking “But how does the Riak client encode/decode
my object”? If we fetch our book back and print the encoded value as a
string, we shall know:

```java
IRiakObject riakObject = booksBucket.fetch(book.ISBN).execute();
System.out.println(riakObject.getValueAsString());
```

```json
{"Title":"Moby Dick",
 "Author":"Herman Melville",
 "Body":"Call me Ishmael. Some years ago...",
 "ISBN":"1111979723",
 "CopiesOwned":3}
```

JSON! The library encodes POJOs as JSON strings. If we wanted to get a
Book object back we could use `bookBucket.fetch(book.ISBN, Book.class);`
to have the client create the proper object type for us. Now that we’ve
ruined the magic of object encoding, let’s clean up our mess:

```java
booksBucket.delete(book.ISBN).execute();
client.shutdown();
```

### Next Steps

More complex use cases can be composed from these initial create, read,
update, and delete (CRUD) operations. [[In the next chapter|Taste of
Riak: Querying]] we will look at how to store and query more complicated
and interconnected data, such as documents.

