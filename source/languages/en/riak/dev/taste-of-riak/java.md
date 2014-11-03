---
title: "Taste of Riak: Java"
project: riak
version: 2.0.0+
document: guide
toc: true
audience: beginner
keywords: [developers, client, java]
---

If you haven't set up a Riak Node and started it, please visit the
[[Prerequisites|Taste of Riak: Prerequisites]] first.

To try this flavor of Riak, a working installation of Java is required.

## Client Setup

To include the Riak Java client in your project, add it to your
project's dependencies. Here is a Maven example:

```xml
<dependencies>
  <dependency>
    <groupId>com.basho.riak</groupId>
    <artifactId>riak-client</artifactId>
    <version>2.0.0</version>
  </dependency
</dependencies>
```

Next, download
[`TasteOfRiak.java`](https://github.com/basho/basho_docs/raw/master/source/data/TasteOfRiak.java)
source code for this tutorial, and save it to your working directory.

<div class="note">
<div class="title">Configuring for a local cluster</div>
The `TasteOfRiak.java` file that you downloaded is set up to communicate
with a one-node Riak cluster listening on `localhost` port 10017. We
recommend modifying the connection info directly within the
`setUpCluster()` method.
</div>

If you execute the `TasteOfRiak.java` file within your IDE, you should
see the following:

```
Basic object created
Location object created for quote object
StoreValue operation created
Client object successfully created
Object storage operation successfully completed
Success! The object we created and the object we fetched have the same value
Quote object successfully deleted
Book object created
Moby Dick information now stored in Riak
Book object successfully fetched
Success! All of our tests check out
```

Since Java doesn’t have a REPL environment, let's walk through the code
to see what it actually did at each step.

## Setting Up the Cluster

The first step in using the Riak Java client is to create a cluster
object to facilitate all interactions with Riak. You'll see this on line
72:

```java
RiakCluster cluster = setUpCluster();
```

This calls the private `setUpCluster` method which begins on line 25.
Using that `cluster` object, we can instantiate a client object which
will execute all Riak interactions:

```java
RiakClient client = new RiakClient(cluster);
```

## Creating Objects in Riak

The first object that we create is a very basic object with a content
type of `text/plain`. Once that object is created, we create a
`StoreValue` operation that will store the object later on down the line

```java
RiakObject quoteObject = new RiakObject()
        .setContentType("text/plain")
        .setValue(BinaryValue.create("You're dangerous, Maverick"));
Namespace quotesBucket = new Namespace("quotes");
Location quoteObjectLocation = new Location(quotesBucket, "Icemand");
StoreValue storeOp = new StoreValue.Builder(quoteObject)
        .withLocation(quoteObjectLocation)
        .build();
```

In line 76 we use our `client` object to execute the storage operation:

```java
StoreValue.Response response = client.execute(storeOp);
```

## Reading Objects from Riak

After that, we check to make sure that the stored object has the same
value as the object that we created. This requires us to fetch the
object by way of a `FetchValue` operation:

```java
FetchValue fetchOp = new FetchValue.Builder(quoteObjectLocation)
        .build();
RiakObject fetchedObject = client.execute(fetchOp).getValue(RiakObject.class);
assert(fetchedObject.getValue.equals(quoteObject.getValue()));
```

If the values are equal, as they should be, the Java client will say
`Success!  The object we created and the object we fetched have the same
value`. If not, then the client will throw an exception.

## Deleting Objects

Now that we've stored and then fetched the object, we can deleted by
creating and executing a `DeleteValue` operation:

```java
DeleteValue deleteOp = new DeleteValue.Builder(quoteObjectLocation)
        .build();
client.execute(deleteOp);
```
```java
fetched3.put("myValue", 42);
myBucket.store("three", fetched3).execute();
```

To update we simply just store the new value with the same key.

## Deleting Objects From Riak

Nothing is complete without a delete.

```java
myBucket.delete("one").execute();
myBucket.delete("two").execute();
myBucket.delete("three").execute();
```

## Working With Complex Objects

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
want to save.  Storing this to Riak should look familiar by now:

```java
Bucket booksBucket = client.fetchBucket("books").execute();
booksBucket.store(book.ISBN, book).execute();
```

Some of you may be thinking “But how does the Riak client encode/decode
my object”?  If we fetch our book back and print the encoded value as a
string, we shall know:

```java
IRiakObject riakObject = booksBucket.fetch(book.ISBN).execute();
System.out.println(riakObject.getValueAsString());
```

```json
{
  "Title": "Moby Dick",
  "Author": "Herman Melville",
  "Body": "Call me Ishmael. Some years ago...",
  "ISBN": "1111979723",
  "CopiesOwned": 3
}
```

JSON! The library encodes POJOs as JSON strings. If we wanted to get a
`Book` object back we could use `bookBucket.fetch(book.ISBN,
Book.class);` to have the client create the proper object type for us.
Now that we’ve ruined the magic of object encoding, let’s clean up our
mess:

```java
booksBucket.delete(book.ISBN).execute();
client.shutdown();
```

## Next Steps

More complex use cases can be composed from these initial create, read,
update, and delete (CRUD) operations. [[In the next chapter|Taste of
Riak: Querying]], we will look at how to store and query more
complicated and interconnected data, such as documents.
