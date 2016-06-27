---
title_supertext: "Getting Started:"
title: "CRUD Operations with Java"
description: ""
project: "riak_kv"
project_version: "2.0.7"
menu:
  riak_kv-2.0.7:
    name: "CRUD Operations"
    identifier: "getting_started_java_crud"
    weight: 100
    parent: "getting_started_java"
toc: true
canonical_link: "https://docs.basho.com/riak/kv/latest/developing/getting-started/java/crud-operations"
---

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

Now that we've stored and then fetched the object, we can delete it by
creating and executing a `DeleteValue` operation:

```java
DeleteValue deleteOp = new DeleteValue.Builder(quoteObjectLocation)
        .build();
client.execute(deleteOp);
```

## Working With Complex Objects

Since the world is a little more complicated than simple integers and
bits of strings, let’s see how we can work with more complex objects.
Take for example, this plain old Java object (POJO) that encapsulates
some knowledge about a book.

```java
public class Book {
    public String title;
    public String author;
    public String body;
    public String isbn;
    publict Integer copiesOwned;
}
```

By default, the Java Riak client serializes POJOs as JSON. Let's create
a new `Book` object to store:

```java
Book mobyDick = new Book();
modyDick.title = "Moby Dick";
mobyDick.author = "Herman Melville";
mobyDick.body = "Call me Ishmael. Some years ago...";
mobyDick.isbn = "11119799723";
mobyDick.copiesOwned = 3;
```

Now we can store that POJO object just like we stored the more simple
object earlier:

```java
Namespace booksBucket = new Namespace("books");
Location mobyDickLocation = new Location(booksBucket, "moby_dick");
StoreValue storeBookOp = new StoreValue.Builder(mobyDick)
        .withLocation(mobyDickLocation)
        .build();
client.execute(storeBookOp);
```

If we fetch the object (using the same method we showed up above and in
`TasteOfRiak.java`), we should get the following:

```json
{
  "title": "Moby Dick",
  "author": "Herman Melville",
  "body": "Call me Ishmael. Some years ago...",
  "isbn": "1111979723",
  "copiesOwned": 3
}
```
