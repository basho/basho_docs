---
title_supertext: "Getting Started:"
title: "CRUD Operations with Java"
description: ""
project: "riak_kv"
project_version: "2.0.4"
menu:
  riak_kv-2.0.4:
    name: "CRUD Operations"
    identifier: "getting_started_java_crud"
    weight: 100
    parent: "getting_started_java"
toc: true
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

We then use our `client` object to execute the storage operation:

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

## Updating Objects

Once we've read the object back in from Riak, we can update the object
and store it back as we did before with the `StoreValue` object:

```java
fetchedObject.setValue(BinaryValue.create("You can be my wingman any time."));
StoreValue updateOp = new StoreValue.Builder(fetchedObject)
        .withLocation(quoteObjectLocation)
        .build();
StoreValue.Response updateOpResp = client.execute(updateOp);
```

For more in depth information on updating objects and sibling resolution in
Riak, see [Updating Objects]({{<baseurl>}}riak/kv/2.0.4/developing/usage/updating-objects/)
and [Conflict Resolution]({{<baseurl>}}riak/kv/2.0.4/developing/usage/conflict-resolution/)
documentation.

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

Since we really like Moby Dick, let's buy a couple more copies
and update the POJO.

To update the POJO, we would use `UpdateValue` by
extending a new `BookUpdate` class as follows:

```java
public static class BookUpdate extends UpdateValue.Update<Book> {
    private final Book update;
    public BookUpdate(Book update){
        this.update = update;
    }

    @Override
    public Book apply(Book t) {
        if(t == null) {
            t = new Book();
        }

        t.author = update.author;
        t.body = update.body;
        t.copiesOwned = update.copiesOwned;
        t.isbn = update.isbn;
        t.title = update.title;

        return t;
    }
}
```

Then using the `BookUpdate` class with our `mobyDick` object:

```java
mobyDick.copiesOwned = 5;
BookUpdate updatedBook = new BookUpdate(mobyDick);

UpdateValue updateValue = new UpdateValue.Builder(mobyDickLocation)
    .withUpdate(updatedBook).build();
UpdateValue.Response response = client.execute(updateValue);
```

For more in depth information on updating objects and sibling resolution in
Riak, see [Updating Objects]({{<baseurl>}}riak/kv/2.0.4/developing/usage/updating-objects/)
and [Conflict Resolution]({{<baseurl>}}riak/kv/2.0.4/developing/usage/conflict-resolution/)
documention.
