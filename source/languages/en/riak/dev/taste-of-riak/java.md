---
title: "Taste of Riak: Java"
project: riak
version: 1.4.0+
document: guide
toc: true
audience: beginner
keywords: [developers, client, java]
---

If you haven't set up a Riak Node and started it, please visit the [[Prerequisites|Taste of Riak: Prerequisites]] first.

To try this flavor of Riak, a working installation of Java is required. 

###Client Setup


Download the [all-in-one Riak Java client jar](http://riak-java-client.s3.amazonaws.com/riak-client-1.1.4-jar-with-dependencies.jar) to your working directory. 

Next, download [TasteOfRiak.java](https://github.com/basho/basho_docs/raw/master/source/data/TasteOfRiak.java) source code for this tutorial, and save it to your working directory.

<div class="note">
<div class="title">Configuring for a local cluster</div>

If you set up a local Riak cluster using the [[five minute install]] method, open up the `TasteOfRiak.java` file in an editor, comment out line 20, uncomment line 23, and save the file.  This code section should now look like:

```java
//IRiakClient client = RiakFactory.pbcClient();

// Note: Use this line instead of the former if using a local devrel cluster
IRiakClient client = RiakFactory.pbcClient("127.0.0.1", 10017);
```

</div>

You can now compile and run this via the command line, or in your favorite IDE.

```bash
$ javac -cp riak-client-1.1.0-jar-with-dependencies.jar TasteOfRiak.java
$ java -ea -cp riak-client-1.1.0-jar-with-dependencies.jar:.  TasteOfRiak
```

Running it should return:

```text
Creating Objects In Riak...
Reading Objects From Riak...
Updating Objects In Riak...
Deleting Objects From Riak...
Working With Complex Objects...
Serialized Object:
	{"Title":"Moby Dick","Author":"Herman Melville","Body":"Call me Ishmael. Some years ago...","ISBN":"1111979723","CopiesOwned":3}
```

Since Java doesn’t have a REPL environment, we shall now walk through the code to see what it actually did at each step.  

###Creating Objects In Riak
The first thing we do in our code is initialize a new Riak client through the `RiakFactory` class.  
Next we fetch the information for a bucket named “test”, and then store our first key/value pair.

```java
IRiakClient client = RiakFactory.pbcClient();

// Note: Use this line instead of the former if using a local devrel cluster
// IRiakClient client = RiakFactory.pbcClient("127.0.0.1", 10017);

Bucket myBucket = client.fetchBucket("test").execute();

int val1 = 1;
myBucket.store("one", val1).execute();
```

In this first example we have stored the integer 1 with the lookup key of ‘one’.  Next let’s store a simple string value of “two” with a matching key.

```java
String val2 = "two";
myBucket.store("two", val2).execute();
```

That was easy.  Finally, let’s store something more complex, an instance of a class that extends `HashMap<String,Integer>`.
You will probably recognize the pattern by now.

```java
StringIntMap val3 = new StringIntMap();
val3.put("value", 3);  
myBucket.store("three", val3).execute();
```

###Reading Objects From Riak
Now that we have a few objects stored, let’s retrieve them and make sure they contain the values we expect.

```java
Integer fetched1 = myBucket.fetch("one", Integer.class).execute();
IRiakObject fetched2 = myBucket.fetch("two").execute();
StringIntMap fetched3 = myBucket.fetch("three", StringIntMap.class).execute();

assert(fetched1 == val1);
assert(fetched2.getValueAsString().compareTo(val2) == 0);
assert(fetched3.equals(val3));
```

That was easy.  We simply request the objects by key, and include a `Class` object of the type we want it to be cast into. If your value is a simple string, you can also omit the `Class` and just use `IRiakObject`’s `getValueAsString()` method.  

###Updating Objects In Riak
While some data may be static, other forms of data may need to be updated.  This is also easy to accomplish.  Let’s update the value of myValue Hashmap entry to 42.

```java
fetched3.put("myValue", 42);
myBucket.store("three", fetched3).execute();
```

To update we simply just store the new value with the same key.

###Deleting Objects From Riak
Nothing is complete without a delete.

```java
myBucket.delete("one").execute();
myBucket.delete("two").execute();
myBucket.delete("three").execute();
```

###Working With Complex Objects
Since the world is a little more complicated than simple integers and bits of strings, let’s see how we can work with more complex objects.  Take for example, this plain old Java object (POJO) that encapsulates some knowledge about a book.

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

Ok, so we have some information about our Moby Dick collection that we want to save.  Storing this to Riak should look familiar by now:

```java
Bucket booksBucket = client.fetchBucket("books").execute();
booksBucket.store(book.ISBN, book).execute();
```

Some of you may be thinking “But how does the Riak client encode/decode my object”?  If we fetch our book back and print the encoded value as a string, we shall know:

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

JSON! The library encodes POJOs as JSON strings.  If we wanted to get a Book object back we could use `bookBucket.fetch(book.ISBN, Book.class);` to have the client create the proper object type for us. 
Now that we’ve ruined the magic of object encoding, let’s clean up our mess:

```java
booksBucket.delete(book.ISBN).execute();
client.shutdown();
```

###Next Steps
More complex use cases can be composed from these initial create, read, update, and delete (CRUD) operations. [[In the next chapter|Taste of Riak: Querying]] we will look at how to store and query more complicated and interconnected data, such as documents.  




