---
title_supertext: "Getting Started:"
title: "CRUD Operations with Python"
description: ""
project: "riak_kv"
project_version: 2.9.1
menu:
  riak_kv-2.9.1:
    name: "CRUD Operations"
    identifier: "getting_started_python_crud"
    weight: 100
    parent: "getting_started_python"
toc: true
---

## Creating Objects In Riak

First, let’s create a few objects and a bucket to keep them in.

```python
myBucket = myClient.bucket('test')

val1 = 1
key1 = myBucket.new('one', data=val1)
key1.store()
```

In this first example, we have stored the integer 1 with the lookup key
of `one`.  Next let’s store a simple string value of `two` with a
matching key.

```python
val2 = "two"
key2 = myBucket.new('two', data=val2)
key2.store()
```

That was easy. Finally, let’s store a bit of JSON. You will probably
recognize the pattern by now.

```python
val3 = {"myValue": 3}
key3 = myBucket.new('three', data=val3)
key3.store()
```

## Reading Objects From Riak

Now that we have a few objects stored, let’s retrieve them and make sure
they contain the values we expect.

```python
fetched1 = myBucket.get('one')
fetched2 = myBucket.get('two')
fetched3 = myBucket.get('three')

assert val1 == fetched1.data
assert val2 == fetched2.data
assert val3 == fetched3.data
```

That was easy. We simply request the objects by key.

## Updating Objects In Riak

While some data may be static, other forms of data may need to be
updated. This is also easy to accomplish. Let’s update the value of
myValue in the 3rd example to `42`.

```python
fetched3.data["myValue"] = 42
fetched3.store()
```

## Deleting Objects From Riak

Nothing is complete without a delete. Fortunately, that's easy too.

```python
fetched1.delete()
fetched2.delete()
fetched3.delete()
```

Now we can verify that the objects have been removed from Riak.

```python
assert myBucket.get('one').exists == False
assert myBucket.get('two').exists == False
assert myBucket.get('three').exists == False
```


## Working With Complex Objects

Since the world is a little more complicated than simple integers and
bits of strings, let’s see how we can work with more complex objects.
Take for example, this object that encapsulates some knowledge about a
book.

```python
book = {
  'isbn': "1111979723",
  'title': "Moby Dick",
  'author': "Herman Melville",
  'body': "Call me Ishmael. Some years ago...",
  'copies_owned': 3
}
```

All right, so we have some information about our Moby Dick collection
that we want to save. Storing this to Riak should look familiar by now:

```python
booksBucket = myClient.bucket('books')
newBook = booksBucket.new(book['isbn'], data=book)
newBook.store()
```

Some of you may be thinking, "But how does the Python Riak client
encode/decode my object?" If we fetch our book back and print the raw
encoded data, we shall know:

```python
fetchedBook = booksBucket.get(book['isbn'])

print(fetchedBook.encoded_data)
```

JSON! The Riak Python client library encodes things as JSON when it can.

```json
{"body": "Call me Ishmael. Some years ago...",
"author": "Herman Melville", "isbn": "1111979723",
"copies_owned": 3, "title": "Moby Dick"}
```

If we wanted to get a deserialized object back we would just use the
regular `fetchedBook.data` method.

Finally, let’s clean up our mess:

```python
fetchedBook.delete()
```
