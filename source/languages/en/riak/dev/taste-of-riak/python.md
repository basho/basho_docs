---
title: "Taste of Riak: Python"
project: riak
version: 1.4.0+
document: guide
toc: true
audience: beginner
keywords: [developers, client, python]
---


If you haven't set up a Riak Node and started it, please visit the
[[Prerequisites|Taste of Riak: Prerequisites]] first.

To try this flavor of Riak, a working installation of Python is
required, with Python 2.7 preferred. One of the Python package managers,
e.g. `setuptools` or `pip`, is also required to install the client
package.

You may install `setuptools` on OS X through MacPorts by running `sudo
port install py-distribute`. `setuptools` and `pip` are included in the
Homebrew formula for Python on OS X as well. Just run `brew install
python`.

## Client Setup

The easiest way to install the client is with `easy_install` or `pip`.
Either of the commands below will ensure that the client and all its
dependencies are installed and on the load path. Depending on where your
Python libraries are held, these may require `sudo`.

```bash
easy_install riak
pip install riak
```

To install from source, download the latest Python client from GitHub
([zip](https://github.com/basho/riak-python-client/archive/master.zip),
[GitHub repository](https://github.com/basho/riak-python-client)), and
extract it to your working directory.

Now, let's build the client.

```bash
python setup.py install
```

## Connecting to Riak

Now, let's start the Python REPL and get set up. Enter the following
into the Python REPL:

```python
import riak
```
If you are using a single local Riak node, use the following to create a
new client instance:

```python
myClient = riak.RiakClient(pb_port=8087, protocol='pbc')

# Because the Python client uses the Protocol Buffers interface by
# default, the following will work the same:
myClient = riak.RiakClient(pb_port=8087)
```

If you set up a local Riak cluster using the [[five-minute install]]
method, use this code snippet instead:

```python
myClient = riak.RiakClient(pb_port=10017, protocol='pbc')
```

We are now ready to start interacting with Riak.


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

## Next Steps

More complex use cases can be composed from these initial create, read,
update, and delete (CRUD) operations. [[In the next chapter|Taste of
Riak: Querying]] we will look at how to store and query more complicated
and interconnected data, such as documents.
