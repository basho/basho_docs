---
title: Taste of Riak - Python Flavor
project: riak
document: guide
toc: true
audience: beginner
keywords: [developers, client, python]
---


If you haven't setup a Riak Node and started it, please visit the [[Prerequisites|Taste of Riak Prerequisites]] first.

To try this flavor of Riak, a working installation of Python is required, with Python 2.7 preferred. 
The Python package `setuptools` is also required to build the source.  You may install setuptools on OSX through MacPorts by running `sudo port install py-distribute`.

###Client Setup

To get the Python client setup we first have to download, build, and install the ProtoBuf library.

1. Download the latest Protobuf source from <https://code.google.com/p/protobuf/>
2. Unzip the archive and move into the directory: `tar -xvf protobuf-2.5.0.tar.gz && cd protobuf-2.5.0/python`
3. Run `sudo python ez_setup.py` to install the Protobuf library.

Next, download the latest python client from GitHub ([zip](https://github.com/basho/riak-python-client/archive/master.zip), [github repository](https://github.com/basho/riak-python-client)), and extract it to your working directory.  

Now, let's build the client.

```bash
python setup.py install
```

Now, let's start the Python REPL, and let’s get set up.  Enter the following into the Python REPL:

```python
import riak
 
myClient = riak.RiakClient()
```

We are now ready to start interacting with Riak.


###Creating Objects In Riak
First, let’s create a few objects and a bucket to keep them in.

```python
myBucket = myClient.bucket('test')

val1 = 1
key1 = myBucket.new('one', data=val1)
key1.store()
```

In this first example we have stored the integer 1 with the lookup key of ‘one’.  Next let’s store a simple string value of “two” with a matching key.

```python
val2 = "two"
key2 = myBucket.new('two', data=val2)
key2.store()
```

That was easy.  Finally, let’s store a bit of JSON.  You will probably recognize the pattern by now.

```python
val3 = {"myValue": 3}
key3 = myBucket.new('three', data=val3)
key3.store()
```

###Reading Objects From Riak
Now that we have a few objects stored, let’s retrieve them and make sure they contain the values we expect.

```python
fetched1 = myBucket.get('one')
fetched2 = myBucket.get('two')
fetched3 = myBucket.get('three')

assert val1 == fetched1.data
assert val2 == fetched2.data
assert val3 == fetched3.data
```

That was easy.  We simply request the objects by key.

###Updating Objects In Riak
While some data may be static, other forms of data may need to be updated.  This is also easy to accomplish.  Let’s update the value of myValue in the 3rd example to 42.

```python
fetched3.data["myValue"] = 42
fetched3.store()
```

###Deleting Objects From Riak
Nothing is complete without a delete, fortunately that's easy too.

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


###Next Steps
More complex use cases can be composed from these initial create, read, update, and delete (CRUD) operations. In the next chapter we will look at how to store and query more complicated and interconnected data, such as documents.  




