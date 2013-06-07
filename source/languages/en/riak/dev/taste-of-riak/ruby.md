---
title: Taste of Riak - Ruby Flavor
project: riak
document: guide
toc: true
audience: beginner
keywords: [developers, client, ruby]
---

If you haven't setup a Riak Node and started it, please visit the [[Prerequisites|Taste of Riak Prerequisites]] first.

To try this flavor of Riak, a working installation of Ruby is required. 

###Client Setup
First, install the Riak Ruby client through `gem`.

```bash
gem install riak-client
```

Start IRB, the Ruby REPL, and let’s get setup.  Enter the following into IRB:

```ruby
require 'riak'

client = Riak::Client.new()
```

We are now ready to start interacting with Riak.

###Creating Objects In Riak
First, let’s create a few objects and a bucket to keep them in.

```ruby
my_bucket = client.bucket("test")

val1 = 1
obj1 = my_bucket.new('one')
obj1.data = val1
obj1.store()
```

In this first example we have stored the integer 1 with the lookup key of ‘one’.  Next let’s store a simple string value of “two” with a matching key.

```ruby
val2 = "two"
obj2 = my_bucket.new('two')
obj2.data = val2
obj2.store()
```

That was easy.  Finally, let’s store a bit of JSON.  You will probably recognize the pattern by now.

```ruby
val3 = { myValue: 3 }
obj3 = my_bucket.new('three')
obj3.data = val3
obj3.store()
```

###Reading Objects From Riak
Now that we have a few objects stored, let’s retrieve them and make sure they contain the values we expect.

```ruby
fetched1 = my_bucket.get('one')
fetched2 = my_bucket.get('two')
fetched3 = my_bucket.get('three')

fetched1.data == val1
fetched2.data == val2
fetched3.data.to_json == val3.to_json
```

That was easy.  We simply request the objects by key.  In the last example we converted to JSON so we can compare a string key to a symbol key.


###Updating Objects In Riak
While some data may be static, other forms of data may need to be updated.  This is also easy to accomplish.  Let’s update the value of myValue in the 3rd example to 42.

```ruby
fetched3.data["myValue"] = 42
fetched3.store()
```

###Deleting Objects From Riak
As a last step, we’ll demonstrate how to delete data.  You’ll see that the delete message can be called either against the Bucket or the Object.

```ruby
my_bucket.delete('one')
obj2.delete()
obj3.delete()
```


###Next Steps
More complex use cases can be composed from these initial create, read, update, and delete (CRUD) operations. In the next chapter we will look at how to store and query more complicated and interconnected data, such as documents.  


