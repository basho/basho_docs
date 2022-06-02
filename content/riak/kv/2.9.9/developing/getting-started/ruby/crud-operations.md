---
title_supertext: "Getting Started:"
title: "CRUD Operations with Ruby"
description: ""
project: "riak_kv"
project_version: 2.9.9
menu:
  riak_kv-2.9.9:
    name: "CRUD Operations"
    identifier: "getting_started_ruby_crud"
    weight: 100
    parent: "getting_started_ruby"
toc: true
aliases:
---

## Creating Objects In Riak

First, let’s create a few objects and a bucket to keep them in.

```ruby
my_bucket = client.bucket("test")

val1 = 1
obj1 = my_bucket.new('one')
obj1.data = val1
obj1.store()
```

In this first example we have stored the integer 1 with the lookup key
of `one`. Next, let’s store a simple string value of `two` with a
matching key.

```ruby
val2 = "two"
obj2 = my_bucket.new('two')
obj2.data = val2
obj2.store()
```

That was easy. Finally, let’s store a bit of JSON. You will probably
recognize the pattern by now.

```ruby
val3 = { myValue: 3 }
obj3 = my_bucket.new('three')
obj3.data = val3
obj3.store()
```

## Reading Objects From Riak

Now that we have a few objects stored, let’s retrieve them and make sure
they contain the values we expect.

```ruby
fetched1 = my_bucket.get('one')
fetched2 = my_bucket.get('two')
fetched3 = my_bucket.get('three')

fetched1.data == val1
fetched2.data == val2
fetched3.data.to_json == val3.to_json
```

That was easy. we simply request the objects by key. in the last
example, we converted to JSON so we can compare a string key to a symbol
key.

## Updating Objects In Riak

While some data may be static, other forms of data may need to be
updated. This is also easy to accomplish. Let’s update the value of
myValue in the 3rd example to 42.

```ruby
fetched3.data["myValue"] = 42
fetched3.store()
```

## Deleting Objects From Riak

As a last step, we’ll demonstrate how to delete data. You’ll see that
the delete message can be called either against the bucket or the
object.

```ruby
my_bucket.delete('one')
obj2.delete()
obj3.delete()
```

## Working With Complex Objects

Since the world is a little more complicated than simple integers and
bits of strings, let’s see how we can work with more complex objects.
Take, for example, this Ruby hash that encapsulates some knowledge about
a book.

```ruby
book = {
  :isbn => '1111979723',
  :title => 'Moby Dick',
  :author => 'Herman Melville',
  :body => 'Call me Ishmael. Some years ago...',
  :copies_owned => 3
}
```

All right, so we have some information about our Moby Dick collection
that we want to save. Storing this to Riak should look familiar by now.

```ruby
books_bucket = client.bucket('books')
new_book = books_bucket.new(book[:isbn])
new_book.data = book
new_book.store()
```

Some of you may be thinking, "But how does the Ruby Riak client
encode/decode my object?" If we fetch our book back and print the raw
data, we shall know:

```ruby
fetched_book = books_bucket.get(book[:isbn])
puts fetched_book.raw_data
```

Raw Data:

```json
{"isbn":"1111979723","title":"Moby Dick","author":"Herman Melville",
"body":"Call me Ishmael. Some years ago...","copies_owned":3}
```

JSON! The Ruby Riak client will serialize objects to JSON when it comes
across structured data like hashes.  For more advanced control over
serialization you can use a library called
[Ripple](https://github.com/basho/ripple), which is a rich Ruby modeling
layer over the basic riak client. Ripple falls outside the scope of
this document but we shall visit it later.

Now, let’s clean up our mess:

```ruby
new_book.delete()
```




