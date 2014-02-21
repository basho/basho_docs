---
title: Data Modeling with Riak Data Types
project: riak
version: 2.0.0+
document: tutorial
audience: intermediate
keywords: [developers, data-modeling, datatypes]
---

In our [[tutorial on Riak Data Types|Using Data Types]], we show you how to create and perform a variety of operations on each of the available Riak [[Data Types]]---[[registers|Data Types#Registers]], [[flags|Data Types#Flags]], [[counters|Data Types#Counters]], [[sets|Data Types#Sets]], and [[maps|Data Types#Maps]].

While that tutorial covers the basics of using Riak Data Types, most real-world applications would need to use Data Types in a more structured way. Here, we'll walk through a very basic example of how an application might approach Riak Data Types through data models, in this case using Riak as a data store for user information.

## Creating a Basic Data Model

For our user database, we'll create a basic `User` type that will house the following information about each user:

* first name
* last name
* interests
* visits to our site

And so we can begin constructing our data model by creating a `User` class that provides us getters and setters for each of those characteristics:

```ruby
class User
  attr_accessor :first_name, :last_name, :interests, :visits
end
```

This enables us to create new `User`s and modify those characteristics:

```ruby
bill = User.new
bill.first_name = 'Bill'
bill.last_name = 'Murray'
bill.interests = ['filming Caddyshack', 'being smartly funny']
bill.visits = 10
bill #<User:0x007fb7f7885408 @first_name="Bill", @last_name="Murray", @interests=["filming Caddyshack", "being smartly funny"], @visits=10>
```

Amongst the Riak [[Data Types]], a `User` is best modeled as a map, because it has a variety of Data Types within it: a few strings (best modeled as [[registers|Data Types#Registers]]), an array (best modeled as a [[set|Data Types#Set]]). Maps are the only Riak Data Type that can house multiple Data Types within them (including maps themselves, although that won't be convered in this tutorial).

## Connecting Our Data Model to Riak

First, we need to create a bucket type that is capable of 

Then, we need to create a client to connect to Riak and specify the bucket in which our `User` objects will be stored (we'll simply call it `users`). For this tutorial, we'll use `localhost` as our host and `8087` as our protobuf port:

```ruby
$client = Riak::Client.new(:host => 'localhost', :pb_port => 8087)
$bucket = $client.bucket 'users'
```

Now, we can begin connecting our data model to a Riak map. We'll do that creating a Riak map when a new `User` is created:

```ruby
class User
  def initialize
    @map = Riak::Crdt::Map.new $bucket, '<key>'
  end
end
```

Note that we haven't specified in which key our map will be stored. In a key/value store like Riak, choosing a key is very important. We'll keep it simple here and use a string consisting of first and last name (separate by an underscore) as the key:

```ruby
class User
  def initialize first_name, last_name
    @map = Riak::Crdt::Map.new $bucket, "#{first_name}_#{last_name}"
  end
end
```

## Storing An Object's Properties in Our Riak Map

At this point, we have a Riak map associated with instantiations of our `User` type, but that map will be empty. Let's modify our initializer function to populate registers in the map with first name and last name information:

```ruby
class User
  def initialize first_name, last_name
    @map = Riak::Crdt::Map.new $bucket, '#{first_name}#{last_name}'
    @map.registers['first_name'] = first_name
    @map.registers['last_name'] = last_name
  end
end
```

Now, if we create a new user, that user will have a `map` instance variable attached to it, the `first_name` and `last_name` strings will be stored in Riak registers, and the key assigned to the map will be just as we should expect (`Bruce_Wayne` in this case):

```ruby
bruce = User.new 'Bruce', 'Wayne'
bruce
#=>  #<User:0x007fe2965cafc8 @map=#<Riak::Crdt::Map:0x007fe2965caf78 @bucket=#<Riak::Bucket {users}>, @key"\#{first_name}\#{last_name}", @bucket_type"map", @options{}, @dirtyfalse, @counters#<Riak::Crdt::TypedCollection:0x007fe296125ae0 @type=Riak::Crdt::InnerCounter, @parent=#<Riak::Crdt::Map:0x007fe2965caf78 ...>, @contents{}, @flags#<Riak::Crdt::TypedCollection:0x007fe2961257c0 @type=Riak::Crdt::InnerFlag, @parent=#<Riak::Crdt::Map:0x007fe2965caf78 ...>, @contents{}, @maps#<Riak::Crdt::TypedCollection:0x007fe296125428 @type=Riak::Crdt::InnerMap, @parent=#<Riak::Crdt::Map:0x007fe2965caf78 ...>, @contents{}, @registers#<Riak::Crdt::TypedCollection:0x007fe296124e60 @type=Riak::Crdt::InnerRegister, @parent=#<Riak::Crdt::Map:0x007fe2965caf78 ...>, @contents{"last_name"=>"Wayne", "first_name"=>"Bruce"}, @sets#<Riak::Crdt::TypedCollection:0x007fe296124460 @type=Riak::Crdt::InnerSet, @parent=#<Riak::Crdt::Map:0x007fe2965caf78 ...>, @contents{}, @context"M\x01\x83P\x00\x00\x00\xC4x\x01\xCB`\xCAa```\xCC`\xCA\x05R\x1CG\x836r\a3\x1F\xB1Od\xC9\x02\t3e\x00!H\x82+-\xB3\xA8\xB8$>/175\x85\x81\xAF(31;>\xA5$>\xA7\xBC\xBC(5=\x03\xBB\t\xCCY\x10\xAD\xACNE\xA5\xC9\xA9y\xEC\fB%\xEE\xD1\x1F?\xB1\xC0\x8C\xE4\xCCI$\xD1D\x16\x98\x89\xE1\x89\x95y \x13y\x9B\xC0&f\x01\x00\xAF\x055\xA8"
```

So now we have our `first_name` and `last_name` variables stored in our map, but we still need to account for `interests` and `visits`. First, let's modify our class definition to store each user's interests in a set within the map:

```ruby
class User
  def initialize first_name, last_name, interests
    @map = Riak::Crdt::Map.new $bucket, '#{first_name}#{last_name}'
    @map.registers['first_name'] = first_name
    @map.registers['last_name'] = last_name
    interests.each do |i|
      @map.sets['interests'].add i
    end
  end
end  
```

Our `visits` variable will work a little bit differently, because when a new user is created the value will simply by zero. But let's create a new instance method `visit_page` that increments the `visits` counter by one every time it is called:

```ruby
class User
  def initialize first_name, last_name, interests
    @map = Riak::Crdt::Map.new $bucket, '#{first_name}#{last_name}'
    @map.registers['first_name'] = first_name
    @map.registers['last_name'] = last_name
    interests.each do |i|
      @map.sets['interests'].add i
    end
  end

  def visit_page
    @map.counters['visits'].increment
  end
end 
```

Now when we create new users, we need to specify their interests as a list:

```ruby
joe = User.new 'Joe', 'Armstrong', ['distributed systems', 'Erlang']
joe
#=> #<User:0x007f9a4b81ead8 @map=#<Riak::Crdt::Map:0x007f9a4b81ea88 @bucket=#<Riak::Bucket {users}>, @key"\#{first_name}\#{last_name}", @bucket_type"map", @options{}, @dirtyfalse, @counters#<Riak::Crdt::TypedCollection:0x007f9a4b89fae8 @type=Riak::Crdt::InnerCounter, @parent=#<Riak::Crdt::Map:0x007f9a4b81ea88 ...>, @contents{}, @flags#<Riak::Crdt::TypedCollection:0x007f9a4b89f8b8 @type=Riak::Crdt::InnerFlag, @parent=#<Riak::Crdt::Map:0x007f9a4b81ea88 ...>, @contents{}, @maps#<Riak::Crdt::TypedCollection:0x007f9a4b89f688 @type=Riak::Crdt::InnerMap, @parent=#<Riak::Crdt::Map:0x007f9a4b81ea88 ...>, @contents{}, @registers#<Riak::Crdt::TypedCollection:0x007f9a4b89f4a8 @type=Riak::Crdt::InnerRegister, @parent=#<Riak::Crdt::Map:0x007f9a4b81ea88 ...>, @contents{"last_name"=>"Armstrong", "first_name"=>"Joe"}, @sets#<Riak::Crdt::TypedCollection:0x007f9a4b89f0e8 @type=Riak::Crdt::InnerSet, @parent=#<Riak::Crdt::Map:0x007f9a4b81ea88 ...>, @contents{"interests"=>#<Riak::Crdt::InnerSet:0x007f9a4b89ee90 @parent=#<Riak::Crdt::TypedCollection:0x007f9a4b89f0e8 ...>, @value#<Set: {"Erlang"}, @name="interests">}, @context"M\x01\x83P\x00\x00\x01Ex\x01\xCB`\xCAa```\xCC`\xCA\x05R\x1CG\x836r\a3\x1F\xB1O\xE4\xCC\x02\t3g0A$\xB8\xD22\x8B\x8AK\xE2\xF3\x12sSS\x18\xF8\x8A2\x13\xB3\xE3SJ\xE2s\xCA\xCB\x8BR\xD33\xB0\x9B\xC0\x9E\x05\xD1\xCA\xEC\x95\x9F\x9A\xC7\xCE\xF0%\xF7\xD8\xB2\x8F\x9FX`\x06rf\xE6\x95\xA4\x16\xA5\x16\x97\x14#\x99\x97_T\\\x9E_\x82\xC3<N\xA0yX\x9D\xCA\bv*\xD4\al\xAEE9\x89y\xE98\x14\x02\x8D\x808\x8A3'\x91D\xEFp@\xBD\xC3\xE9X\x94[\\R\x94\x9F\x97\x0E\xF4TF\x1D\xD8SY\x00K\x04Y\xA1"
```

And then we can have Joe Armstrong visit our page:

```ruby
joe.visit_page
```

The page visit counter did not exist prior to this method call, but the counter will be created (and incremented) all at once.

The problem with our `User` model so far is that we can't actually _retrieve_ any information about specific users from Riak. So let's create some getters that retrieve that information from Riak:

```ruby
class User
  # retain class methods from above

  def first_name
    @map.registers['first_name']
  end

  def last_name
    @map.registers['last_name']
  end

  def interests
    @map.sets['interests'].to_a
  end

  def visits
    @map.counters['visits'].value
  end
end
```

Now, we can create a new user and then access that user's characteristics _from Riak_:

```ruby
joe = User.new 'Joe', 'Armstrong', ['distributed systems', 'Erlang']
joe.first_name #=> "Joe"
joe.last_name #=> "Armstrong"
joe.interests #=> ["distributed systems", "Erlang"]
joe.visits #=> 0
joe.visit_page
joe.visits #=> 1
```

We can also create instance methods that add and remove specific interests:

```ruby
class User
  # retain class methods from above

  def add_interest interest
    @map.sets['interests'].add interest
  end

  def remove_interest interest
    unless !interests.include? interest
      @map.sets['interests'].remove interest
    end
  end
end
```

## Converting to JSON

If we wanted to connect our application to an in-browser interface, we would probably need to be able to convert any given `User` to JSON. So let's add an `as_json` method to our class:

```ruby
require 'json'

class User
  # class methods from above

  def as_json
    {
      :first_name => first_name,
      :last_name => last_name,
      :interests => interests,
      :visits => visits
    }.to_json
  end
end
```

Now, we can instantly convert our `User` map into a stringified JSON object and pipe it to our client-side application:

```ruby
bruce = User.new 'Bruce', 'Wayne', ['crime fighting', 'climbing', 'wooing Rachel Dawes']

bruce.as_json
#=>  "{"first_name":"Bruce","last_name":"Wayne","interests":["climbing","crime fighting","wooing Rachel Dawes"],"visits":0}"
```
