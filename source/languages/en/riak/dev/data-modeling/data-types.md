---
title: Data Modeling with Riak Data Types
project: riak
version: 2.0.0+
document: tutorial
audience: intermediate
keywords: [use-cases, developers, data-modeling, datatypes]
---

In our [[tutorial on Riak Data Types|Using Data Types]], we show you how to create and perform a variety of operations on each of the CRDT-inspired [[Data Types]] available in Riak 2.0 and later: [[registers|Data Types#Registers]], [[flags|Data Types#Flags]], [[counters|Data Types#Counters]], [[sets|Data Types#Sets]], and [[maps|Data Types#Maps]].

While that tutorial covers the basics of using Data Types, most real-world applications would need to use Data Types in a more structured and less ad hoc way. Here, we'll walk through a basic example of how an application might approach Data Types in conjunction with application-side data models, creating a `User` type as the basis for a CRM-style user information store.

## Creating the Basic Data Model

We'll begin with a `User` type that will house the following information about each user:

* first name
* last name
* interests
* visits to our site
* whether the user has a paid account

And so we can begin constructing our data model by creating a `User` class that provides us getters and setters for each of those characteristics:

```ruby
class User
  attr_accessor :first_name, :last_name, :interests, :visits, :paid_account
end
```

```python
class User:
    def __init__(self):
        self.first_name = None
        self.last_name = None
        self.interests = None
        self.visits = None
        self.paid_account = None
```

```java
public class User {
	public String firstName;
	public String lastName;
	public Set<String> interests;
	public Long visits;
	public boolean paidAccount;

	public User() {}

	public String getFirstName() {
		return this.firstName;
	}

	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	// and so on for the other attributes
}
```

This enables us to create new `User`s and modify those characteristics:

```ruby
bill = User.new
bill.first_name = 'Bill'
bill.last_name = 'Murray'
bill.interests = ['filming Caddyshack', 'being smartly funny']
bill.visits = 10
bill.paid_account = false
bill
# <User:0x007fb7f7885408 @first_name="Bill", @last_name="Murray", @interests=["filming Caddyshack", "being smartly funny"], @visits=10>
```

```python
bill = User()
bill.first_name = 'Bill'
bill.last_name = 'Murray'
bill.interests = ['filming Caddyshack', 'being smartly funny']
bill.visits = 10
bill.paid_account = False
bill
# <__main__.User instance at 0x1063febd8>
```

```java
User bill = new User();
bill.setFirstName("Bill");
bill.setLastName("Murray");
Set<String> interests = new HashSet<>();
interests.add("filming Caddyshack").add("being smartly funny");
bill.setInterests(interests);
bill.setVisits(10);
bill.setPaidAccount(false);
```

Amongst the Riak [[Data Types]], a `User` is best modeled as a map, because maps can hold a variety of Data Types within them, in our case a few strings (best modeled as [[registers|Data Types#Registers]]), an array (best modeled as a [[set|Data Types#Set]]), and a Boolean (best modeled as a [[flag|Data Types#Flags]]). Maps can also house other maps, but that will not be covered in this tutorial.

## Connecting Our Data Model to Riak

First, we need to create a bucket type suited for maps, i.e. with the `datatype` property set to `map`. More on that can be found in the [[Using Bucket Types]] tutorial.

Once the bucket type is ready (we'll name the bucket type `map_bucket` for our purposes here), we need to create a client to connect to Riak. For this tutorial, we'll use `localhost` as our host and `8087` as our [[protocol buffers|PBC API]] port:

```ruby
$client = Riak::Client.new(:host => 'localhost', :pb_port => 8087)
```

```python
from riak import RiakClient
client = RiakClient(protocol='pbc', pb_port=8087)
```

```java
public class User {
	private RiakClient client;

	public User() {
		// Assuming that you have initialized and started a RiakCluster object:

		this.client = new RiakClient(cluster);
	}
}
```

Now, we can begin connecting our data model to a Riak map. We'll do that creating a map whenever a new `User` object is created:

```ruby
class User
  def initialize
    @map = Riak::Crdt::Map.new $bucket, '<key>', 'map_bucket'
  end
end

# In the example above, the map was created specifying a bucket, key, and bucket type. An alternative way of creating maps is to specify a bucket type that will be used for ALL maps:

Riak::Crdt::DEFAULT_BUCKET_TYPES[:map] = 'map_bucket'

# If this parameter is set, you no longer need to specify a bucket type when creating maps. The rest of this tutorial will proceed assum
```

```python
from riak.datatypes import Map

class User:
    def __init__(self):
        bucket = client.bucket_type('map_bucket').bucket('<bucket>')
        self.user_map = Map(bucket, '<key>')
```

```java
public class User {
	final private String bucket = "users";
	final private String bucketType = "maps";
	private String key;
	private Location location;

	public User(String firstName, String lastName) {
		this.key = String.format("%s_%s", firstName.toLowerCase(), lastName.toLowerCase());
		this.location = new Location(new Namespace(bucketType, bucket), key);
	}
}
```

Note that we haven't specified under which key our map will be stored. In a key/value store like Riak, choosing a key is very important. We'll keep it simple here and use a string consisting of first and last name (separate by an underscore) as the key:

```ruby
class User
  def initialize first_name, last_name
    @key = "#{first_name}_#{last_name}"
    @map = Riak::Crdt::Map.new($client.bucket 'users', @key)
  end
end
```

```python
class User:
    def __init__(self, first_name, last_name):
        bucket = client.bucket_type('map_bucket').bucket('users')
        key = "{}_{}".format(first_name, last_name)

        # The Python client can use the new() function to automatically
        # detect that a map is being targeted by the client
        user_map = bucket.new(key)
```

```java
public class User {
	public User(String firstName, String lastName) {
		this.key = String.format("%s_%s", firstName.toLowerCase(), lastName.toLowerCase());
		this.location = new Location(new Namespace(bucketType, bucket), key);

		// In the Java client, maps are updated on the basis of the map's
		// location, we specified in the line directly above
	}
}
```

## Storing An Object's Properties in Our Riak Map

At this point, we have a Riak map associated with instantiations of our `User` type, but that map will be empty. Let's modify our initializer function to populate registers in the map with first name and last name information:

```ruby
class User
  def initialize first_name, last_name
    @key = "#{first_name}_#{last_name}"
    @map = Riak::Crdt::Map.new($client.bucket 'users', @key)
    @map.registers['first_name'] = first_name
    @map.registers['last_name'] = last_name
  end
end
```

```python
class User:
    def __init__(self, first_name, last_name):
        bucket = client.bucket_type('maps').bucket('users')
        key = "{}_{}".format(first_name, last_name)
        user_map = Map(bucket, key)
        user_map.registers['first_name'].assign(first_name)
        user_map.registers['last_name'].assign(last_name)
        user_map.store()
```

```java
public class User {
	private Context getMapContext() throws Exception {
		FetchMap fetch = new FetchMap.Builder(location).build();
		return client.execute(fetch).getContext();
	}

	private void updateMapWithContext(MapUpdate mu) throws Exception {
		Context ctx = getMapContext();
		UpdateMap update = new UpdateMap.Builder(location, mu)
				.withContext(ctx)
				.build();
		client.execute(update);
	}

	private void updateMapWithoutContext(MapUpdate mu) throws Exception {
		UpdateMap update = new UpdateMap.Builder(location, mu).build();
		client.execute(update);
	}

	public User(String firstName, String lastName) {
		this.key = String.format("%s_%s", firstName.toLowerCase(), lastName.toLowerCase());
		this.location = new Location(new Namespace(bucketType, bucket), key);
		RegisterUpdate ru1 = new RegisterUpdate(BinaryValue.create(firstName));
		RegisterUpdate ru2 = new RegisterUpdate(BinaryValue.create(lastName));
		MapUpdate mu = new MapUpdate()
				.update("first_name", ru1)
				.update("last_name", ru2);
		updateMapWithoutContext(mu);
	}
}
```

Now, if we create a new user, that user will have a `map` instance variable attached to it, the `first_name` and `last_name` strings will be stored in Riak registers, and the key will be `Bruce_Wayne`:

```ruby
bruce = User.new 'Bruce', 'Wayne'
#=> #<User:0x007fe2965cafc8 @map=#<Riak::Crdt::Map:0x007fe2965caf78 @bucket=#<Riak::Bucket {users}>, @key"Bruce_Wayne", @bucket_type"map", @options{}, @dirtyfalse, @counters#<Riak::Crdt::TypedCollection:0x007fe296125ae0 @type=Riak::Crdt::InnerCounter, @parent=#<Riak::Crdt::Map:0x007fe2965caf78 ...>, @contents{}, @flags#<Riak::Crdt::TypedCollection:0x007fe2961257c0 @type=Riak::Crdt::InnerFlag, @parent=#<Riak::Crdt::Map:0x007fe2965caf78 ...>, @contents{}, @maps#<Riak::Crdt::TypedCollection:0x007fe296125428 @type=Riak::Crdt::InnerMap, @parent=#<Riak::Crdt::Map:0x007fe2965caf78 ...>, @contents{}, @registers#<Riak::Crdt::TypedCollection:0x007fe296124e60 @type=Riak::Crdt::InnerRegister, @parent=#<Riak::Crdt::Map:0x007fe2965caf78 ...>, @contents{"last_name"=>"Wayne", "first_name"=>"Bruce"}, @sets#<Riak::Crdt::TypedCollection:0x007fe296124460 @type=Riak::Crdt::InnerSet, @parent=#<Riak::Crdt::Map:0x007fe2965caf78 ...>, @contents{}, @context"M\x01\x83P\x00\x00\x00\xC4x\x01\xCB`\xCAa```\xCC`\xCA\x05R\x1CG\x836r\a3\x1F\xB1Od\xC9\x02\t3e\x00!H\x82+-\xB3\xA8\xB8$>/175\x85\x81\xAF(31;>\xA5$>\xA7\xBC\xBC(5=\x03\xBB\t\xCCY\x10\xAD\xACNE\xA5\xC9\xA9y\xEC\fB%\xEE\xD1\x1F?\xB1\xC0\x8C\xE4\xCCI$\xD1D\x16\x98\x89\xE1\x89\x95y \x13y\x9B\xC0&f\x01\x00\xAF\x055\xA8"
```

```python
bruce = User('Bruce', 'Wayne')
```

```java
User bruce = new User("Bruce", "Wayne");
```

So now we have our `first_name` and `last_name` variables stored in our map, but we still need to account for `interests` and `visits`. First, let's modify our class definition to store each user's interests in a set within the map:

```ruby
class User
  def initialize first_name, last_name, interests
    @key = "#{first_name}_#{last_name}"
    @map = Riak::Crdt::Map.new($client.bucket 'users', @key)
    # We'll use a batch function here to avoid making more trips to Riak than we need to
    @map.batch do |m|
      m.registers['first_name'] = first_name
      m.registers['last_name'] = last_name
      interests.each do |i|
        m.sets['interests'].add i
      end
    end
  end
end  
```

```python
class User:
    def __init__(self, first_name, last_name, interests):
        bucket = client.bucket_type('maps').bucket('users')
        key = "{}_{}".format(first_name, last_name)
        user_map = Map(bucket, key)
        user_map.registers['first_name'].assign(first_name)
        user_map.registers['last_name'].assign(last_name)
        for interest in interests:
            user_map.sets['interests'].add(interest)
        user_map.store()
```

```java
public class User {
	// Retaining our getMapContext() and other functions from above

	private SetUpdate setIntoSetUpdate(Set<String> rawSet) {
		SetUpdate su = new SetUpdate():
		rawSet.forEach((String item) -> {
			su.add(BinaryValue.create(item));
		});
		return su;
	}

	public User(String firstName, String lastName, Set<String> interests) {
		this.key = String.format("%s_%s", firstName.toLowerCase(), lastName.toLowerCase());
		this.location = new Location(new Namespace(bucketType, bucket), key);
		RegisterUpdate ru1 = new RegisterUpdate(BinaryValue.create(firstName));
		RegisterUpdate ru2 = new RegisterUpdate(BinaryValue.create(lastName));
		SetUpdate su = setIntoSetUpdate(rawSet);		
		MapUpdate mu = new MapUpdate()
				.update("first_name", ru1)
				.update("last_name", ru2)
				.update("interests", su);
		updateMapWithoutContext(mu);
	}
}
```

Now when we create new users, we need to specify their interests as a list:

```ruby
joe = User.new 'Joe', 'Armstrong', ['distributed systems', 'Erlang']
#=> #<User:0x007f9a4b81ead8 @map=#<Riak::Crdt::Map:0x007f9a4b81ea88 @bucket=#<Riak::Bucket {users}>, @key"\#{first_name}\#{last_name}", @bucket_type"map", @options{}, @dirtyfalse, @counters#<Riak::Crdt::TypedCollection:0x007f9a4b89fae8 @type=Riak::Crdt::InnerCounter, @parent=#<Riak::Crdt::Map:0x007f9a4b81ea88 ...>, @contents{}, @flags#<Riak::Crdt::TypedCollection:0x007f9a4b89f8b8 @type=Riak::Crdt::InnerFlag, @parent=#<Riak::Crdt::Map:0x007f9a4b81ea88 ...>, @contents{}, @maps#<Riak::Crdt::TypedCollection:0x007f9a4b89f688 @type=Riak::Crdt::InnerMap, @parent=#<Riak::Crdt::Map:0x007f9a4b81ea88 ...>, @contents{}, @registers#<Riak::Crdt::TypedCollection:0x007f9a4b89f4a8 @type=Riak::Crdt::InnerRegister, @parent=#<Riak::Crdt::Map:0x007f9a4b81ea88 ...>, @contents{"last_name"=>"Armstrong", "first_name"=>"Joe"}, @sets#<Riak::Crdt::TypedCollection:0x007f9a4b89f0e8 @type=Riak::Crdt::InnerSet, @parent=#<Riak::Crdt::Map:0x007f9a4b81ea88 ...>, @contents{"interests"=>#<Riak::Crdt::InnerSet:0x007f9a4b89ee90 @parent=#<Riak::Crdt::TypedCollection:0x007f9a4b89f0e8 ...>, @value#<Set: {"Erlang"}, @name="interests">}, @context"M\x01\x83P\x00\x00\x01Ex\x01\xCB`\xCAa```\xCC`\xCA\x05R\x1CG\x836r\a3\x1F\xB1O\xE4\xCC\x02\t3g0A$\xB8\xD22\x8B\x8AK\xE2\xF3\x12sSS\x18\xF8\x8A2\x13\xB3\xE3SJ\xE2s\xCA\xCB\x8BR\xD33\xB0\x9B\xC0\x9E\x05\xD1\xCA\xEC\x95\x9F\x9A\xC7\xCE\xF0%\xF7\xD8\xB2\x8F\x9FX`\x06rf\xE6\x95\xA4\x16\xA5\x16\x97\x14#\x99\x97_T\\\x9E_\x82\xC3<N\xA0yX\x9D\xCA\bv*\xD4\al\xAEE9\x89y\xE98\x14\x02\x8D\x808\x8A3'\x91D\xEFp@\xBD\xC3\xE9X\x94[\\R\x94\x9F\x97\x0E\xF4TF\x1D\xD8SY\x00K\x04Y\xA1"
```

```python
joe = User('Joe', 'Armstrong', ['distributed systems', 'Erlang'])
```

```java
Set<String> interests = new HashSet<String>();
interests.add("distributed systems");
interests.add("Erlang");
User joe = new User("Joe", "Armstrong", interests);
```

Our `visits` variable will work a little bit differently, because when a new user is created the value will simply be zero. So let's create a new instance method `visit_page` that increments the `visits` counter by one every time it is called:

```ruby
class User
  def initialize first_name, last_name, interests
    @map = Riak::Crdt::Map.new($client.bucket 'users', "#{first_name}_#{last_name}")
    @map.batch do |m|
      m.registers['first_name'] = first_name
      m.registers['last_name'] = last_name
      interests.each do |i|
        m.sets['interests'].add i
      end
    end
  end

  def visit_page
    @map.counters['visits'].increment
  end
end 
```

```python
class User:
    def __init__(self, first_name, last_name, interests):
        bucket = client.bucket_type('maps').bucket('users')
        key = "{}_{}".format(first_name, last_name)
        self.user_map = Map(bucket, key)
        self.user_map.registers['first_name'].assign(first_name)
        self.user_map.registers['last_name'].assign(last_name)
        for interest in interests:
            self.user_map.sets['interests'].add(interest)
        self.user_map.store()

    def visit_page(self):
        self.user_map.counters['visits'].increment()
        self.user_map.store()
```

```java
public class User {
	public User() {
		// Retaining from above
	}

	public void visitPage() {
		CounterUpdate cu = new CounterUpdate(1);
		MapUpdate mu = new MapUpdate()
				.update("visits", cu);

		// Using our updateMapWithoutContext from above
		updateMapWithoutContext(mu);
	}
}
```

And then we can have Joe Armstrong visit our page:

```ruby
joe.visit_page
```

```python
joe.visit_page()
```

```java
joe.visitPage();
```

The page visit counter did not exist prior to this method call, but the counter will be created (and incremented) all at once.

Finally, we need to include `paid_account` in our map as a [[flag|Data Types#Flags]]. Each user will initially be added to Riak as a non-paying user, and we can create methods to upgrade and downgrade the user's account:

```ruby
class User
  def initialize first_name, last_name, interests
    @map = Riak::Crdt::Map.new($client.bucket 'users', "#{first_name}_#{last_name}")
    @map.batch do |m|
      m.registers['first_name'] = first_name
      m.registers['last_name'] = last_name
      interests.each do |i|
        m.sets['interests'].add i
      end
      m.flags['paid_account'] = false
    end
  end

  def upgrade_account
    @map.flags['paid_account'] = true
  end

  def downgrade_account
    @map.flags['paid_account'] = false
  end
end
```

```python
class User:
    def __init__(self, first_name, last_name, interests):
        bucket = client.bucket_type('maps').bucket('users')
        key = "{}_{}".format(first_name, last_name)
        self.user_map = Map(bucket, key)
        self.user_map.registers['first_name'].assign(first_name)
        self.user_map.registers['last_name'].assign(last_name)
        for interest in interests:
            self.user_map.sets['interests'].add(interest)
        self.user_map.store()

    def upgrade_account(self):
        self.user_map.flags['paid_account'].enable()
        self.user_map.store()

    def downgrade_account(self):
        self.user_map.flags['paid_account'].disable()
        self.user_map.store()
```

```java
public class User {
	// Using all the material from above

	public void upgradeAccount() {
		FlagUpdate setToTrue = new FlagUpdate().set(true);
		Context ctx = getMapContext();
		MapUpdate mu = new MapUpdate()
				.withContext(ctx)
				.update("paid_account", setToTrue);
		updateMapWithContext(mu);
	}

	public void downgradeAccount() {
		FlagUpdate setToFalse = new FlagUpdate().set(false);
		MapUpdate mu = new MapUpdate()
				.withContext(ctx)
				.update("paid_account", setToFalse);
		updateMapWithContext(mu);
	}
}
```

The problem with our `User` model so far is that we can't actually _retrieve_ any information about specific users from Riak. So let's create some getters to do that:

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

  def paid_account
    @map.flags['paid_account']
  end
end
```

```python
class User:
    # retain class methods from above

    @property
    def first_name(self):
        return self.user_map.reload().registers['first_name'].value

    @property
    def last_name(self):
        return self.user_map.reload().registers['last_name'].value

    @property
    def interests(self):
        return self.user_map.reload().sets['interests'].value

    @property
    def visits(self):
        return self.user_map.reload().counters['visits'].value

    @property
    def paid_account(self):
        return self.user_map.reload().flags['paid_account'].value
```

```java
public class User {
	private RiakMap getMap() throws Exception {
		FetchMap fetch = new FetchMap.Builder(location).build();
		return client.execute(fetch).getDatatype();
	}

	public String getFirstName() {
		return getMap().getRegister("first_name").toString();
	}

	public String getLastName() {
		return getMap().getRegister("last_name").toString();
	}

	public Set<String> getInterests() {
		Set<String> setBuilder = new HashSet<String>();
		Set<BinaryValue> binarySet = getMap().getSet("interests").viewAsSet();
		binarySet.forEach((BinaryValue item) -> {
			setBuilder.add(item.toString());
		});
		return setBuilder;
	}

	public Long getVisits() {
		return getMap().getCounter("visits").view();
	}

	public boolean getPaidAccount() {
		return getMap().getFlag("paid_account").view();
	}
}
```

Now, we can create a new user and then access that user's characteristics directly from our Riak map:

```ruby
joe = User.new 'Joe', 'Armstrong', ['distributed systems', 'Erlang']
joe.first_name #=> "Joe"
joe.last_name #=> "Armstrong"
joe.interests #=> ["distributed systems", "Erlang"]
joe.visits #=> 0
joe.visit_page
joe.visits #=> 1
```

```python
joe = User('Joe', 'Armstrong', ['distributed systems', 'Erlang'])
joe.first_name # 'Joe'
joe.last_name # 'Armstrong'
joe.interests # frozenset(['Erlang', 'distributed systems'])
joe.visits # 0
joe.visit_page()
joe.visits # 1
```

```java
Set<String> interests = new HashSet<String>();
interests.add("distributed systems");
interests.add("Erlang");
User joe = new User("Joe", "Armstrong", interests);

joe.getFirstName();
joe.getLastName();
joe.getInterests();
joe.getVisits();
joe.visitPage();
joe.getVisits();
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

```python
class User:
    # retain class methods from above

    def add_interest(self, interest):
        self.user_map.sets['interests'].add(interest)
        self.user_map.store()

    def remove_interest(self, interest):
        self.user_map.sets['interests'].discard(interest)
        self.user_map.store()
```

```java
public class User {
	// Stuff from above

	public void addInterest(String interest) {
		SetUpdate su = new SetUpdate().add(BinaryValue.create(interest));
		MapUpdate mu = new MapUpdate()
				.update("interests", su);
		updateMapWithoutContext(mu);
	}

	public void removeInterest(String interest) {
		SetUpdate su = new SetUpdate().remove(BinaryValue.create(interest));
		Context ctx = getMapContext();
		MapUpdate mu = new MapUpdate()
				.withContext(ctx)
				.update("interests", mu);
		updateMapWithContext(mu);
	}
}
```

## Converting to JSON

If we wanted to connect our application to an in-browser interface, we would probably need to be able to convert any given `User` to JSON. So let's add a JSON conversion method to our class:

```ruby
require 'json'

class User
  # retain class methods from above

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

```python
import json

class User:
    # retain class methods from above

    def as_json():
      m = self.user_map.reload()
      user_dict = {
          'firstName': m.registers['first_name'].value,
          'lastName': m.registers['last_name'].value,
          'interests': list(m.sets['interests'].value),
          'visits': m.counters['visits'].value,
          'paidAccount': m.flags['paid_account'].value

      }
      return json.dumps(user_dict)
```

Now, we can instantly convert our `User` map into a stringified JSON object and pipe it to our client-side application:

```ruby
bruce = User.new 'Bruce', 'Wayne', ['crime fighting', 'climbing', 'wooing Rachel Dawes']
bruce.visit_page
bruce.as_json
#=>  "{"first_name":"Bruce","last_name":"Wayne","interests":["climbing","crime fighting","wooing Rachel Dawes"],"visits":1}"
```

```python
bruce = User('Bruce', 'Wayne', ['crime fighting', 'climbing', 'wooing Rachel Dawes'])
bruce.visit_page()
bruce.as_json()
# '{"interests": ["climbing", "crime fighting", "wooing Rachel Dawes"], "lastName": "Wayne", "visits": 1, "firstName": "Bruce", "paidAccount": false}'
```

## Accessing a Map Later On

While the `User` class created above will create a new map and automatically populate its fields. But in most applications, we would also need to be able to modify that map later without necessarily having access to the object, for example if we wanted to upgrade Bruce Wayne's plan in response to an HTTP request, e.g. a `PUT` request to a `/user/Bruce_Wayne` endpoint.

A map can be located and later modified on the basis of the key associated with it:

```ruby
# In the Ruby client, the Riak::Crdt::Map.new function can be used both to create a map where it does not exist OR to modify a map that already exists
map_to_modify = Riak::Crdt::Map.new '<bucket>' '<key>'

# In our case, we'll be using using the 'users' bucket, as above
bucket = $client.bucket 'users'
map_to_modify = Riak::Crdt::Map.new bucket, '<key>'
```

```python
# In the Python client, the Map() class can be used both to create a map where it does not exist OR to modify a map that already exists

map_to_modify = Map('<bucket>', '<key>')

# In our case, we'll be using the the 'users' bucket and 'maps' bucket type, as above
bucket = client.bucket_type('maps').bucket('users')
map_to_modify = Map(bucket, '<key>')
```

On that basis, we could upgrade the plan for Bruce Wayne:

```ruby
bucket = $client.bucket 'users'
user_to_modify = Riak::Crdt::Map.new bucket, 'Bruce_Wayne'
user_to_modify.flags['paid_account'] = true
```

```python
bucket = client.bucket_type('maps').bucket('users')
user_to_modify = Map(bucket, 'Bruce_Wayne')
user_to_modify.flags['paid_account'].enable()
user_to_modify.store()
```

This can also be done more programmatically:

```ruby
def upgrade_user_account bucket, user_key
  user_map_to_modify = Riak::Crdt::Map.new bucket, user_key
  user_map_to_modify.flags['paid_account'] = true
end
```

```python
def upgrade_user_account(bucket, user_key):
    user_map_to_modify = Map(bucket, user_key)
    user_map_to_modify.flags['paid_account'].enable()
    user_map_to_modify.store()
```
