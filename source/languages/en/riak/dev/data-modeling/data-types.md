---
title: Data Modeling with Riak Data Types
project: riak
version: 2.0.0+
document: tutorial
audience: intermediate
keywords: [use-cases, developers, data-modeling, datatypes]
---

In our [[tutorial on Riak Data Types|Using Data Types]], we show you how
to create and perform a variety of operations on each of the
CRDT-inspired [[Data Types]] available in Riak 2.0 and later:
[[registers|Data Types#Registers]], [[flags|Data Types#Flags]],
[[counters|Data Types#Counters]], [[sets|Data Types#Sets]], and
[[maps|Data Types#Maps]].

While that tutorial covers the basics of using Data Types, most
real-world applications would need to use Data Types in a more
structured and less ad hoc way. Here, we'll walk through a basic example
of how an application might approach Data Types in conjunction with
application-side data models, creating a `User` type as the basis for a
CRM-style user information store.

## Creating the Basic Data Model

We can begin by creating a new type, `User`, that will house the
following information about each user:

* first name
* last name
* interests
* visits to our site
* whether the user has a paid account

We can see from the above that a `User` is best modeled as a Riak map
because maps can hold a variety of Data Types within them, in our case a
few strings (best modeled as [[registers|Data Types#Registers]]), an
array (best modeled as a [[set|Data Types#Set]]), and a Boolean (best
modeled as a [[flag|Data Types#Flags]]). Maps can also house other maps,
but that will not be covered in this tutorial.

Our basic modeling approach will be to create a `User` class that ties
any given `User` object directly to a map in Riak. From there, we'll
create class methods that define interactions with Riak, so that the map
can be properly updated when updates are made to the `User` object.

## Connecting Our Data Model to Riak

The first step in connecting our data model to Riak is the same step
that is always involved with using Riak maps. We need to create a bucket
type suited for maps, i.e. with the `datatype` property set to `map`,
which is covered in our tutorial on [[using bucket types]].

Once the bucket type is ready (we'll name it `maps` for the sake of
simplicity, although you can name yours whatever you'd like), we need to
create a client to connect to Riak. For this tutorial, we'll use
`localhost` as our host and `8087` as our [[protocol buffers|PBC API]]
port:

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

```csharp
TODO
```

<div class="note">
<div class="title">Getting started with Riak clients</div>
If you are connecting to Riak using one of Basho's official [[client
libraries]], you can find more information about getting started with
your client in our [[quickstart guide|Five-Minute
Install#setting-up-your-riak-client]].
</div>

Now, we can begin connecting our data model to a Riak map. We can do
that by creating a reference to a bucket type, bucket, and key. We
already know which bucket type we're using (`maps`) from above. So from
there we need to choose a bucket and key. In this tutorial, we'll assume
that all user maps are stored in the bucket `maps`, and for the key
we'll do something a bit more creative: we'll construct a key out of
each user's first and last name, with an underscore in the middle. And
so the map for the user Brian May would have the key `brian_may`. Below,
we'll start building our class, initializing the class with a reference
to the appropriate map:


```ruby
class User
  def initialize(first_name, last_name)
    key = "#{first_name}_#{last_name}"
    @map = Riak::Crdt::Map.new($client.bucket('users'), key)
  end
end
```

```python
class User:
    def __init__(self, first_name, last_name):
        bucket = client.bucket_type('maps').bucket('users')
        key = "{}_{}".format(first_name, last_name)

        # The Python client can use the new() function to automatically
        # detect that a map is being targeted by the client
        user_map = bucket.new(key)
```

```java
public class User {
	private String key;
	private Location location;

	public User(String firstName, String lastName) {
		String key =
			String.format("%s_%s", firstName.toLowerCase(), lastName.toLowerCase());

		this.key = key;

        // In the Java client, maps are updated on the basis of the
        // map's bucket type/bucket/key Location:
		this.location = new Location(new Namespace(bucketType, bucket), key);
	}
}
```

```csharp
TODO
```

## Storing An Object's Properties in Our Riak Map

At this point, we have a Riak map associated with instantiations of our
`User` type, but that map will be empty. Let's modify our initializer
function to populate [[registers|Data Types#Registers]] in the map with
first name and last name information:

```ruby
class User
  def initialize(first_name, last_name)
    key = "#{first_name}_#{last_name}"
    @map = Riak::Crdt::Map.new($client.bucket 'users', key)
    @map.batch do |m|
      m.registers['first_name'] = first_name
      m.registers['last_name'] = last_name
    end
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

        # Thus far, all changes to the user_map object have only been
        # made locally. To commit them to Riak, we have to use the
        # store() method. You can alter Riak Data Types as much as you
        # wish on the client side prior to committing those changes to
        # Riak.
        user_map.store()
```

```java
public class User {
	private String key;
	private Location location;

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

    /**
     * Fetches our map's abstract context object, which assists Riak in
     * making intelligent decisions about map convergence behind the
     * scenes. This will assist us later in the tutorial.
     */

	private Context getMapContext() throws Exception {
		FetchMap fetch = new FetchMap.Builder(location).build();
		return client.execute(fetch).getContext();
	}

    /**
     * Updates our map using the abstract context object fetched using
     * the private getMapContext() function above.
     */

	private void updateMapWithContext(MapUpdate mu) throws Exception {
		Context ctx = getMapContext();
		UpdateMap update = new UpdateMap.Builder(location, mu)
				.withContext(ctx)
				.build();
		client.execute(update);
	}

    /**
     * Updates our map without an abstract context object. Context is not
     * needed for some map updates.
     */

	private void updateMapWithoutContext(MapUpdate mu) throws Exception {
		UpdateMap update = new UpdateMap.Builder(location, mu).build();
		client.execute(update);
	}
}
```

```csharp
TODO
```

Now, if we create a new user, that user will have a `map` instance
variable attached to it, the `first_name` and `last_name` strings will
be stored in Riak registers, and the key will be `Bruce_Wayne`:

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

```csharp
TODO
```

So now we have our `first_name` and `last_name` variables stored in our
map, but we still need to account for `interests` and `visits`. First,
let's modify our class definition to store each user's interests in a
set within the map:

```ruby
class User
  def initialize first_name, last_name, interests
    @key = "#{first_name}_#{last_name}"
    @map = Riak::Crdt::Map.new($client.bucket 'users', @key)

    # We'll use a batch function here to avoid making more trips to Riak
    # than we need to. We highly recommend using batch functions of this
    # sort whenever possible.
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
	// Retaining our getMapContext() and other functions from above:

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

  /**
   * Transforms a Set of Strings into a SetUpdate that can be sent to
   * Riak:
   */

	private SetUpdate setIntoSetUpdate(Set<String> rawSet) {
		SetUpdate su = new SetUpdate():
    for (String item : rawSet) {
      su.add(BinaryValue.create(item));
    }
		return su;
	}
}
```

```csharp
TODO
```

Now when we create new users, we need to specify their interests as a
list:

```ruby
joe = User.new('Joe', 'Armstrong', ['distributed systems', 'Erlang'])
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

```csharp
TODO
```

Our `visits` variable will work a little bit differently, because when a
new user is created the value will simply be zero. This is true of _all_
Riak counters. If you fetch the value of a counter that has not yet been
modified, it will be zero, even if you query a counter in a random
bucket and key. Let's create a new instance method that increments the
`visits` counter by one every time it is called:

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

        // To decrement a counter, pass a negative number to the
        // CounterUpdate object

		MapUpdate mu = new MapUpdate()
				.update("visits", cu);

		// Using our updateMapWithoutContext method from above, as
        // context is not necessary for counter updates
		updateMapWithoutContext(mu);
	}
}
```

```csharp
TODO
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

```csharp
TODO
```

The page visit counter did not exist prior to this method call, but the
counter will be created (and incremented) all at once.

Finally, we need to include `paid_account` in our map as a [[flag|Data
Types#Flags]]. Each user will initially be added to Riak as a non-paying
user, and we can create methods to upgrade and downgrade the user's
account at will:

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
	// Retaining all of the class methods from above

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

```csharp
TODO
```

The problem with our `User` model so far is that we can't actually
_retrieve_ any information about specific users from Riak. So let's
create some getters to do that:

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

    /**
     * Fetches our map from Riak in its current state, which enables us
     * to fetch current values for all of the fields of the map, as
     * in the methods below.
     */

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

	public boolean getAccountStatus() {
		return getMap().getFlag("paid_account").view();
	}
}
```

```csharp
TODO
```

Now, we can create a new user and then access that user's
characteristics directly from our Riak map:

```ruby
joe = User.new('Joe', 'Armstrong', ['distributed systems', 'Erlang'])
joe.first_name #=> "Joe"
joe.last_name #=> "Armstrong"
joe.interests #=> ["distributed systems", "Erlang"]
joe.visits #=> 0
joe.visit_page
joe.visits #=> 1
joe.paid_account #=> fase
```

```python
joe = User('Joe', 'Armstrong', ['distributed systems', 'Erlang'])
joe.first_name # 'Joe'
joe.last_name # 'Armstrong'
joe.interests # frozenset(['Erlang', 'distributed systems'])
joe.visits # 0
joe.visit_page()
joe.visits # 1
joe.paid_account # false
```

```java
Set<String> interests = new HashSet<String>();
interests.add("distributed systems");
interests.add("Erlang");
User joe = new User("Joe", "Armstrong", interests);

joe.getFirstName(); // Joe
joe.getLastName(); // Armstrong
joe.getInterests(); // ["distributed systems", "Erlang"]
joe.getVisits(); // 0
joe.visitPage();
joe.getVisits(); // 1
joe.getAccountStatus(); // false
```

```csharp
TODO
```

We can also create instance methods that add and remove specific
interests:

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
	// Retaining all of the class methods from above

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

```csharp
TODO
```

## Converting to JSON

If we wanted to connect our application to an in-browser interface, we
would probably need to be able to convert any given `User` to JSON. So
let's add a JSON conversion method to our class:

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

```java
// For JSON generation, we'll use the Jackson JSON library

import com.fasterxml.jackson.databind.ObjectMapper;

class User {
	// Retaining all of the class methods from above

    public String toJson() throws Exception {
        ObjectMapper jsonMapper = new ObjectMapper();

        Map<String, Object> userJsonMap = new HashMap<String, Object>();

        RiakMap userRiakMap = getMap();

        userJsonMap.put("firstName", userRiakMap.getRegister("first_name").getValue().toString());
        userJsonMap.put("lastName", userRiakMap.getRegister("last_name").getValue().toString());
        userJsonMap.put("interests", userRiakMap.getSet("interests").viewAsSet());
        userJsonMap.put("visits", userRiakMap.getCounter("visits").view());
        userJsonMap.put("paidAccount", userRiakMap.getFlag("paid_account").view());

        return jsonMapper.writeValueAsString(userJsonMap);
	}
}
```

```csharp
TODO
```

Now, we can instantly convert our `User` map into a stringified JSON
object and pipe it to our client-side application:

```ruby
bruce = User.new('Bruce', 'Wayne', ['crime fighting', 'climbing stuff'])
bruce.visit_page
bruce.as_json
#=>  "{"first_name":"Bruce","last_name":"Wayne","interests":["climbing","crime fighting"],"visits":1}"
```

```python
bruce = User('Bruce', 'Wayne', ['crime fighting', 'climbing stuff'])
bruce.visit_page()
bruce.as_json()
# '{"interests": ["climbing", "crime fighting"], "lastName": "Wayne", "visits": 1, "firstName": "Bruce", "paidAccount": false}'
```

```java
Set<String> interests = new HashSet<String>();
interests.add("crime fighting");
interests.add("climbing stuff");
User bruce = new User("Bruce", "Wayne", interests);
bruce.toJson();
```

```csharp
TODO
```

