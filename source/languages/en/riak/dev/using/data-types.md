---
title: Using Datatypes
project: riak
version: 2.0.0+
document: tutorials
toc: true
audience: intermediate
keywords: [developers, datatypes]
---

In versions of Riak >= 2.0, Riak users can make use of a variety of Riak-specific datatypes inspired by research on convergent replicated data types ([[Datatypes]]). While Riak was originally built as a mostly data-agnostic key/value store, datatypes enable you to use Riak as a _data-aware_ system and thus to perform a variety of transactions on a range of datatypes.

In total, Riak supports five CRDT-inspired datatypes: [[counters|Datatypes#Counters]], [[flags|Datatypes#Flags]], [[registers|Datatypes#Registers]], [[sets|Datatypes#Sets]], and [[maps|Datatypes#Maps]]. Of those five types, counters, sets, and maps can be used as bucket-level datatypes, whereas flags and registers must be embedded in maps (more on that [[below|Using Datatypes#Maps]]).

<div class="note">
<div class="title">Note</div>
Counters are the one Riak datatype available in versions prior to 2.0, introduced in version 1.4. The implentation of counters in version 2.0 has been almost completely revamped, and so if you are using Riak 2.0+, we strongly recommend that you follow the usage documentation here rather than the documentation for the older version of counters.
</div>

## Setting Up Buckets to Use Riak Datatypes

In order to use Riak datatypes, you must first create a [[bucket type|Using Bucket Types]] that sets the `datatype` bucket parameter to either `counter`, `map`, or `set`.

The following would create a separate bucket type for each of the three bucket-level datatypes:

```bash
riak-admin bucket-type create map_bucket '{"props":{"datatype":"map"}}'
riak-admin bucket-type create set_bucket '{"props":{"datatype":"set"}}'
riak-admin bucket-type create counter_bucket '{"props":{"datatype":"counter"}}'
```

**Note**: The names `map_bucket`, `set_bucket`, and `counter_bucket` are _not_ reserved terms. You are always free to name bucket types whatever you like, with the exception of `default`.

Once you've created a datatype, you can check to make sure that the bucket property configuration (the `props` list) associated with that type is correct. This can be done either via HTTP or through the `riak-admin` interface.

Using `curl`, we can verify that the bucket `props` associated with the `map_bucket` type include `datatype` being set to `map`:

```curl
curl http://localhost:8098/types/map_bucket/props
```

If the `map_bucket` type exists and has been set up properly, we should see `"datatype":"map"` in the JSON response.

We can also see which properties are associated with a bucket type using the [[`riak-admin`|riak-admin Command Line]] interface, specifically the `status` command:

```bash
riak-admin bucket-type status map_bucket
```

Instead of JSON, this will return a simple list of properties and associated values, i.e. a list of `property: value` pairs. If our `map_bucket` bucket type has been set properly, we should see the following pair in our console output:

```bash
datatype: map
```

If a bucket type has been properly constructed, it needs to be activate to be usable in Riak. This can also be done using the `bucket-type` command interface:

```bash
riak-admin bucket-type activate map_bucket
```

To check whether activation has been successful, simply use the same `bucket-type status` command detailed above.

## Usage Examples

The examples below show you how to use Riak datatypes at the application level. Code samples are currently available in Ruby (using Basho's oficial [Riak Ruby client](https://github.com/basho/riak-ruby-client/tree/bk-crdt-doc)).

All examples will use the bucket type names from above (`counter_bucket`, `set_bucket`, and `map_bucket`).

### Registers and Flags

Registers and flags cannot be used on their own in Riak. That is, you cannot use a bucket/key pair as a register or flag directly. Instead, they must be used within a map. For usage examples, see the section on maps [[below|Using Datatypes#Maps]].

### Counters

Counters are a bucket-level Riak datatype that can be used either by themselves, i.e. associated with a bucket/key pair, or within a map. The examples below will show you how to use counters in both settings.

Now, whenever we use a bucket as a counter, it will know that the bucket needs to be assigned the `counter_bucket` type.

At this point, we can create and name a Riak bucket to house our counter (or as many counters as we'd like). We'll keep it simple and name our bucket `counters`:

```ruby
bucket = client.bucket 'counters'
```

To create a counter, you need to specific a bucket/key pair to hold that counter. Here is the general syntax for that:

```ruby
counter = Riak::Crdt::Counter.new bucket, key, bucket_type
```

Let's say that we want to create a counter called `traffic_tickets` in our `counters` bucket to keep tabs on our legal misbehavior. We can create this counter and ensure that the `counters` bucket will use our `counter_bucket` datatype like this:

```ruby
counter = Riak::Crdt::Counter.new counters, 'traffic_tickets', 'counter_bucket'

# Alternatively, the Ruby client enables you to set a bucket type as being
# globally associated with a Riak datatype. The following would set all
# counter buckets to use the counter_bucket bucket type:

Riak::Crdt::DEFAULT_BUCKET_TYPES[:counter] = 'counter_bucket'
to create our
# This would enable us to create our counter without specifying a bucket type:

counter = Riak::Crdt::Counter.new counters, 'traffic_tickets'
```

Now that our client knows which bucket/key pairing to use for our counter, `traffic_tickets` will start out at 0 by default. If we happen to get a ticket that afternoon, we would need to increment the counter:

```ruby
counter.increment
```

The default value of an increment operation is 1, but you can increment by more than one if you'd like (but always by an integer). Let's say that we decide to spend an afternoon flaunting traffic laws and manage to rack up five tickets:

```ruby
counter.increment 5
```

If we're curious about how many tickets we have accumulated, we can simply retrieve the value of the counter at any time:

```ruby
counter.value
# Output will always be an integer
```

Any good counter needs to decrement in addition to increment, and Riak counters allow you to do precisely that. Let's say that we hire an expert who manages to get a traffic ticket stricken from the record:

```ruby
counter.decrement

# Just like incrementing you can also decrement by more than one, e.g.:

counter.decrement 3
```

Operations on counters can be performed singly, as above, but let's say that we now have two counters in play, `dave_traffic_tickets` and `susan_traffic_tickets`. Both of these ne'er-do-wells get a traffic ticket on the same day and we need to increment both counters:

```ruby
counters = [dave_traffic_tickets, susan_traffic_tickets]

counters.each do |c|
  c.increment
end
```

### Sets

As with counters (and maps, as shown below), using sets involves setting up a bucket/key pair to house a set and running set-specific operations on that pair.

Here is the general syntax for setting up a bucket/key combination to handle a set:

```ruby
# Note: both the Riak Ruby Client and Ruby the language have a class called Set. Make sure that you refer to the Ruby version as ::Set and the Riak client version as Riak::Crdt::Set

set = Riak::Crdt::Set.new bucket, key, bucket_type
```

Let's say that we want to use a set to store a list of cities that we want to visit. Let's create a Riak set, simply called `set`, stored in the key `cities` in the bucket `travel` (using the `set_bucket` bucket type we created in the previous section):

```ruby
travel = client.bucket 'travel'
set = Riak::Crdt::Set.new travel, 'cities', 'set_bucket'

# Alternatively, the Ruby client enables you to set a bucket type as being
# globally associated with a Riak datatype. The following would set all
# set buckets to use the set_bucket bucket type:

Riak::Crdt::DEFAULT_BUCKET_TYPES[:set] = 'set_bucket'

# This would enable us to create our set without specifying a bucket type:

set = Riak::Crdt::Set.new travel, 'cities'
```

Upon creation, our set is empty. We can verify that it is empty at any time:

```ruby
set.empty?

# true
```

But let's say that we read a travel brochure saying that Toronto and Montreal are nice places to go. Let's add them to our `cities` set:

```ruby
set.add 'Toronto'
set.add 'Montreal'
```

Later on, we hear that Hamilton and Ottawa are nice cities to visit in Canada, but if we visit them, we won't have time to visit Montreal. Let's remove Montreal and add the others:

```ruby
set.remove 'Montreal'
set.add 'Hamilton'
set.add 'Ottawa'
```

Now, we can check on which cities are currently in our set:

```ruby
set.members

# This returns:

#<Set: {"Hamilton", "Ottawa", "Toronto"}>
```

Or we can see whether our set includes a specific member:

```ruby
set.include? 'Vancouver'
# false

set.include? 'Ottawa'
# true
```

We can also determine the size of the set:

```ruby
set.members.length
```

Or we can add a city---or multiple cities---to multiple sets. Let's say that Dave and Jane have visited Cairo and Beijing, and that we wish to add both cities to the `dave_cities_visited` and `jane_cities_visited` sets (provided that those sets have been created):

```ruby
[dave_cities_visited, jane_cities_visited].each do |set|
  ['Cairo', 'Beijing'].each do |city|
    set.add city
  end
end
```

You can also turn a set into a an array (or list, depending on the language):

```ruby
set.to_a

# Returns an Array:
# ["city1", "city2", "etc"]
```

### Maps

The map is in many ways the richest of the Riak datatypes because all of the other datatypes can be embedded within them, _including maps themselves_, to create arbitrarily complex custom datatypes out of a few basic building blocks.

The semantics of dealing with counters, sets, and maps within maps are usually very similar to working with those types at the bucket level, and so usage is usually very intuitive.

The general syntax for creating a Riak map is directly analogous to the syntax for creating other datatypes:

```ruby
map = Riak::Crdt::Map.new bucket, key
```

Let's say that we want to use Riak to store information about our company's customers. We'll use the bucket `customers` to do so. Each customer's data will be contained in its own key in the `customers` bucket. Let's create a map for the user Ahmed (`ahmed_info`) in our bucket and simply call it `map` for simplicity's sake (we'll use the `map_bucket` bucket type from above):

```ruby
customers = client.bucket 'customers'
map = Riak::Crdt::Map.new customers, 'ahmed_info', 'map_bucket'

# Alternatively, the Ruby client enables you to set a bucket type as being
# globally associated with a Riak datatype. The following would set all
# map buckets to use the map_bucket bucket type:

Riak::Crdt::DEFAULT_BUCKET_TYPES[:map] = 'map_bucket'

# This would enable us to create our map without specifying a bucket type:

map = Riak::Crdt::Map.new customers, 'ahmed_info'
```

#### Registers Within Maps

The first piece of info we want to store in our map is Ahmed's name and phone number, both of which are best stored as registers:

```ruby
map.registers['first_name'] = 'Ahmed'
map.registers['phone_number'] = '5551234567'

# Integers need to be stored as strings and then converted back when the data is retrieved. The following would work as well:

map.registers['phone_number'] = 5551234567.to_s
```

This will work even though registers `first_name` and `phone_number` did not previously exist, as Riak will create those registers for you.

### Counters Within Maps

We also want to know how many times Ahmed has visited our website. We'll use a `page_visits` counter for that and run the following operation when Ahmed visits our page for the first time:

```ruby
map.counters['page_visits'].increment

# This operation may return false even if successful
```

Even though the `page_visits` counter did not exist previously, the above operation will create it (with a default starting point of 0) and the increment operation will bump the counter up to 1.

#### Flags Within Maps

Now let's say that we add an Enterprise plan to our pricing model. We'll create an `enterprise_customer` flag to track whether Ahmed has signed up for the new plan. He hasn't yet, so we'll set it to `false`:

```ruby
map.flags['enterprise_customer'] = false
```

We can retrieve the value of that flag at any time:

```ruby
map.flags['enterprise_customer']

# false
```

#### Sets Within Maps

We'd also like to know what Ahmed's interests are so that we can better design a user experience for him. Through his purchasing decisions, we find out that Ahmed likes robots, opera, and motorcyles. We'll store that information in a set inside of our map:

```ruby
%w{ robots opera motorcycles }.each do |interest|
  map.sets['interests'].add interest
end
```

We can then verify that the `interests` set includes these three interests:

```ruby
%w{ robots opera motorcycles }.each do |interest|
  map.sets['interests'].include? interest
end

# This will return three Boolean values
```

We learn from a recent purchasing decision that Ahmed actually doesn't seem to like opera. He's much more keen on indie pop. Let's change the `interests` set to reflect that:

```ruby
map.sets['interests'].remove 'opera'

# This operation may return false even if successful

map.sets['interests'].add 'indie pop'
```

#### Maps Within Maps (Within Maps?)

We've stored a wide of variety of information---of a wide variety of types---within the `ahmed_info` map thus far, but we have yet to explore recursively storing maps within maps (which can be nested as deeply as you wish).

Our company is doing well and we have lots of useful information about Ahmed, but now we want to store information about Ahmed's contacts as well. We'll start with storing some information about Ahmed's colleague Annika inside of a map called `annika_info`.

First, we'll store Annika's first name, last name, and phone number in registers:

```ruby
map.maps['annika_info'].registers['first_name'] = 'Annika'
map.maps['annika_info'].registers['last_name'] = 'Weiss'
map.maps['annika_info'].registers['phone_number'] = 5559876543.to_s
```

The value of a register in a map can be obtained without a special method:

```ruby
map.maps['annika_info'].registers['first_name']

# "Annika"
```

Registers can also be removed:

```ruby
map.maps['annika_info'].registers.remove 'phone_number'
```

Now, we'll store whether or not Annika is subscribed to a variety of plans within the company as well:

```ruby
map.maps['annika_info'].flags['enterprise_plan'] = false
map.maps['annika_info'].flags['family_plan'] = false
map.maps['annika_info'].flags['free_plan'] = true
```

The value of a flag can be retrieved at any time:

```ruby
map.maps['annika_info'].flags['enterprise_plan']

# false
```

It's also important to track the number of purchases that Annika has made with our company. Annika just made her first widget purchase:

```ruby
map.maps['annika_info'].counters['widget_purchases'].increment
```

Now let's store Annika's interests in a set:

```ruby
map.maps['annika_info'].sets['interests'].add 'tango dancing'
```

We can remove that interest in just the way that we would expect:

```ruby
map.maps['annika_info'].set['interests'].remove 'tango dancing'
```

If we wanted to add store information about one of Annika's specific purchases, we could do so within a map:

```ruby
map.maps['annika_info'].maps['purchase'].flags['first_purchase'] = true
map.maps['annika_info'].maps['purchase'].register['amount'] = 1271.to_s
map.maps['annika_info'].maps['purchase'].sets['items'].add 'large_widget'
# and so on
```
