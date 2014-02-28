---
title: Using Data Types
project: riak
version: 2.0.0+
document: tutorials
toc: true
audience: intermediate
keywords: [developers, data-types]
---

In versions of 2.0 and greater, Riak users can make use of a variety of Riak-specific data types inspired by research on convergent replicated data types ([[Data Types]]).

While Riak was originally built as a mostly data-agnostic key/value store, Riak Data Types enable you to use Riak as a _data-aware_ system and thus to perform a variety of transactions on five CRDT-inspired data types: [[counters|Data Types#Counters]], [[flags|Data Types#Flags]], [[registers|Data Types#Registers]], [[sets|Data Types#Sets]], and [[maps|Data Types#Maps]]. Of those five types, counters, sets, and maps can be used as bucket-level data types, whereas flags and registers must be embedded in maps (more on that [[below|Using Data Types#Maps]]).

<div class="note">
<div class="title">Note</div>
Counters are the one Riak Data Type available in versions prior to 2.0, introduced in version 1.4. The implentation of counters in version 2.0 has been almost completely revamped, and so if you are using Riak 2.0+, we strongly recommend that you follow the usage documentation here rather than documentation for the older version of counters.
</div>

## Setting Up Buckets to Use Riak Data Types

In order to use Riak data types, you must first create a [[bucket type|Using Bucket Types]] that sets the `datatype` bucket parameter to either `counter`, `map`, or `set`.

The following would create a separate bucket type for each of the three bucket-level data types:

```bash
riak-admin bucket-type create map_bucket '{"props":{"datatype":"map"}}'
riak-admin bucket-type create set_bucket '{"props":{"datatype":"set"}}'
riak-admin bucket-type create counter_bucket '{"props":{"datatype":"counter"}}'
```

**Note**: The names `map_bucket`, `set_bucket`, and `counter_bucket` are _not_ reserved terms. You are always free to name bucket types whatever you like, with the exception of `default`.

Once you've created a Riak Data Type, you can check to make sure that the bucket property configuration associated with that type is correct. This can be done through the `riak-admin` interface.

```bash
riak-admin bucket-type status map_bucket
```

This will return a list of bucket properties and their associated values in the form of `property: value`. If our `map_bucket` bucket type has been set properly, we should see the following pair in our console output:

```bash
datatype: map
```

If a bucket type has been properly constructed, it needs to be activated to be usable in Riak. This can also be done using the `bucket-type` command interface:

```bash
riak-admin bucket-type activate map_bucket
```

To check whether activation has been successful, simply use the same `bucket-type status` command detailed above.

## Usage Examples

The examples below show you how to use Riak Data Types at the application level. Code samples are currently available in Ruby (using Basho's oficial [Riak Ruby client](https://github.com/basho/riak-ruby-client/tree/bk-crdt-doc)).

All examples will use the bucket type names from above (`counter_bucket`, `set_bucket`, and `map_bucket`).

### Registers and Flags

Registers and flags cannot be used on their own in Riak. You cannot use a bucket/key pair as a register or flag directly. Instead, they must be used within a map. For usage examples, see the section on [[using maps|Using Data Types#Maps]] below.

### Counters

Counters are a bucket-level Riak Data Type that can be used either by themselves, i.e. associated with a bucket/key pair, or within a map. The examples in this section will show you how to use counters on their own.

First, we need to create and name a Riak bucket to house our counter (or as many counters as we'd like). We'll keep it simple and name our bucket `counters`:

```ruby
bucket = client.bucket 'counters'
```

```erlang
%% Buckets are simply named binaries in the Erlang client.
%% See below for more information.
```

To create a counter, you need to specify a bucket/key pair to hold that counter. Here is the general syntax for doing so:

```ruby
counter = Riak::Crdt::Counter.new bucket, key, bucket_type
```

```erlang
%% Counters are not encapsulated with the bucket/key in the Erlang
%% client. See below for more information.
```

Let's say that we want to create a counter called `traffic_tickets` in our `counters` bucket to keep tabs on our legal misbehavior. We can create this counter and ensure that the `counters` bucket will use our `counter_bucket` bucket type like this:

```ruby
counter = Riak::Crdt::Counter.new counters, 'traffic_tickets', 'counter_bucket'

# Alternatively, the Ruby client enables you to set a bucket type as being
# globally associated with a Riak Data Type. The following would set all
# counter buckets to use the counter_bucket bucket type:

Riak::Crdt::DEFAULT_BUCKET_TYPES[:counter] = 'counter_bucket'
to create our
# This would enable us to create our counter without specifying a bucket type:

counter = Riak::Crdt::Counter.new counters, 'traffic_tickets'
```

```erlang
Counter = riakc_counter:new().

%% Counters in the Erlang client are opaque data structures that
%% collect operations as you mutate them. We will associate the data
%% structure with a bucket type, bucket, and key later on.
```

Now that our client knows which bucket/key pairing to use for our counter, `traffic_tickets` will start out at 0 by default. If we happen to get a ticket that afternoon, we would need to increment the counter:

```ruby
counter.increment
```

```erlang
Counter1 = riakc_counter:increment(Counter).
```

The default value of an increment operation is 1, but you can increment by more than one if you'd like (but always by an integer). Let's say that we decide to spend an afternoon flaunting traffic laws and manage to rack up five tickets:

```ruby
counter.increment 5
```

```erlang
Counter2 = riakc_counter:increment(5, Counter1).
```

If we're curious about how many tickets we have accumulated, we can simply retrieve the value of the counter at any time:

```ruby
counter.value
# Output will always be an integer
```

```erlang
riakc_counter:dirty_value(Counter2).

%% The value fetched from Riak is always immutable, whereas the "dirty
%% value" takes into account local modifications that have not been
%% sent to the server. For example, whereas the call above would return
%% '6', the call below will return '0' since we started with an empty
%% counter:

riakc_counter:value(Counter2).

%% To fetch the value stored on the server, use the call below:

{ok, CounterX} = riakc_pb_socket:fetch_type(Pid,
                                    {<<"counter_bucket">>,<<"counters">>},
                                    <<"traffic_tickets">>).
```

Any good counter needs to decrement in addition to increment, and Riak counters allow you to do precisely that. Let's say that we hire an expert who manages to get a traffic ticket stricken from the record:

```ruby
counter.decrement

# Just like incrementing you can also decrement by more than one, e.g.:

counter.decrement 3
```

```erlang
Counter3 = riakc_counter:decrement(Counter2).

%% As with incrementing, you can also decrement by more than one:

Counter4 = riakc_counter:decrement(3, Counter3).

%% At some point, we'll want to send our local updates to the server
%% so they get recorded and are visible to others. Extract the update
%% using the to_op/1 function, then pass it to
%% riakc_pb_socket:update_type/4,5.

riakc_pb_socket:update_type(Pid, {<<"counter_bucket">>,<<"counters">>},
                            <<"traffic_tickets">>,
                            riakc_counter:to_op(Counter4)).
```

Operations on counters can be performed singly, as above, but let's say that we now have two counters in play, `dave_traffic_tickets` and `susan_traffic_tickets`. Both of these ne'er-do-wells get a traffic ticket on the same day and we need to increment both counters:

```ruby
counters = [dave_traffic_tickets, susan_traffic_tickets]

counters.each do |c|
  c.increment
end
```

```erlang
Counters = [{<<"dave">>, DaveTrafficTickets},
            {<<"susan">>, SusanTrafficTickets}].

[ begin
     Update = riakc_counter:to_op(riakc_counter:increment(C)),
     riakc_pb_socket:update_type(Pid, {<<"counter_bucket">>, <<"counters">>},
                                 Key, Update)
  end || {Key, C} <- Counters ].
```

### Sets

As with counters (and maps, as shown below), using sets involves setting up a bucket/key pair to house a set and running set-specific operations on that pair.

Here is the general syntax for setting up a bucket/key combination to handle a set:

```ruby
# Note: both the Riak Ruby Client and Ruby the language have a class called Set. Make sure that you refer to the Ruby version as ::Set and the Riak client version as Riak::Crdt::Set

set = Riak::Crdt::Set.new bucket, key, bucket_type
```

```erlang
%% Like counters, sets are not encapsulated in a
%% bucket/key in the Erlang client. See below for more
%% information.
```

Let's say that we want to use a set to store a list of cities that we want to visit. Let's create a Riak set, simply called `set`, stored in the key `cities` in the bucket `travel` (using the `set_bucket` bucket type we created in the previous section):

```ruby
travel = client.bucket 'travel'
set = Riak::Crdt::Set.new travel, 'cities', 'set_bucket'

# Alternatively, the Ruby client enables you to set a bucket type as being
# globally associated with a Riak Data Type. The following would set all
# set buckets to use the set_bucket bucket type:

Riak::Crdt::DEFAULT_BUCKET_TYPES[:set] = 'set_bucket'

# This would enable us to create our set without specifying a bucket type:

set = Riak::Crdt::Set.new travel, 'cities'
```

```erlang
Set = riakc_set:new().

%% Sets in the Erlang client are opaque data structures that
%% collect operations as you mutate them. We will associate the data
%% structure with a bucket type, bucket, and key later on.
```

Upon creation, our set is empty. We can verify that it is empty at any time:

```ruby
set.empty?

# true
```

```erlang
riakc_set:size(Set) == 0.

%% Query functions like size/1, is_element/2, and fold/3 operate over
%% the immutable value fetched from the server. In the case of a new
%% set that was not fetched, this is an empty collection, so the size
%% is 0.
```

But let's say that we read a travel brochure saying that Toronto and Montreal are nice places to go. Let's add them to our `cities` set:

```ruby
set.add 'Toronto'
set.add 'Montreal'
```

```erlang
Set1 = riakc_set:add_element(<<"Toronto">>, Set),
Set2 = riakc_set:add_element(<<"Montreal">>, Set1).
```

Later on, we hear that Hamilton and Ottawa are nice cities to visit in Canada, but if we visit them, we won't have time to visit Montreal. Let's remove Montreal and add the others:

```ruby
set.remove 'Montreal'
set.add 'Hamilton'
set.add 'Ottawa'
```

```erlang
Set3 = riakc_set:del_element(<<"Montreal">>, Set2),
Set4 = riakc_set:add_element(<<"Hamilton">>, Set3),
Set5 = riakc_set:add_element(<<"Ottawa">>, Set4).
```

Now, we can check on which cities are currently in our set:

```ruby
set.members

#<Set: {"Hamilton", "Ottawa", "Toronto"}>
```

```erlang
riakc_set:dirty_value(Set5).

%% The value fetched from Riak is always immutable, whereas the "dirty
%% value" takes into account local modifications that have not been
%% sent to the server. For example, where the call above would return
%% [<<"Hamilton">>, <<"Ottawa">>, <<"Toronto">>], the call below would
%% return []. These are essentially ordsets:

riakc_set:value(Set5).

%% To fetch the value stored on the server, use the call below:

{ok, SetX} = riakc_pb_socket:fetch_type(Pid,
                                 {<<"set_bucket">>,<<"travel">>},
                                  <<"cities">>).
```

Or we can see whether our set includes a specific member:

```ruby
set.include? 'Vancouver'
# false

set.include? 'Ottawa'
# true
```

```erlang
%% At this point, Set5 is the most "recent" set from the standpoint
%% of our application.

riakc_set:is_element(<<"Vancouver">>, Set5).
riakc_set:is_element(<<"Ottawa">>, Set5).
```

We can also determine the size of the set:

```ruby
set.members.length
```

```erlang
riakc_set:size(Set5).
```

### Maps

The map is in many ways the richest of the Riak Data Types because all of the other Data Types can be embedded within them, _including maps themselves_, to create arbitrarily complex custom Data Types out of a few basic building blocks.

The semantics of dealing with counters, sets, and maps within maps are usually very similar to working with those types at the bucket level, and so usage is usually very intuitive.

The general syntax for creating a Riak map is directly analogous to the syntax for creating other data types:

```ruby
map = Riak::Crdt::Map.new bucket, key
```

```erlang
%% Maps in the Erlang client are opaque data structures that
%% collect operations as you mutate them. We will associate the data
%% structure with a bucket type, bucket, and key later on.
```

Let's say that we want to use Riak to store information about our company's customers. We'll use the bucket `customers` to do so. Each customer's data will be contained in its own key in the `customers` bucket. Let's create a map for the user Ahmed (`ahmed_info`) in our bucket and simply call it `map` for simplicity's sake (we'll use the `map_bucket` bucket type from above):

```ruby
customers = client.bucket 'customers'
map = Riak::Crdt::Map.new customers, 'ahmed_info', 'map_bucket'

# Alternatively, the Ruby client enables you to set a bucket type as being
# globally associated with a Riak Data Type. The following would set all
# map buckets to use the map_bucket bucket type:

Riak::Crdt::DEFAULT_BUCKET_TYPES[:map] = 'map_bucket'

# This would enable us to create our map without specifying a bucket type:

map = Riak::Crdt::Map.new customers, 'ahmed_info'
```

```erlang
Map = riakc_map:new().

%% Maps in the Erlang client are opaque data structures that
%% collect operations as you mutate them. We will associate the data
%% structure with a bucket type, bucket, and key later on.
```

#### Registers Within Maps

The first piece of info we want to store in our map is Ahmed's name and phone number, both of which are best stored as registers:

```ruby
map.registers['first_name'] = 'Ahmed'
map.registers['phone_number'] = '5551234567'

# Integers need to be stored as strings and then converted back when the data is retrieved. The following would work as well:

map.registers['phone_number'] = 5551234567.to_s
```

```erlang
riakc_map:
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

Now, we'll store whether Annika is subscribed to a variety of plans within the company as well:

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
