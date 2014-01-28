---
title: Using Datatypes
project: riak
version: 2.0.0+
document: tutorials
toc: true
audience: intermediate
keywords: [developers, datatypes]
---

In versions of Riak >= 2.0, Riak users can make use of a variety of convergent replicated data types ([[CRDTs]]). While Riak was originally built as a mostly data-agnostic key/value store, datatypes enable you to use Riak as a _data-aware_ system and thus to perform a variety of transactions on a range of datatypes.

In total, Riak support five CRDT-inspired datatypes: [[counters|CRDTs#Counters]], [[flags|CRDTs#Flags]], [[registers|CRDTs#Registers]], [[sets|CRDTs#Sets]], and [[maps|CRDTs#Maps]]. Of those five types, counters, sets, and maps can be used as bucket-level datatypes, whereas flags and registers must be embedded in maps (more on that [[below|Using Datatypes#Maps]]).

<div class="note">
<div class="title">Note</div>
Counters were the one Riak datatype made available prior to version 2.0 (in version 1.4). Usage documentation can be found [[here|HTTP Counters]]. The implentation of counters in version 2.0 has been almost completely revamped, and so if you are using Riak 2.0+, we strongly recommend that you follow the usage documentation here rather than the documentation for the older version of counters.
</div>

## Setting Up Buckets to Use Riak Datatypes

In order to use Riak datatypes, you must create a [[bucket type|Using Bucket Types]] that sets the `datatype` bucket parameter to either `counter`, `map`, or `set`.

The following would create three bucket types for three datatypes:

```bash
riak-admin bucket-type create map_bucket '{"props":{"datatype":"map"}}'
riak-admin bucket-type create set_bucket '{"props":{"datatype":"set"}}'
riak-admin bucket-type create counter_bucket '{"props":{"datatype":"counter"}}'
```

**Note**: The names `map_bucket`, `set_bucket`, and `counter_bucket` are not reserved terms. You are always free to name bucket types whatever you like, with the exception of `default`.

Once you've created a datatype, you can check to make sure that the bucket property configuration associated with that type is correct. This can be done either via HTTP or through the `riak-admin` interface.

Using `curl`, we can verify that the bucket `props` associated with the `map_bucket` type include `datatype` being set to `map`:

```curl
curl http://localhost:8098/types/map_bucket/props
```

If the `map_bucket` type exists and has been set up properly, we should see `"datatype":"map"` in the JSON response.

We can also see which properties are associated with a bucket type using the [[`riak-admin`|Riak Admin]] interface, specifically the `status` command:

```bash
riak-admin bucket-type status map_bucket
```

Instead of JSON, this will return a simple list of properties and associated values, i.e. a list of `property: value` pairs. If our `map_bucket` bucket type has been set properly, we should see the following pair in our console output:

```bash
datatype: map
```

## Usage Examples

The examples below show you how to use Riak datatypes at the application level. Code samples are currently available in Ruby (using Basho's oficial [Riak Ruby client](https://github.com/basho/riak-ruby-client/tree/bk-crdt-doc)).

### Registers and Flags

Registers and flags cannot be used on their own in Riak. That is, you cannot use a bucket/key pair as a register or flag directly. Instead, they must be used within a map. For usage examples, see the section on maps [[below|Using Datatypes#Maps]].

### Counters

Alongside sets and maps, counters are a datatype that can be used at the bucket level. The examples below will show you how to use counters both at the bucket level and within maps.

First, let's create and name a Riak bucket that houses any and all counters that we'd like to use. A bucket set up to use counters can store as many counters as want. We'll keep it simple and name our bucket `counters`:

```ruby
bucket = client.bucket 'counters'
```

To create a counter, you need to specific a bucket/key pair to hold that counter. Here is the general syntax for that:

```ruby
counter = Riak::Crdt::Counter.new bucket, key
```

Let's say that we want to create a counter called `traffic_tickets` in our `counters` bucket to keep tabs on our legal misbehavior:

```ruby
counter = Riak::Crdt::Counter.new counters, traffic_tickets
```

Our `traffic_tickets` counter will start out at 0 by default. If we happen to get a ticket that afternoon, we would need to increment the counter:

```ruby
counter.increment
```

The default value of the `increment` function is 1, but you can increment by more than one if you'd like. Let's say that we decide to spend an afternoon flaunting traffic laws and rack up five tickets:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d 5 \
  <host>:<port>/types/counter_bucket/buckets/counters/datatypes/traffic_tickets
```

```ruby
counter.increment 5
```

If we're curious about how many tickets we have accumulated, we can simply use the `value` method:

```curl
curl <host>:<port>/types/counter_bucket/buckets/counters/traffic_tickets

# JSON Response:

{"type":"counter", "value": <current_counter_value>}
```

```ruby
counter.value
# Output will always be an integer
```

The counterpart of `increment` is of course `decrement`. If we hire an expert who manages to get a traffic ticket stricken from the record:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d -1 \
  <host>:<port>/types/counter_bucket/buckets/counters/traffic_tickets
```

```ruby
counter.decrement

# You can also decrement by more than one, e.g.:
# counter.decrement 3
```

The `value` method will return the value of a counter at any given time, but you can also return the new value of a counter immediately after `increment` or `decrement` using `increment_and_return` and `decrement_and_return`. Let's say that our `counter`'s current value is 10:


```curl
curl <host>:<port>/types/counter_bucket/buckets/counters/traffic_tickets
# {"type":"counter", "value": 10}

# ugh...this one doesn't work for curl examples
```

```ruby
counter.value
# 10

counter.increment_and_return
# 11

counter.decrement_and_return
# 10

counter.increment_and_return 10
# 20

counter.decrement_and_return 5
# 15
```

Operations on counters can be performed singly, as in the examples above, or as batched operations. The following increments the counter by 5, then returns the value of the count by 3:

```ruby
counter.batch do |c|
  c.increment 5
  puts c.value
  c.increment 3
end
```

Or, you can perform operations on multiple counters. Let's say that there are now two counters in play, `dave_traffic_tickets` and `susan_traffic_tickets`. If both of them get a ticket on the same day:

```curl
for counter in dave_traffic_tickets susan_traffic_tickets
do
  curl -XPOST \
    -H "Content-Type: application/json" \
    -d 1 \
    <host>:<port>/types/counter_bucket/buckets/counters/datatypes/$counter
done
```

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

set = Riak::Crdt::Set.new bucket, key
```

Let's say that we want to use a set to store a list of cities that we want to visit. Let's create a Riak set called `cities` in the bucket `travel`:

```ruby
travel = client.bucket 'travel'
set = Riak::Crdt::Set.new travel, cities
```

Let's say that we read a travel brochure saying that Toronto and Montreal are nice places to be. Let's add them to our `cities` set:

```curl
for city in Toronto Montreal
do
  curl -XPOST \
    -H "Content-Type: application/json" \
    -d '{"add":"$city"}' \
    <host>:<port>/types/set_bucket/buckets/travel/datatypes/cities
done
```

```ruby
set.add 'Toronto'
set.add 'Montreal'
```

Then, we hear that Hamilton and Ottawa are nice cities to visit in Canada, but if we visit them, we won't have time to visit Montreal. Let's use a batch operation to add them to the set while also removing Montreal:

```ruby
set.batch do |s|
  s.add 'Hamilton'
  s.add 'Ottawa'
  s.remove 'Montreal'
end
```

Now, we can check on which cities are currently in our set:

```ruby
set.members
# <Set: {'Hamilton','Ottawa','Toronto'}>
```

Or we can see whether our set includes a specific member:

```ruby
set.include? 'Vancouver'
# false

set.include? 'Ottawa'
# true
```

Or we can add a city---or multiple cities---to multiple sets. Let's say that Dave and Jane have visited Cairo and Beijing, and that we wish to add them to `dave_cities_visited` and `jane_cities_visited`:

```ruby
[dave_cities_visited, jane_cities_visited].each do |set|
  ['Cairo', 'Beijing'].each do |city|
    set.add city
  end
end
```

### Maps

The map is in many ways the richest of the Riak datatype because all of the other datatypes can be embedded within them, including maps themselves, to create arbitrarily complex custom datatypes out of the basic building blocks provided by the other Riak datatypes.

Here is the general syntax for creating a Riak map:

```ruby
map = Riak::Crdt::Map.new bucket, key
```

Let's say that we want to use Riak to store information about our company's customers. We'll use the bucket `customers` to do so. Each customer's data will be contained in its own key in the `customers` bucket. Let's create a map for the user Ahmed (`ahmed`) in our bucket and simply call it `map` for simplicity's sake:

```ruby
map = Riak::Crdt::Map.new customers, ahmed
```

The first piece of info we want to store in our map is Ahmed's name and phone number, both of which are best stored as registers:

```ruby
map.registers['first_name'] = 'Ahmed'
map.registers['phone_number'] = '5551234567' # integers need to be stored as strings and then converted back when the data is retrieved
```

This will work even though registers `first_name` and `phone_number` did not previously exist, as Riak will create those registers for you.

We also want to know how many times Ahmed has visited our website. We'll use a `page_visits` counter for that and run the following operation when Ahmed visits our page for the first time:

```ruby
map.counters['page_visits'].increment
```

Even though the `page_visits` counter did not exist previously, the above operation will create it (with a default starting point of 0) and the `increment` method will bump the counter up to 1.

Now let's say that we add an Enterprise plan to our pricing model. We'll create an `enterprise_customer` flag to track whether Ahmed has signed up for the new plan. He hasn't yet, so we'll set it to `false`:

```ruby
map.flags['enterprise_customer'] = false
```

We'd also like to know what Ahmed's interests are so that we can better design a user experience for him. Through his purchasing decisions, we find out that Ahmed likes playing with robots, opera, and motorcyles. We'll store that information in a set:

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

The marketing department decides to ditch the Enterprise plan in our pricing model, and so the `enterprise_customer` flag no longer provides any useful information in our data model. Let's get rid of it:

```ruby
map.flags.delete 'enterprise_customer'
```

We learn from a recent purchasing decision that Ahmed actually doesn't seem to like opera. He's much more keen on indie pop. Let's change the `interests` set to reflect that:

```ruby
map.sets['interests'].remove 'opera'
map.sets['interests'].add 'indie pop'
```

Now, let's say that Ahmed fills out a questionnaire on our site and we learn a lot of new things about him. 
We also learn from our analytics software that he's visited the site 50 times in the last month
We can make all of those changes in one go using a batch operation:

```ruby
map.batch do |m|
  m.sets['interests'].add 'Sudoku'

  m.counters['page_visits'].increment 50
end
```


```ruby
map.registers['favorite movie'] = 'The Avengers 2'

map.flags['retweeted'] = false

map.maps['atlantis'].registers['location'] #=> 'Narnia'

map.counters.delete 'thermometers'

user_maps = [larry_map, moe_map, curly_map]

user_maps.each do |m|
  m.flags['dead'] = true
end

map.batch do |m|
  m.counters['retweets'].increment
  m.flags['popular'] = true
  m.sets['followers'].add 'lucperkins'
end

maps = [map1, map2]

maps.each do |map|
  map.batch do |m|
    m.counters['retweets'].increment
    m.flags['popular'] = true
    m.sets['followers'].add 'lucperkins'
  end
end
```

Now, if we need to

## Scratchpad

https://gist.github.com/russelldb/5da7d895cebc77dd38b8
https://github.com/basho/riak_kv/blob/develop/src/riak_kv_wm_crdt.erl#L26

Result:

```json
{
  "type": ..., // String designating what datatype is presented (counter, set, map)
  "value": ..., // Representation of the datatype's value
  "context": ... // Opaque context
}
```

Mutating datatype:

```curl
POST /types/<type>/buckets/<bucket>/datatypes/
POST /types/<type>/buckets/<bucket>/datatypes/key
```

Operations are submitted in JSON payloads. If the key is not specified, one will be generated and returned in the Location header

### Query parameters

For `r`, `pr`, `w`, `pw`, `dw`:

Value | Description |
:-----|:------------|
`default` | Whatever the bucket default is. This is the valued used for any absent value. |
`quorum` | (Bucket `n_val` / 2) + 1 |
`all` | All replicas must respond. |
`one` | Any one response is enough. |
integer value | The specific number of vnodes must respond. Must be =< N. |

Param | Description |
:-----|:------------|
`r` | Read quorum |
`pr` | Primary read quorum |
`basic_quorum` | Boolean. Return as soon as a quorum of responses are received if true. Default is the bucket default if absent. |
`notfound_ok` | Boolean. A `not_found` response from a vnode counts toward `r` quorum if true. Default is the bucket default if absent. |
`include_context` | Boolean. If the datatype requires the opaque `context` for safe removal, include it in the response. Default `true`.

## Error Codes


## Scratchpad

`include_context` on updates is invoked only if they have `return_body`

`include_context=false` => "I'm just going to read this and pass it on" or "I'm going to read, but I _know_ that I won't remove anything"
`include_context=true` => "I might remove something"

"add wins" strategy is the default; remove will only win when no concurrent adds have occurred

"observed remove" behavior

Weird thing: you can add something to a set or map even if you already know it's there; this is to make sure that "add wins" under certain circumstances (either adding a )

`return_body` | 
