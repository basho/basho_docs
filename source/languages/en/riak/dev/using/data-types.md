---
title: Using Datatypes
project: Riak
version: 2.0.0+
document: tutorials
toc: true
audience: intermediate
keywords: [developers, datatypes]
---

In versions of Riak >= 2.0, you can make use of a variety of convergent replicated data types (CRDTs). While Riak was originally built as a data-agnostic key/value store, datatypes make Riak aware of your data and enable you to make specific transactions 

In total, there are five Riak datatypes: [[counters|CRDTs#Counters]], [[flags|CRDTs#Flags]], [[registers|CRDTs#Registers]], [[sets|CRDTs#Sets]], and [[maps|CRDTs#Maps]]. Of those five types, counters, sets, and maps can be used as bucket-level datatypes, whereas flags and registers must be embedded in maps (more on that [[below|Using Datatypes#Maps]]).

<div class="note">
<div class="title">Note</div>
Counters were the one Riak datatype made available prior to version 2.0 (in version 1.4). Usage documentation can be found [[here|HTTP Counters]]. The implentation of counters in version 2.0 has been almost completely revamped, and so if you are using Riak 2.0+, we strongly recommend that you follow the usage documentation here rather than the documentation for the older version of counters. 
</div>

## Setting Up Buckets to Use Riak Datatypes

In order to use Riak datatypes, you must do so by 

Setting up a bucket to store specific data types:

```bash
riak-admin bucket-type create map_bucket '{"props":{"datatype":"map"}}'
riak-admin bucket-type create set_bucket '{"props":{"datatype":"set"}}'
riak-admin bucket-type create counter_bucket '{"props":{"datatype":"counter"}}'
```

And then check:

```curl
curl http://localhost:8098/types/map_bucket/props | python -mjson.tool # or pjson or another tool
```

## Counters

Alongside sets and maps, counters are a datatype that can be used at the bucket level. The examples below will show you how to use counters both at the bucket level and within maps.

#### Using a Counters Bucket

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

Our `traffic_tickets` counter will start out at 0. If we happen to get a ticket that afternoon, we would need to increment the counter:

```ruby
counter.increment
```

The default value of the `increment` function is 1, but you can increment by more than one if you'd like. Let's say that we decide to spend an afternoon flaunting traffic laws and rack up five tickets:

```ruby
counter.increment 5
```

If we're curious about how many tickets we have accumulated, we can simply use the `value` method:

```ruby
counter.value
# Output will always be an integer
```

The counterpart of `increment` is of course `decrement`. If we hire an expert who manages to get a traffic ticket stricken from the record:

```ruby
counter.decrement

# You can also decrement by more than one, e.g.:
# counter.decrement 3
```

The `value` method will return the value of a counter at any given time, but you can also return the new value of a counter immediately after `increment` or `decrement` using `increment_and_return` and `decrement_and_return`. Let's say that our `counter`'s current value is 10:

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

```ruby
counters = [dave_traffic_tickets, susan_traffic_tickets]

counters.each do |c|
  c.increment
end
```

## Sets

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

## Maps

Maps are in many ways the richest Riak datatype because within them you can embed all of the other datatypes, _including maps themselves_. 

```ruby
map = Riak::Crdt::Map.new bucket key

map.counters['tweets'].value
map.sets['followers'].include? 'Horse_ebooks'

map.sets['cacti'].value #=> #<Set: {'saguaro', 'prickly pear', 'fishhook'}>
map.sets['cacti'].remove 'prickly pear'
map.sets['cacti'].value #=> #<Set: {'saguaro', 'fishhook'}>

map.registers['favorite movie'] = 'The Avengers'

# Change of opinion
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

#### Register

Registers cannot be used on their own in Riak. They must be used within a map.

Form: `{"assign":Value}` where `value` is the new string value of the register

#### Flag

Embedded in map only

Form: the string `enable` or `disable`

## Usage Examples

The examples below show you how to use Riak datatypes at the application level. Code samples are currently available in Ruby (using Basho's oficial [Riak Ruby client](https://github.com/basho/riak-ruby-client/tree/bk-crdt-doc)).

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

## Operations

#### Counter

Integer

Form is either:

* An integer (increments by default)
* An object containing `increment` or `decrement` and an integer

#### Set

Array of strings

Form: Object containing any combination of `add`, `add_all`, `remove`, `remove_all` fields; `add` and `remove` should refer to single string values, while `add_all` and `remove_all` should be arrays of strings; the `context` field may be included

#### Map

Form: Object containing any combination of the fields `add`, `remove`, or `update`; `add` and `remove` should be lists of field names described above; `update` should be an object containing fields and the operation to apply to the type associated associated with the field

Naming convention: field name + `_` + type

* `firstname_register`
* `retweeted_flag`
* `userinfo_map`
* `followers_set`