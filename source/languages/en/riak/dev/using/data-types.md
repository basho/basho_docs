---
title: Using Datatypes
project: Riak
version: 2.0.0+
document: tutorials
toc: true
audience: intermediate
keywords: [developers, datatypes]
---

<!-- https://gist.github.com/russelldb/5da7d895cebc77dd38b8
https://github.com/basho/riak_kv/blob/develop/src/riak_kv_wm_crdt.erl#L26 -->

Ruby example code from the [official Riak Ruby client](https://github.com/basho/riak-ruby-client/tree/bk-crdt-doc).

There are three options for bucket types:

`map`
`set`
`counter`

You can _not_ create a bucket that stores flags or registers. Flags and registers can only be found in buckets storing maps or sets.

## Setting Up Buckets to Use Riak Datatypes

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

#### Register

Registers cannot be used on their own in Riak. They must be used within a map.

Form: `{"assign":Value}` where `value` is the new string value of the register

#### Flag

Embedded in map only

Form: the string `enable` or `disable`

## Usage Examples

## Counters

<div class="note">
<div class="title">Note</div>
Although counters were made available in Riak in version 1.4, if you are using Riak 2.0, we recommend that you do not use counters the way they were used in earlier versions.
</div>

First, create and name a Riak bucket. We'll name ours `counters` for this example:

```ruby
bucket = client.bucket 'counters'
```

Now, we need to create a counter. The general syntax for doing so:

```ruby
counter = Riak::Crdt::Counter.new bucket, key
```

But let's say that we want to create a counter called `traffic_tickets` in our `counters` bucket to keep tabs on our legal misbehavior:

```ruby
counter = Riak::Crdt::Counter.new counters, traffic_tickets
```

At any time, we can check to see how many traffic tickets we've accumulated:

```ruby
counter.value
# Output will always be an integer
```

If we get additional traffic tickets, we can use the `increment` function 

counter.increment
counter.decrement

counter.batch do |c|
  c.increment 5
  puts c.value
  c.increment 3
end

counters = [counter1, counter2]

counters.each do |c|
  c.increment
end
```

## Sets

```ruby
# Both the Riak Ruby Client and Ruby the language have a class called Set. Make sure that you refer to the Ruby version as ::Set and the Riak client version as Riak::Crdt::Set

set = Riak::Crdt::Set.new bucket, key
set.members #=> <Set: {'Edinburgh', 'Leeds', 'London'}>
set.add 'Newcastle'
set.remove 'London'

set.include? 'Leeds' #=> true

set.batch do |s|
  s.add 'York'
  s.add 'Aberdeen'
  s.remove 'Newcastle'
end

cities = ['Manchester', 'Liverpool', 'Nottingham']

set.bach do |s|
  cities.each do |city|
    s.add city
  end
end
```

## Maps

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
