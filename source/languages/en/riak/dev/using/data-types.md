---
title: Using Datatypes
project: Riak
version: 2.0.0+
document: tutorials
toc: true
audience: intermediate
keywords: [developers, data-types]
---

https://gist.github.com/russelldb/5da7d895cebc77dd38b8

There are three options:

`map`
`set`
`counter`

You can _not_ create a bucket that stores flags or registers.

## Using Bucket Types with CRDTs

Setting up a bucket to store a specific data type, e.g. maps:

```bash
riak-admin bucket-type create map_bucket '{"props":{"datatype":"map"}}'
riak-admin bucket-type create set_bucket '{"props":{"datatype":"set"}}'
riak-admin bucket-type create counter_bucket '{"props":{"datatype":"counter"}}'
```

Via HTTP:

```curl
curl -XPUT \
-H "Content-Type: application/json" \
-d '{"props":{"datatype":"map"}}' \
http://localhost:8098/buckets/map_bucket/props
```

And then check:

```curl
curl http://localhost:8098/buckets/map_bucket/props | python -mjson.tool
```

JSON response if th

## Flags

## Registers

## Sets

```ruby
# Both the Riak Ruby Client and Ruby the language have a class called Set. Make sure that you refer to the Ruby version as ::Set and the Riak client version as Riak::Crdt::Set

```

## Counters

```ruby
counter = Riak::Crdt::Counter.new my_bucket, my_key
counter.value
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
```
