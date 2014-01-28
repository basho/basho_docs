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

## Operations on Datatypes

When using Riak without using datatypes, the operations available to you are essentially of the get/put/delete variety, i.e. creating, updating, or deleting values associated with a key. The important thing to note about this approach is that operations are always performed on the _whole value_.

With datatypes, this is not the case, because they enable you to perform operations on _parts of a value_. If you're using a set, for example, you can add an element to a set by sending an `add` command to Riak along with the value that you wish to add, instead of having to fetch the current set, add the element on the application side, and then update the value in Riak. The same goes for maps. You can work with maps by performing specific operations on a map, such as adding and removing fields, using the datatype semantics that Riak provides.

The one slight exception to this is counters. If you're using a counter, you are indeed interacting with the "whole value" associated with a key, but with the important difference that Riak is aware of that value. When you increment that value, you are not getting the value, changing it on the application side, and then updating the value in Riak. Instead, you simply send a message to Riak to increment the counter, and Riak knows what to do from there.

<div class="note">
<div class="title">Note</div>
All of the examples in this section will be in the form of <tt>curl</tt> requests, because these requests best illustrate how transactions are made to datatypes under the hood. Transactions using Basho's official Riak clients will not be shown in this section because these clients provide APIs that abstract away the underlying transactions. Code examples for those clients can be found in the [[Usage Examples|Using Datatypes#Usage-Examples]] section below.
</div>

## The General Form of Operations

All datatype operations performed via HTTP take place using the methods `GET`, `POST`, and `DELETE` methods and URLs of the following form:

```curl
<host>:<port>/types/<bucket_type>/buckets/<bucket>/datatypes/<key>
```

A few things to note:

* The `<bucket_type>` used must be set up to handle the necessary datatype, as explained [[above|Using Datatypes#Setting-Up-Buckets-to-Use-Riak-Datatypes]]
* All data passed to Riak datatypes, be it using `-d` or `--data` or some other means, needs to be passed as JSON using a `Content-Type: application/json` header
* The URL structure above is different from that of non-datatype requests

Below is a listing of the specific operations available for each of Riak's datatypes.

### Counters

The value of a counter is _always_ a positive or negative integer. The available counter-specific operations are `increment` and `decrement`. Here is an example of incrementing the counter `retweets` in the bucket `my_counters` by 1 (using the bucket type `counter_bucket` from above):

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d 1 \
  http://localhost:8098/types/counter_bucket/buckets/my_counters/datatypes/retweets
```

If you increment or decrement a counter that does not exist prior to running the operation, the operation will affect a counter that will be automatically created for that bucket/key pair. All counters begin at 0. 

Here's an example of decrementing the same counter by 5:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d -5
  http://localhost:8098/types/counter_bucket/buckets/my_counters/datatypes/retweets
```

You can retrieve the value of a counter using a `GET` request on that key:

```curl
curl http://localhost:8098/types/counter_bucket/buckets/my_counters/datatypes/retweets
```

If the value of the counter is, say, 10, the response will be JSON of the following form:

```json
{
  "type": "counter",
  "value": 10
}
```

Removing a counter entirely involves a `DELETE` request, but that request must conform to the URL structure for non-datatype-specific requests:

```curl
curl -XDELETE \
  http://localhost:8098/types/counter_bucket/buckets/my_counters/keys/retweets
```

`DELETE` requests to URLs containing `/datatypes` will not work.

### Sets

Riak sets are essentially lists of binaries. There are four available operations for sets: `add`,

Operation | JSON syntax
:---------|:-----------
`add` | `"add": "<value>"`
`add_all` | `"add_all": ["<value_1>", "<value_2>", ... ]`
`remove` | `"remove": "<value>"`
`remove_all` | `"remove_all": ["<value_1>", "<value_2>"]`

In the following examples, we'll be working with a set called `vegetables`, which is located in the bucket `my_sets`, which bears the bucket type `set_bucket` (from above).

Let's add the vegetable `onions` to our set:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"add":"onions"}' \
  http://localhost:8098/types/set_bucket/buckets/my_sets/datatypes/vegetables
```

Now, let's retrieve that set:

```curl
curl http://localhost:8098/types/set_bucket/buckets/my_sets/datatypes/vegetables
```

The response will contain the set's type (`set`), the current value of the set as an array (in this case `["onions"]`), as well as the current context of the set (more on contexts [[here|Riak Datatypes#Contexts]]:

```json
{
  "type": "set",
  "value": ["onions"],
  "context": "SwEIGINsAAAAAWgCbQAAAAjFUrELUuB0xGEBagAAAAYBb25pb24Ng2wAAAABaAJhAWEBag=="
}
```

If you'd like to retrieve the `value` of the set without the `context`, you can pass the `include_context=false` parameter as part of the query:

```curl
curl http://localhost:8098/types/set_bucket/buckets/my_sets/datatypes/vegetables?include_context=false
```

This will return the following JSON:

```json
{
  "type": "set",
  "value": ["onions"]
}
```

Now, let's add two more vegetables, `kale` and `parsnips` to our set:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"add_all":["kale","parsnips"]}' \
  http://localhost:8098/types/set_bucket/buckets/my_sets/datatypes/vegetables
```

Now, a `GET` request on the `vegetables` set should return the following:

```json
{
  "type": "set",
  "value": ["kale", "onion", "parsnips"]
}
```

Removing a vegetable, say `kale`, from the set involves `POST`ing a `remove` command via JSON, _not_ running a `DELETE` request (as `DELETE` requests can be used only to delete a set):

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"remove":"kale"}' \
  http://localhost:8098/types/set_bucket/buckets/my_sets/datatypes/vegetables
```

If we want to remove multiple vegetables from our set, we can use the `remove_all` operation. Let's say that we want to remove the last two vegetables in the set, `onion` and `parsnips`:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"remove_all":["onion","parsnips"]}' \
  http://localhost:8098/types/set_bucket/buckets/my_sets/datatypes/vegetables
```

Removing the set entirely invoves a `DELETE` request analogous to the `DELETE` request in the section on counters above:

```curl
curl -XDELETE \
  http://localhost:8098/types/set_bucket/buckets/my_sets/keys/vegetables
```

### Maps

Using maps via HTTP is more somewhat more complex than using counters or sets because maps allow you to store any Riak datatype within them, _including maps themselves_ (the way that a data format like JSON allows you to store objects inside of objects).

Operations involving a map will always involve _fields_ inside of that map. When using Riak maps, those fields must be named according to the following convention:

> field name + `_` + type

Here are some examples:

* `firstname_register`
* `retweeted_flag`
* `userinfo_map`
* `followers_set`
* `pagevisits_counter`

There are three operations involving map fields:

Operation | Description | JSON Syntax
:---------|:------------|:-----------
`add` | Create a new field (flag, register, counter, set, or map) | `{"add":"<field>"}` or `{"add":[<fields>]}`
`remove` | Remove an existing field (flag, register, counter, set, or map) | `{"remove":"<field>"}` or `{"remove":[<fields>]}`
`update` | Perform a datatype-specific operation on a field, e.g. `enable` on a flag or `add_all` on a set | `{"update":{<field>:<operation>}}` or `{"update"{<field>:<operation>, <field2>:<operation2>, ...}}`

All `GET` requests on a map return JSON of the following form (if `include_context=false` is passed as a parameter):

```json
{
  "type": "counter | set | map",
  "value": "<value>"
}
```

Or of the following form if context is included (this is the default behavior):

```json
{
  "type": "counter | set | map",
  "value": "<value>",
  "content": "<context>"
}
```

The following sections discuss using all of the available Riak datatypes as they are used inside of maps. The overarching example will involve a map storing CRM-style information for a customer named Ahmed. The map will be named `ahmed_info` and it will be stored in the bucket `crm`, which bears the type `map_bucket` (as shown above).

#### Flags

[[Flags|CRDTs#Flags]] essentially act like Boolean values. Flags have two possible states: `enable` and `disable`. An `operation` on a flag takes the following form:

```json
{"update": {"<name>_flag":"enable | disable"}}
```

Let's create the flag `enterprise_plan_flag` to note whether Ahmed is currently an Enterprise Plan subscriber. He is not currently, so we'll set the flag to `disable` when creating the flag:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"update":{"enterprise_plan_flag":"disable"}}' \
  http://localhost:10018/types/map_bucket/buckets/crm/datatypes/ahmed_info
```

If you create a flag without specifying `enable` or `disable`, Riak will default to assigning `disable` upon creation. Hence, the following will also create the `enterprise_plan_flag` and set it to `disable`:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"add":"enterprise_plan_flag"}' \
  http://localhost:10018/types/map_bucket/buckets/crm/datatypes/ahmed_info
```

Now, let's fetch the map in its current state (without context):

```curl
curl http://localhost:10018/types/map_bucket/buckets/crm/datatypes/ahmed_info?include_context=false
```

The response:

```json
{
  "type": "map",
  "value": {
    "enterprise_plan_flag": false
  }
}
```

**Note**: While the value of a flag will always be `true` or `false` when it is read, you _cannot_ set it as `true` or `false`. You must set it as `enable` or `disable`.

Finally, you can remove a flag using the `remove` command:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"remove":"enterprise_plan_flag"}' \
  http://localhost:10018/types/map_bucket/buckets/crm/datatypes/ahmed_info
```

**Note**: Fields can be removed _only_ using the `remove` command. You cannot remove fields using `DELETE` requests, as those requests can refer only to a map as a whole and not to any of its consituent parts.

### Registers

Registers hold a single binary, for example a string. If you need to store multiple binaries in a field, then you should consider using a set instead.

Let's say that we want to add a `middle_name_register` to the `ahmed_info` map to hold Ahmed's middle name. If we don't know Ahmed's middle name at first, then using the `add` operation will simply add an empty register:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"add":"middle_name_register"}' \
  http://localhost:10018/types/map_bucket/buckets/crm/datatypes/ahmed_info
```

Let's say that we find out that Ahmed's middle name is Darius. We can now update the register to reflect that

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"update":{"middle_name_register":"Darius"}}' \
  http://localhost:10018/types/map_bucket/buckets/crm/datatypes/ahmed_info
```

**Note**: If you attempt to update a register that does not yet exist, the register will be created for you and then updated to the value passed to it.

Or if we decide that we don't really need to know customers' middle names, we can simply remove the register from the map:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"remove":"middle_name_register"}' \
  http://localhost:10018/types/map_bucket/buckets/crm/datatypes/ahmed_info
```

### Counters

While usage of counters was covered in the section [[above|Using Datatypes#Counters]], it is important to note that counter-related operations are syntatically different when they are used within maps rather than at the bucket/key level. Counters within maps are incremented and decremented by pairing an integer value with the counter as part of a map `update` operation (positive integers to increment, negative integers to decrement).

Let's say that we want to track the number of times that Ahmed leaves a comment on our company blog. We'll do so using a `blog_comments_counter` field in our map. As expected, counters begin at 0 and can be created automatically as part of an `update` operation. The following would create the `blog_comments_counter` and increment it from 0 (the default initial value) to 5:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"update":{"blog_comments_counter":5}}' \
  http://localhost:10018/types/map_bucket/buckets/crm/datatypes/ahmed_info
```

The following would decrement that counter by 3:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"update":{"blog_comments_counter":-3}}' \
  http://localhost:10018/types/map_bucket/buckets/crm/datatypes/ahmed_info
```

### Sets

Using sets within maps is much like using sets on their own at the bucket/key level, involving the same `add`, `add_all`, `remove`, and `remove_all` commands, with the difference that those commands are nested within a broader `update` command to the set.

Let's say that we want to track Ahmed's interests so that we can have a better sense of which of our products he might be interested in. We'll use an `interests_set` to do so. We can start by creating an empty set:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"add":"interests_set"}' \
  http://localhost:10018/types/map_bucket/buckets/crm/datatypes/ahmed_info
```

Now, when we fetch the map, we'll see an empty JSON array associated with that set (if `include_context` is set to `false`):

```json
{
  "type": "map",
  "value": {
    "interests_set": []
  }
}
```

Let's say that we find out that Ahmed is interested in cooking, opera, and motorcycles. We can all of those values to the `interests` set in one operation:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"update":{"interests_set":{"add_all":["opera","cooking","motorcycles"]}}}' \
  http://localhost:10018/types/map_bucket/buckets/crm/datatypes/ahmed_info
```

We can also remove multiple values from a set in a single operation. If we find out that Ahmed actually isn't all that interested in cooking or opera, we can remove those like so:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"update":{"interests_set":{"remove_all":["opera","cooking"]}}}' \
  http://localhost:10018/types/map_bucket/buckets/crm/datatypes/ahmed_info
```

From there, we can add a single entry to the set using the `add` operation:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"update":{"interests_set":{"add":"stamp collecting"}}}' \
  http://localhost:10018/types/map_bucket/buckets/crm/datatypes/ahmed_info
```

Or we can remove a single entry using the `remove` operation:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"update":{"interests_set":{"remove":"stamp collecting"}}}' \
  http://localhost:10018/types/map_bucket/buckets/crm/datatypes/ahmed_info
```

Sets as a whole can be removed from a map in the way that we would expect:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"update":{"remove":"interests_set"}}' \
  http://localhost:10018/types/map_bucket/buckets/crm/datatypes/ahmed_info
```

### Maps

Using maps within maps actually isn't fundamentally different from using other Riak datatypes within maps. They can be added, removed, and updated just like any other Riak datatype, although using maps within maps can become tricky when nesting maps within maps within maps within...

Let's say that we want to know not just about Ahmed, but about his colleagues as well. And so we want to embed a map with info about Ahmed's colleague Annika (we'll call the map `annika_info_map`) into the `ahmed_info` map. We can start by inserting an empty map:

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '{"add":"annika_info_map"}' \
  http://localhost:10018/types/map_bucket/buckets/crm/datatypes/ahmed_info
```

Here is the JSON for map (without context):

```json
{
  "type": "map",
  "value": {
    "annika_info_map": {}
  }
}
```

**Note**: An empty map is represented by an empty JSON object, as opposed to an array for an empty set or an empty string for a register (as above).

Let's add some registers to the map to store Annika's first name (`Annika`) and last name (`Weiss`):

```curl
curl -XPOST \
  -H "Content-Type: application/json" \
  -d '' \
  http://localhost:10018/types/map_bucket/buckets/crm/datatypes/ahmed_info
```

## Errors

`Invalid operation on a datatype '<type>': <operation>`

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
