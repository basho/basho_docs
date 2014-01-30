---
title: Advanced Datatypes Usage
project: riak
version: 2.0.0+
document: tutorials
toc: true
audience: advanced
keywords: [developers, datatypes]
---

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

## Scratchpad

`include_context` on updates is invoked only if they have `return_body`

`include_context=false` => "I'm just going to read this and pass it on" or "I'm going to read, but I _know_ that I won't remove anything"
`include_context=true` => "I might remove something"

"add wins" strategy is the default; remove will only win when no concurrent adds have occurred

"observed remove" behavior

Weird thing: you can add something to a set or map even if you already know it's there; this is to make sure that "add wins" under certain circumstances (either adding a )

https://gist.github.com/russelldb/5da7d895cebc77dd38b8
https://github.com/basho/riak_kv/blob/develop/src/riak_kv_wm_crdt.erl#L26