---
title_supertext: "Getting Started:"
title: "Querying with NodeJS"
description: ""
project: "riak_kv"
project_version: 2.9.1
menu:
  riak_kv-2.9.1:
    name: "Querying"
    identifier: "getting_started_nodejs_query"
    weight: 101
    parent: "getting_started_nodejs"
toc: true
aliases:
  - /riak/2.9.1/dev/taste-of-riak/querying-nodejs
  - /riak/kv/2.9.1/dev/taste-of-riak/querying-nodejs
---

## Node.js Version Setup

For the Node.js version, please download the source from GitHub by either
[cloning](https://github.com/basho/taste-of-riak) the source code
repository or downloading the [current zip of the master
branch](https://github.com/basho/taste-of-riak/archive/master.zip).
The code for this chapter is in `nodejs/Ch02-Schemas-and-Indexes`. Be
sure to run `npm install` in this directory prior to running `node
./app.js` to run the code.

## A Quick Note on Querying and Schemas

_Schemas_? Yes, we said that correctly: S-C-H-E-M-A-S. It's not a dirty
word. Even in a key/value store, you will still have a logical database
schema of how all the data relates to other data. This can be as simple
as using the same key across multiple buckets for different types of
data to having fields in your data that are related by name. These
querying methods will introduce you to some ways of laying out your data
in Riak, along with how to query it back.

## Denormalization

If you're coming from a relational database, the easiest way to get your
application's feet wet with NoSQL is to denormalize your data into
related chunks. For example, with a customer database, you might have
separate tables for customers, addresses, preferences, etc. In Riak,
you can denormalize all that associated data into a single object and
store it into a `Customer` bucket. You can keep pulling in associated
data until you hit one of the big denormalization walls:

* Size Limits (objects greater than 1MB)
* Shared/Referential Data (data that the object doesn't "own")
* Differences in Access Patterns (objects that get read/written once vs.
  often)

At one of these points we will have to split the model.

## Same Keys, Different Buckets

The simplest way to split up data would be to use the same identity key
across different buckets. A good example of this would be a `Customer`
object, an `Order` object, and an `OrderSummaries` object that keeps
rolled up info about orders such as total, etc. Let's put some data into
Riak so we can play with it.

* [*Example:* Creating a customer](https://github.com/basho/taste-of-riak/blob/master/nodejs/Ch02-Schemas-and-Indexes/app.js#L24-L33)
* [*Example:* Creating orders and order summaries](https://github.com/basho/taste-of-riak/blob/master/nodejs/Ch02-Schemas-and-Indexes/app.js#L193-L262)

While individual Customer and Order objects don't change much (or
shouldn't change), the "Order Summary" object will likely change often.
It will do double duty by acting as an index for all a customer's
orders, and also holding some relevant data such as the order total,
etc. If we showed this information in our application often, it's only
one extra request to get all the info.

[*Example:* Fetching by shared key](https://github.com/basho/taste-of-riak/blob/master/nodejs/Ch02-Schemas-and-Indexes/app.js#L78-L96)

Which returns our amalgamated objects:

```bash
info: Customer     1: {"id":"1","name":"John Smith","address":"123 Main Street","city":"Columbus","state":"Ohio","zip":"43210","phone":"+1-614-555-5555","createdDate":"2013-10-01 14:30:26"}
info: OrderSummary 1: {"customerId":"1","summaries":[{"orderId":"1","total":415.98,"orderDate":"2013-10-01 14:42:26"},{"orderId":"2","total":359.99,"orderDate":"2013-10-15 16:43:16"},{"orderId":"3","total":74.98,"orderDate":"2013-11-03 17:45:28"}]}
```

While this pattern is very easy and extremely fast with respect to
queries and complexity, it's up to the application to know about these
intrinsic relationships.

## Secondary Indexes

{{% note %}}
Secondary indexes in Riak KV require a sorted backend: [Memory]({{<baseurl>}}riak/kv/2.9.1/setup/planning/backend/memory) or [LevelDB]({{<baseurl>}}riak/kv/2.9.1/setup/planning/backend/leveldb). [Bitcask]({{<baseurl>}}riak/kv/2.9.1/setup/planning/backend/bitcask) does not support secondary indexes.

See [Using Secondary Indexes (2i)]({{<baseurl>}}riak/kv/2.9.1/developing/usage/secondary-indexes) for more information on developing with secondary indexes.
{{% /note %}}

If you're coming from an SQL world, Secondary Indexes (2i) are a lot
like SQL indexes. They are a way to quickly look up objects based on a
secondary key, without scanning through the whole dataset. This makes it
very easy to find groups of related data by values, or even ranges of
values. To properly show this off, we will now add some more data to our
application, and add some secondary index entries at the same time.

[*Example:* Adding index data](https://github.com/basho/taste-of-riak/blob/master/nodejs/Ch02-Schemas-and-Indexes/app.js#L98-L141)

As you may have noticed, ordinary key/value data is opaque to 2i, so we
have to add entries to the indexes at the application level. Now let's
find all of Jane Appleseed's processed orders, we'll look up the orders
by searching the `SalespersonId` integer index for Jane's id of `9000`.

[*Example:* Query for orders where the SalespersonId index is set to 9000](https://github.com/basho/taste-of-riak/blob/master/nodejs/Ch02-Schemas-and-Indexes/app.js#L143-L159)

Which returns:

```text
Jane's Orders: 1, 3
```

Jane processed orders 1 and 3. We used an "integer" index to reference
Jane's ID, next let's use a "binary" index. Now, let's say that the VP
of Sales wants to know how many orders came in during October 2013. In
this case, we can exploit 2i's range queries. Let's search the
`OrderDate` binary index for entries between `2013-10-01` and
`2013-10-31`.

[*Example:* Query for orders where the OrderDate index is between 2013-10-01 and
2013-10-31](https://github.com/basho/taste-of-riak/blob/master/nodejs/Ch02-Schemas-and-Indexes/app.js#L161-175)

Which returns:

```text
October's Orders: 1, 2
```

Boom! Easy-peasy. We used 2i's range feature to search for a range of
values, and demonstrated binary indexes.

So to recap:

* You can use Secondary Indexes to quickly look up an object based on a
  secondary id other than the object's key.
* Indexes can have either Integer or Binary(String) keys
* You can search for specific values, or a range of values
* Riak will return a list of keys that match the index query
