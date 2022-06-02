---
title_supertext: "Getting Started:"
title: "Querying with C Sharp"
description: ""
project: "riak_kv"
project_version: 2.9.4
menu:
  riak_kv-2.9.4:
    name: "Querying"
    identifier: "getting_started_csharp_query"
    weight: 101
    parent: "getting_started_csharp"
toc: true
aliases:
  - /riak/2.9.4/dev/taste-of-riak/querying-csharp
  - /riak/kv/2.9.4/dev/taste-of-riak/querying-csharp
---

## C Sharp Version Setup

For the C# version, please download the source from GitHub by either
[cloning][taste_of_riak] the source code repository or downloading the
[current zip of the master branch][master_zip]. The code for this
chapter is in `/csharp`. Open up `TasteOfRiak.sln` in Visual Studio or
your IDE of choice.

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
rolled up info about orders such as total, etc. You can find the source
for these POCOs in `Customer.cs`, `Order.cs` and
`OrderSummaries.cs`.  Let's put some data into Riak so we can play
with it.

```csharp
Console.WriteLine("Creating Data");
Customer customer = CreateCustomer();
IEnumerable<Order> orders = CreateOrders(customer);
OrderSummary orderSummary = CreateOrderSummary(customer, orders);

Console.WriteLine("Starting Client");
using (IRiakEndPoint endpoint = RiakCluster.FromConfig("riakConfig"))
{
    IRiakClient client = endpoint.CreateClient();

    Console.WriteLine("Storing Data");

    client.Put(ToRiakObject(customer));

    foreach (Order order in orders)
    {
        // NB: this adds secondary index data as well
        client.Put(ToRiakObject(order));
    }

    client.Put(ToRiakObject(orderSummary));

    ...
    ...
    ...
}
```

While individual `Customer` and `Order` objects don't change much (or
shouldn't change), the `OrderSummaries` object will likely change often.
It will do double duty by acting as an index for all a customer's
orders, and also holding some relevant data such as the order total,
etc. If we showed this information in our application often, it's only
one extra request to get all the info.

```csharp
Console.WriteLine("Fetching related data by shared key");
string key = "1";

var result = client.Get(customersBucketName, key);
CheckResult(result);
Console.WriteLine("Customer     1: {0}\n", GetValueAsString(result));

result = client.Get(orderSummariesBucketName, key);
CheckResult(result);
Console.WriteLine("OrderSummary 1: {0}\n", GetValueAsString(result));
```

Which returns our amalgamated objects:

```bash
Fetching related data by shared key
Customer     1: {"CustomerId":1,"Name":"John Smith","Address":"123 Main Street","City":"Columbus","State":"Ohio","Zip":"43210","Phone":"+1-614-555-5555","CreatedDate":"2013-10-01 14:30:26"}
OrderSummary 1: {"CustomerId":1,"Summaries":[{"OrderId":1,"Total":415.98,"OrderDate":"2013-10-01 14:42:26"},{"OrderId":2,"Total":359.99,"OrderDate":"2013-10-15 16:43:16"},{"OrderId":3,"Total":74.98,"OrderDate":"2013-11-03 17:45:28"}]}
```

While this pattern is very easy and extremely fast with respect to
queries and complexity, it's up to the application to know about these
intrinsic relationships.

## Secondary Indexes

{{% note %}}
Secondary indexes in Riak KV require a sorted backend: [Memory]({{<baseurl>}}riak/kv/2.9.4/setup/planning/backend/memory) or [LevelDB]({{<baseurl>}}riak/kv/2.9.4/setup/planning/backend/leveldb). [Bitcask]({{<baseurl>}}riak/kv/2.9.4/setup/planning/backend/bitcask) does not support secondary indexes.

See [Using Secondary Indexes (2i)]({{<baseurl>}}riak/kv/2.9.4/developing/usage/secondary-indexes) for more information on developing with secondary indexes.
{{% /note %}}

If you're coming from an SQL world, Secondary Indexes (2i) are a lot
like SQL indexes. They are a way to quickly look up objects based on a
secondary key, without scanning through the whole dataset. This makes it
very easy to find groups of related data by values, or even ranges of
values. To properly show this off, we will make a note of where
secondary index data is added to our model objects.

```csharp
private static RiakObject ToRiakObject(Order order)
{
    var orderRiakObjectId = new RiakObjectId(ordersBucketName, order.Id.ToString());
    var riakObject = new RiakObject(orderRiakObjectId, order);

    IntIndex salesPersonIdIndex = riakObject.IntIndex(ordersSalesPersonIdIndexName);
    salesPersonIdIndex.Add(order.SalesPersonId.ToString());

    BinIndex orderDateIndex = riakObject.BinIndex(ordersOrderDateIndexName);
    orderDateIndex.Add(order.OrderDate.ToString("yyyy-MM-dd"));

    return riakObject;
}
```

As you may have noticed, ordinary key/value data is opaque to 2i, so we
have to add entries to the indexes at the application level. Now let's
find all of Jane Appleseed's processed orders, we'll look up the orders
by searching the `SalespersonId` integer index for Jane's id of `9000`.

```csharp
// Query for order keys where the SalesPersonId index is set to 9000
var riakIndexId = new RiakIndexId(ordersBucketName, ordersSalesPersonIdIndexName);
RiakResult<RiakIndexResult> indexRiakResult = client.GetSecondaryIndex(riakIndexId, 9000); // NB: *must* use 9000 as integer here.
CheckResult(indexRiakResult);
RiakIndexResult indexResult = indexRiakResult.Value;
Console.WriteLine("Jane's orders (key values): {0}", string.Join(", ", indexResult.IndexKeyTerms.Select(ikt => ikt.Key)));
```

Which returns:

```text
Jane's orders (key values): 1, 3
```

Jane processed orders 1 and 3. We used an "integer" index to reference
Jane's ID, next let's use a "binary" index. Now, let's say that the VP
of Sales wants to know how many orders came in during October 2013. In
this case, we can exploit 2i's range queries. Let's search the
`OrderDate` binary index for entries between `2013-10-01` and
`2013-10-31`.

```csharp
// Query for orders where the OrderDate index is between 2013-10-01 and 2013-10-31
riakIndexId = new RiakIndexId(ordersBucketName, ordersOrderDateIndexName);
indexRiakResult = client.GetSecondaryIndex(riakIndexId, "2013-10-01", "2013-10-31"); // NB: *must* use strings here.
CheckResult(indexRiakResult);
indexResult = indexRiakResult.Value;
Console.WriteLine("October orders (key values): {0}", string.Join(", ", indexResult.IndexKeyTerms.Select(ikt => ikt.Key)));
```

Which returns:

```text
October orders (key values): 1, 2
```

We used 2i's range feature to search for a range of values, and demonstrated binary indexes.

So to recap:

* You can use Secondary Indexes to quickly look up an object based on a
  secondary id other than the object's key.
* Indexes can have either Integer or Binary(String) keys
* You can search for specific values, or a range of values
* Riak will return a list of keys that match the index query


[taste_of_riak]: https://github.com/basho/taste-of-riak
[master_zip]: https://github.com/basho/taste-of-riak/archive/master.zip

