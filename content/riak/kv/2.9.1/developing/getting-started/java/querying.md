---
title_supertext: "Getting Started:"
title: "Querying with Java"
description: ""
project: "riak_kv"
project_version: 2.9.1
menu:
  riak_kv-2.9.1:
    name: "Querying"
    identifier: "getting_started_java_query"
    weight: 101
    parent: "getting_started_java"
toc: true
aliases:
  - /riak/2.9.1/dev/taste-of-riak/querying-java
  - /riak/kv/2.9.1/dev/taste-of-riak/querying-java
---

## Java Version Setup

For the Java version, please download the source from GitHub by either
[cloning](https://github.com/basho/taste-of-riak) the source code
repository or downloading the [current zip of the master
branch](https://github.com/basho/taste-of-riak/archive/master.zip).
The code for this chapter is in `/java/Ch02-Schemas-and-Indexes`. You
may import this code into your favorite editor, or just run it from the
command line using the commands in `BuildAndRun.sh` if you are running
on a *nix* OS.

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
for these POJO's in `Customer.java`, `Order.java` and
`OrderSummaries.java`.  Let's put some data into Riak so we can play
with it.

```java
// From SipOfRiak.java

private static Customer createCustomer() {
    Customer customer = new Customer();
    customer.CustomerId = 1;
    customer.Name = "John Smith";
    customer.Address = "123 Main Street";
    customer.City = "Columbus";
    customer.State = "Ohio";
    customer.Zip = "43210";
    customer.Phone = "+1-614-555-5555";
    customer.CreatedDate = "2013-10-01 14:30:26";
    return customer;
}

private static ArrayList<Order> createOrders() {
    ArrayList<Order> orders = new ArrayList<Order>();

    Order order1 = new Order();
    order1.OrderId = 1;
    order1.CustomerId = 1;
    order1.SalespersonId = 9000;
    order1.Items.add(
            new Item("TCV37GIT4NJ",
                    "USB 3.0 Coffee Warmer",
                    15.99));
    order1.Items.add(
            new Item("PEG10BBF2PP",
                    "eTablet Pro; 24GB; Grey",
                    399.99));
    order1.Total = 415.98;
    order1.OrderDate = "2013-10-01 14:42:26";
    orders.add(order1);

    Order order2 = new Order();
    order2.OrderId = 2;
    order2.CustomerId = 1;
    order2.SalespersonId = 9001;
    order2.Items.add(
            new Item("OAX19XWN0QP",
                    "GoSlo Digital Camera",
                    359.99));
    order2.Total = 359.99;
    order2.OrderDate = "2013-10-15 16:43:16";
    orders.add(order2);

    Order order3 = new Order();
    order3.OrderId = 3;
    order3.CustomerId = 1;
    order3.SalespersonId = 9000;
    order3.Items.add(
            new Item("WYK12EPU5EZ",
                    "Call of Battle = Goats - Gamesphere 4",
                    69.99));
    order3.Items.add(
            new Item("TJB84HAA8OA",
                    "Bricko Building Blocks",
                    4.99));
    order3.Total = 74.98;
    order3.OrderDate = "2013-11-03 17:45:28";
    orders.add(order3);
    return orders;
}

private static OrderSummary createOrderSummary(ArrayList<Order> orders) {
    OrderSummary orderSummary = new OrderSummary();
    orderSummary.CustomerId = 1;
    for(Order order: orders)
    {
        orderSummary.Summaries.add(new OrderSummaryItem(order));
    }
    return orderSummary;
}

public static void main(String[] args) throws RiakException {

    System.out.println("Creating Data");
    Customer customer = createCustomer();
    ArrayList<Order> orders = createOrders();
    OrderSummary orderSummary = createOrderSummary(orders);

    System.out.println("Starting Client");
    IRiakClient client = RiakFactory.pbcClient("127.0.0.1", 10017);


    System.out.println("Creating Buckets");
    Bucket customersBucket = client.fetchBucket("Customers").lazyLoadBucketProperties().execute();
    Bucket ordersBucket = client.fetchBucket("Orders").lazyLoadBucketProperties().execute();
    Bucket orderSummariesBucket = client.fetchBucket("OrderSummaries").lazyLoadBucketProperties().execute();

    System.out.println("Storing Data");
    customersBucket.store(String.valueOf(customer.CustomerId), customer).execute();
    for (Order order : orders) {
        ordersBucket.store(String.valueOf(order.OrderId), order).execute();
    }
    orderSummariesBucket.store(String.valueOf(orderSummary.CustomerId), orderSummary).execute();
```

While individual `Customer` and `Order` objects don't change much (or
shouldn't change), the `OrderSummaries` object will likely change often.
It will do double duty by acting as an index for all a customer's
orders, and also holding some relevant data such as the order total,
etc. If we showed this information in our application often, it's only
one extra request to get all the info.

```java
    System.out.println("Fetching related data by shared key");
    String key = "1";
    String fetchedCust = customersBucket.fetch(key).execute().getValueAsString();
    String fetchedOrdSum = orderSummariesBucket.fetch(key).execute().getValueAsString();
    System.out.format("Customer     1: %s\n", fetchedCust);
    System.out.format("OrderSummary 1: %s\n", fetchedOrdSum);
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
Secondary indexes in Riak KV require a sorted backend: [Memory]({{<baseurl>}}riak/kv/2.9.1/setup/planning/backend/memory) or [LevelDB]({{<baseurl>}}riak/kv/2.9.1/setup/planning/backend/leveldb). [Bitcask]({{<baseurl>}}riak/kv/2.9.1/setup/planning/backend/bitcask) does not support secondary indexes.

See [Using Secondary Indexes (2i)]({{<baseurl>}}riak/kv/2.9.1/developing/usage/secondary-indexes) for more information on developing with secondary indexes.
{{% /note %}}

If you're coming from an SQL world, Secondary Indexes (2i) are a lot
like SQL indexes. They are a way to quickly look up objects based on a
secondary key, without scanning through the whole dataset. This makes it
very easy to find groups of related data by values, or even ranges of
values. To properly show this off, we will now add some more data to our
application, and add some secondary index entries at the same time.

```java
    System.out.println("Adding Index Data");
    IRiakObject riakObj = ordersBucket.fetch("1").execute();
    riakObj.addIndex("SalespersonId", 9000);
    riakObj.addIndex("OrderDate", "2013-10-01");
    ordersBucket.store(riakObj).execute();

    IRiakObject riakObj2 = ordersBucket.fetch("2").execute();
    riakObj2.addIndex("SalespersonId", 9001);
    riakObj2.addIndex("OrderDate", "2013-10-15");
    ordersBucket.store(riakObj2).execute();

    IRiakObject riakObj3 = ordersBucket.fetch("3").execute();
    riakObj3.addIndex("SalespersonId", 9000);
    riakObj3.addIndex("OrderDate", "2013-11-03");
    ordersBucket.store(riakObj3).execute();
```

As you may have noticed, ordinary key/value data is opaque to 2i, so we
have to add entries to the indexes at the application level. Now let's
find all of Jane Appleseed's processed orders, we'll look up the orders
by searching the `SalespersonId` integer index for Jane's id of `9000`.

```java
    // Query for orders where the SalespersonId index is set to 9000
    List<String> janesOrders = ordersBucket.fetchIndex(IntIndex.named("SalespersonId"))
                                           .withValue(9000).execute();

    System.out.format("Jane's Orders: %s\n", StringUtil.Join(", ", janesOrders));
```

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

```java
    // Query for orders where the OrderDate index is between 2013-10-01 and 2013-10-31
    List<String> octoberOrders = ordersBucket.fetchIndex(BinIndex.named("OrderDate"))
                                             .from("2013-10-01").to("2013-10-31").execute();

    System.out.format("October's Orders: %s\n", StringUtil.Join(", ", octoberOrders));
```

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
