---
title_supertext: "Getting Started:"
title: "Querying with Erlang"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Querying"
    identifier: "getting_started_erlang_query"
    weight: 101
    parent: "getting_started_erlang"
toc: true
aliases:
  - /riak/2.1.4/dev/taste-of-riak/querying-erlang
  - /riak/kv/2.1.4/dev/taste-of-riak/querying-erlang
---

## A Quick Note on Querying and Schemas

_Schemas_? Yes, we said that correctly: S-C-H-E-M-A-S. It's not a dirty
word. Even in a key/value store, you will still have a logical database
schema of how all the data relates to other data. This can be as simple
as using the same key across multiple buckets for different types of
data to having fields in your data that are related by name. These
querying methods will introduce you to some ways of laying out your data
in Riak, along with how to query it back.

A more comprehensive discussion can be found in [Key/Value Modeling]({{<baseurl>}}riak/kv/2.1.4/developing/key-value-modeling).

## Denormalization

If you're coming from a relational database, the easiest way to get your
application's feet wet with NoSQL is to denormalize your data into
related chunks. For example, with a customer database, you might have
separate tables for customers, addresses, preferences, etc. In Riak, you
can denormalize all that associated data into a single object and store
it into a `Customer` bucket. You can keep pulling in associated
data until you hit one of the big denormalization walls:

* Size limits (objects greater than 1MB)
* Shared/referential Data (data that the object doesn't "own")
* Differences in access patterns (objects that get read/written once vs.
  often)

At one of these points we will have to split the model.

## Same Keys, Different Buckets

The simplest way to split up data would be to use the same identity key
across different buckets. A good example of this would be a `Customer`
object, an `Order` object, and an `OrderSummaries` object that keeps
rolled up info about orders such as total, etc.

Let's put some data into Riak so we can play with it. Fire up your
Erlang REPL with the client library in the path, and enter in the
following:

```erlang
rd(customer, {customer_id, name, address, city, state, zip, phone, created_date}).
rd(item, {item_id, title, price}).
rd(order, {order_id, customer_id, salesperson_id, items, total, order_date}).
rd(order_summary_entry, {order_id, total, order_date}).
rd(order_summary, {customer_id, summaries}).


Customer = #customer{ customer_id= 1,
                      name= "John Smith",
                      address= "123 Main Street",
                      city= "Columbus",
                      state= "Ohio",
                      zip= "43210",
                      phone= "+1-614-555-5555",
                      created_date= {{2013,10,1},{14,30,26}}}.

Orders =  [ #order{
              order_id= 1,
              customer_id= 1,
              salesperson_id= 9000,
              items= [
                #item{
                  item_id= "TCV37GIT4NJ",
                  title= "USB 3.0 Coffee Warmer",
                  price= 15.99 },
                #item{
                  item_id= "PEG10BBF2PP",
                  title= "eTablet Pro, 24GB, Grey",
                  price= 399.99 }],
              total= 415.98,
              order_date= {{2013,10,1},{14,42,26}}},

            #order{
              order_id= 2,
              customer_id= 1,
              salesperson_id= 9001,
              items= [
                #item{
                  item_id= "OAX19XWN0QP",
                  title= "GoSlo Digital Camera",
                  price= 359.99 }],
              total= 359.99,
              order_date= {{2013,10,15},{16,43,16}}},

            #order {
              order_id= 3,
              customer_id= 1,
              salesperson_id= 9000,
              items= [
                #item{
                  item_id= "WYK12EPU5EZ",
                  title= "Call of Battle= Goats - Gamesphere 4",
                  price= 69.99 },
                #item{
                  item_id= "TJB84HAA8OA",
                  title= "Bricko Building Blocks",
                  price= 4.99 }],
              total= 74.98,
              order_date= {{2013,11,3},{17,45,28}}}
          ].

OrderSummary =  #order_summary{
                  customer_id= 1,
                  summaries= [
                    #order_summary_entry{
                      order_id= 1,
                      total= 415.98,
                      order_date= {{2013,10,1},{14,42,26}}
                    },
                    #order_summary_entry{
                      order_id= 2,
                      total= 359.99,
                      order_date= {{2013,10,15},{16,43,16}}
                    },
                    #order_summary_entry{
                      order_id= 3,
                      total= 74.98,
                      order_date= {{2013,11,3},{17,45,28}}}]}.

## Remember to replace the ip and port parameters with those that match your cluster.
{ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 10017).

CustomerBucket = <<"Customers">>.
OrderBucket = <<"Orders">>.
OrderSummariesBucket = <<"OrderSummaries">>.

CustObj = riakc_obj:new(CustomerBucket,
                        list_to_binary(
                          integer_to_list(
                            Customer#customer.customer_id)),
                        Customer).

riakc_pb_socket:put(Pid, CustObj).

StoreOrder = fun(Order) ->
  OrderObj = riakc_obj:new(OrderBucket,
                           list_to_binary(
                             integer_to_list(
                               Order#order.order_id)),
                           Order),
  riakc_pb_socket:put(Pid, OrderObj)
end.

lists:foreach(StoreOrder, Orders).


OrderSummaryObj = riakc_obj:new(OrderSummariesBucket,
                                list_to_binary(
                                  integer_to_list(
                                    OrderSummary#order_summary.customer_id)),
                                OrderSummary).

riakc_pb_socket:put(Pid, OrderSummaryObj).

```

While individual `Customer` and `Order` objects don't change much (or
shouldn't change), the `OrderSummaries` object will likely change often.
It will do double duty by acting as an index for all a customer's
orders, and also holding some relevant data such as the order total,
etc. If we showed this information in our application often, it's only
one extra request to get all the info.

```erlang
{ok, FetchedCustomer} = riakc_pb_socket:get(Pid,
                                            CustomerBucket,
                                            <<"1">>).
{ok, FetchedSummary} = riakc_pb_socket:get(Pid,
                                           OrderSummariesBucket,
                                           <<"1">>).
rp({binary_to_term(riakc_obj:get_value(FetchedCustomer)),
    binary_to_term(riakc_obj:get_value(FetchedSummary))}).
```

Which returns our amalgamated objects:

```erlang
{#customer{customer_id = 1,name = "John Smith",
           address = "123 Main Street",city = "Columbus",
           state = "Ohio",zip = "43210",phone = "+1-614-555-5555",
           created_date = {{2013,10,1},{14,30,26}}},
 #order_summary{customer_id = 1,
                summaries = [#order_summary_entry{order_id = 1,
                                                  total = 415.98,
                                                  order_date = {{2013,10,1},{14,42,26}}},
                             #order_summary_entry{order_id = 2,total = 359.99,
                                                  order_date = {{2013,10,15},{16,43,16}}},
                             #order_summary_entry{order_id = 3,total = 74.98,
                                                  order_date = {{2013,11,3},{17,45,28}}}]}}
```

While this pattern is very easy and extremely fast with respect to
queries and complexity, it's up to the application to know about these
intrinsic relationships.

## Secondary Indexes

{{% note %}}
Secondary indexes in Riak KV require a sorted backend: [Memory]({{<baseurl>}}riak/kv/2.1.4/setup/planning/backend/memory) or [LevelDB]({{<baseurl>}}riak/kv/2.1.4/setup/planning/backend/leveldb). [Bitcask]({{<baseurl>}}riak/kv/2.1.4/setup/planning/backend/bitcask) does not support secondary indexes.

See [Using Secondary Indexes (2i)]({{<baseurl>}}riak/kv/2.1.4/developing/usage/secondary-indexes) for more information on developing with secondary indexes.
{{% /note %}}

If you're coming from an SQL world, Secondary Indexes (2i) are a lot
like SQL indexes. They are a way to quickly look up objects based on a
secondary key, without scanning through the whole dataset. This makes it
very easy to find groups of related data by values, or even ranges of
values. To properly show this off, we will now add some more data to our
application, and add some secondary index entries at the same time.

```erlang
FormatDate = fun(DateTime) ->
  {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
  lists:concat([Year,Month,Day,Hour,Min,Sec])
end.

AddIndicesToOrder = fun(OrderKey) ->
  {ok, Order} = riakc_pb_socket:get(Pid, OrderBucket,
                                    list_to_binary(integer_to_list(OrderKey))),

  OrderData = binary_to_term(riakc_obj:get_value(Order)),
  OrderMetadata = riakc_obj:get_update_metadata(Order),

  MD1 = riakc_obj:set_secondary_index(OrderMetadata,
                                      [{{binary_index, "order_date"},
                                        [FormatDate(OrderData#order.order_date)]}]),

  MD2 = riakc_obj:set_secondary_index(MD1,
                                      [{{integer_index, "salesperson_id"},
                                        [OrderData#order.salesperson_id]}]),

  Order2 = riakc_obj:update_metadata(Order,MD2),
  riakc_pb_socket:put(Pid,Order2)
end.

lists:foreach(AddIndicesToOrder, [1,2,3]).

```

As you may have noticed, ordinary Key/Value data is opaque to 2i, so we
have to add entries to the indices at the application level. Now let's
find all of Jane Appleseed's processed orders, we'll lookup the orders
by searching the `saleperson_id_int` index for Jane's id of `9000`.

```erlang
riakc_pb_socket:get_index_eq(Pid, OrderBucket, {integer_index, "salesperson_id"}, 9000).
```

Which returns:

```erlang
{ok,{index_results_v1,[<<"1">>,<<"3">>],
                      undefined,undefined}}
```

Jane processed orders 1 and 3. We used an "integer" index to reference
Jane's id, next let's use a "binary" index. Now, let's say that the VP
of Sales wants to know how many orders came in during October 2013. In
this case, we can exploit 2i's range queries. Let's search the
`order_date_bin` index for entries between `20131001` and `20131031`.

```erlang
riakc_pb_socket:get_index_range(Pid, OrderBucket,
                                {binary_index, "order_date"},
                                <<"20131001">>, <<"20131031">>).
```

Which returns:

```erlang
{ok,{index_results_v1,[<<"1">>,<<"2">>],
                      undefined,undefined}}
```

Boom! Easy-peasy. We used 2i's range feature to search for a range of
values, and demonstrated binary indexes.

So, to recap:

* You can use Secondary Indexes to quickly lookup an object based on a
  secondary id other than the object's key.
* Indices can have either Integer or Binary(String) keys
* You can search for specific values, or a range of values
* Riak will return a list of keys that match the index query
