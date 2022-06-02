---
title_supertext: "Getting Started:"
title: "Querying with Go"
description: ""
project: "riak_kv"
project_version: 2.9.1
menu:
  riak_kv-2.9.1:
    name: "Querying"
    identifier: "getting_started_go_query"
    weight: 101
    parent: "getting_started_go"
toc: true
aliases:
  - /riak/2.9.1/dev/taste-of-riak/querying-golang
  - /riak/kv/2.9.1/dev/taste-of-riak/querying-golang
---

## Go Version Setup

For the Go version, please download the source from GitHub by either [cloning](https://github.com/basho/taste-of-riak) the source code repository or downloading the [current zip of the master branch](https://github.com/basho/taste-of-riak/archive/master.zip). Ensure that the source is located in your `GOPATH`. The code for this chapter is in `go/ch02/ch02.go`. You may import this code into your favorite editor, or just run it from the command line using the `Makefile` if you are running on a *nix* OS.

>A Quick Note on Querying and Schemas:
>
>Even with a key/value store, you will still have a logical database schema of how all the data relates to one another. This can be as simple as using the same key across multiple buckets for different types of data, to having fields in your data that are related by name. These querying methods will introduce you to some ways of laying out your data in Riak, along with how to query it back.

### Denormalization

If you're coming from a relational database, the easiest way to get your application started with NoSQL is to denormalize your data into related chunks. For example with a customer database, you might have separate tables for Customers, Addresses, Preferences, etc. In Riak KV, you can denormalize all that associated data into a single object and store it into a `Customer` bucket.  You can keep pulling in associated data until you hit one of the big denormalization walls:

* Size Limits (objects greater than 1MB)
* Shared/Referential Data (data that the object doesn't "own")
* Differences in Access Patterns (objects that get read/written once vs. often)

At one of these points we will have to split the model.

### Same Keys - Different Buckets

The simplest way to split up data would be to use the same identity key across different buckets. A good example of this would be a `Customer` object, an `Order` object, and an `OrderSummaries` object that keeps rolled up info about orders such as Total, etc. Let's put some data into Riak KV so we can play with it.

```golang
package main

import (
  "encoding/json"
  "errors"
  "fmt"
  "reflect"
  "sync"
  "time"

  riak "github.com/basho/riak-go-client"
  util "github.com/basho/taste-of-riak/go/util"
)

const (
  timeFmt              = "2006-01-02 15:04:05"
  customersBucket      = "Customers"
  ordersBucket         = "Orders"
  orderSummariesBucket = "OrderSummaries"
)

type Customer struct {
  Name        string
  Address     string
  City        string
  State       string
  Zip         string
  Phone       string
  CreatedDate time.Time
}

type Order struct {
  Id            string
  CustomerId    string
  SalespersonId string
  Items         []*OrderItem
  Total         float32
  Date          time.Time
}

type OrderItem struct {
  Id    string
  Title string
  Price float32
}

type OrderSummary struct {
  CustomerId string
  Summaries  []*OrderSummaryItem
}

type OrderSummaryItem struct {
  Id    string
  Total float32
  Date  time.Time
}

func main() {
  var err error
  var customerId string

  util.Log.Println("Creating Data")

  var cd time.Time
  cd, err = time.Parse(timeFmt, "2013-10-01 14:30:26")
  if err != nil {
    util.ErrExit(err)
  }

  customer := &Customer{
    Name:        "John Smith",
    Address:     "123 Main Street",
    City:        "Columbus",
    State:       "Ohio",
    Zip:         "43210",
    Phone:       "+1-614-555-5555",
    CreatedDate: cd,
  }

  util.Log.Printf("customer: %v", customer)

  util.Log.Println("Starting Client")

  // un-comment-out to enable debug logging
  // riak.EnableDebugLogging = true

  o := &riak.NewClientOptions{
    RemoteAddresses: []string{util.GetRiakAddress()},
  }

  var c *riak.Client
  c, err = riak.NewClient(o)
  if err != nil {
    util.ErrExit(err)
  }

  defer func() {
    if err := c.Stop(); err != nil {
      util.ErrExit(err)
    }
  }()

  util.Log.Println("Storing Customer")

  var cmd riak.Command
  var customerJson []byte

  customerJson, err = json.Marshal(customer)
  if err != nil {
    util.ErrExit(err)
  }

  obj := &riak.Object{
    Bucket:      customersBucket,
    ContentType: "application/json",
    Value:       customerJson,
  }

  cmd, err = riak.NewStoreValueCommandBuilder().
    WithContent(obj).
    WithReturnBody(true).
    Build()
  if err != nil {
    util.ErrExit(err)
  }
  if eerr := c.Execute(cmd); eerr != nil {
    util.ErrExit(eerr)
  }

  svc := cmd.(*riak.StoreValueCommand)
  customerId = svc.Response.GeneratedKey
  if customerId == "" {
    util.ErrExit(errors.New("expected generated customer Id"))
  } else {
    util.Log.Println("Customer ID:", customerId)
  }

  util.Log.Println("Storing Data")

  var orders []*Order
  orders, err = createOrders(customerId)
  if err != nil {
    util.ErrExit(err)
  }

  var orderSummary *OrderSummary
  var orderSummaryJson []byte
  orderSummary = createOrderSummary(customerId, orders)

  ccmds := 1 + len(orders)
  cmds := make([]riak.Command, ccmds)

  // command to store OrderSummary
  orderSummaryJson, err = json.Marshal(orderSummary)
  if err != nil {
    util.ErrExit(err)
  }
  obj = &riak.Object{
    Bucket:      orderSummariesBucket,
    Key:         customerId,
    ContentType: "application/json",
    Value:       orderSummaryJson,
  }
  cmds[0], err = riak.NewStoreValueCommandBuilder().
    WithContent(obj).
    Build()
  if err != nil {
    util.ErrExit(err)
  }

  for i, order := range orders {
    // command to store Order
    var orderJson []byte
    orderJson, err = json.Marshal(order)
    if err != nil {
      util.ErrExit(err)
    }
    obj = &riak.Object{
      Bucket:      ordersBucket,
      Key:         order.Id,
      ContentType: "application/json",
      Value:       orderJson,
    }
    cmds[i+1], err = riak.NewStoreValueCommandBuilder().
      WithContent(obj).
      Build()
    if err != nil {
      util.ErrExit(err)
    }
  }

  errored := false
  wg := &sync.WaitGroup{}
  for _, cmd := range cmds {
    a := &riak.Async{
      Command: cmd,
      Wait:    wg,
    }
    if eerr := c.ExecuteAsync(a); eerr != nil {
      errored = true
      util.ErrLog.Println(eerr)
    }
  }
  wg.Wait()
  if errored {
    util.ErrExit(errors.New("error, exiting!"))
  }
}

func createOrders(customerId string) ([]*Order, error) {
  o := make([]*Order, 3)

  d, err := time.Parse(timeFmt, "2013-10-01 14:42:26")
  if err != nil {
    return nil, err
  }
  o[0] = &Order{
    Id:            "1",
    CustomerId:    customerId,
    SalespersonId: "9000",
    Items: []*OrderItem{
      {
        Id:    "TCV37GIT4NJ",
        Title: "USB 3.0 Coffee Warmer",
        Price: 15.99,
      },
      {
        Id:    "PEG10BBF2PP",
        Title: "eTablet Pro, 24GB; Grey",
        Price: 399.99,
      },
    },
    Total: 415.98,
    Date:  d,
  }

  d, err = time.Parse(timeFmt, "2013-10-15 16:43:16")
  if err != nil {
    return nil, err
  }
  o[1] = &Order{
    Id:            "2",
    CustomerId:    customerId,
    SalespersonId: "9001",
    Items: []*OrderItem{
      {
        Id:    "OAX19XWN0QP",
        Title: "GoSlo Digital Camera",
        Price: 359.99,
      },
    },
    Total: 359.99,
    Date:  d,
  }

  d, err = time.Parse(timeFmt, "2013-11-03 17:45:28")
  if err != nil {
    return nil, err
  }
  o[2] = &Order{
    Id:            "3",
    CustomerId:    customerId,
    SalespersonId: "9000",
    Items: []*OrderItem{
      {
        Id:    "WYK12EPU5EZ",
        Title: "Call of Battle : Goats - Gamesphere 4",
        Price: 69.99,
      },
      {
        Id:    "TJB84HAA8OA",
        Title: "Bricko Building Blocks",
        Price: 4.99,
      },
    },
    Total: 74.98,
    Date:  d,
  }

  return o, nil
}

func createOrderSummary(customerId string, orders []*Order) *OrderSummary {

  s := &OrderSummary{
    CustomerId: customerId,
    Summaries:  make([]*OrderSummaryItem, len(orders)),
  }

  for i, o := range orders {
    s.Summaries[i] = &OrderSummaryItem{
      Id:    o.Id,
      Total: o.Total,
      Date:  o.Date,
    }
  }

  return s
}
```

While individual `Customer` and `Order` objects don't change much (or shouldn't change), the `Order Summaries` object will likely change often. It will do double duty by acting as an index for all a customer's orders and also holding some relevant data, such as the order total, etc. If we showed this information in our application often, it's only one extra request to get all the info.

```golang
util.Log.Println("Fetching related data by shared key")

cmds = cmds[:0]

// fetch customer
cmd, err = riak.NewFetchValueCommandBuilder().
  WithBucket(customersBucket).
  WithKey(customerId).
  Build()
if err != nil {
  util.ErrExit(err)
}
cmds = append(cmds, cmd)

// fetch OrderSummary
cmd, err = riak.NewFetchValueCommandBuilder().
  WithBucket(orderSummariesBucket).
  WithKey(customerId).
  Build()
if err != nil {
  util.ErrExit(err)
}
cmds = append(cmds, cmd)

doneChan := make(chan riak.Command)
errored = false
for _, cmd := range cmds {
  a := &riak.Async{
    Command: cmd,
    Done:    doneChan,
  }
  if eerr := c.ExecuteAsync(a); eerr != nil {
    errored = true
    util.ErrLog.Println(eerr)
  }
}
if errored {
  util.ErrExit(errors.New("error, exiting!"))
}

for i := 0; i < len(cmds); i++ {
  select {
  case d := <-doneChan:
    if fv, ok := d.(*riak.FetchValueCommand); ok {
      obj := fv.Response.Values[0]
      switch obj.Bucket {
      case customersBucket:
        util.Log.Printf("Customer     1: %v", string(obj.Value))
      case orderSummariesBucket:
        util.Log.Printf("OrderSummary 1: %v", string(obj.Value))
      }
    } else {
      util.ErrExit(fmt.Errorf("unknown response command type: %v", reflect.TypeOf(d)))
    }
  case <-time.After(5 * time.Second):
    util.ErrExit(errors.New("fetch operations took too long"))
  }
}
```

Which returns our amalgamated objects:

```sh
2015/12/29 09:44:10 OrderSummary 1: {"CustomerId":"I4R9AdTpJ7RL13qj14ED9Qjzbyy","Summaries":[{"Id":"1","Total":415.98,"Date":"2013-10-01T14:42:26Z"},{"Id":"2","Total":359.99,"Date":"2013-10-15T16:43:16Z"},{"Id":"3","Total":74.98,"Date":"2013-11-03T17:45:28Z"}]}
2015/12/29 09:44:10 Customer     1: {"Name":"John Smith","Address":"123 Main Street","City":"Columbus","State":"Ohio","Zip":"43210","Phone":"+1-614-555-5555","CreatedDate":"2013-10-01T14:30:26Z"
```

While this pattern is very easy and extremely fast with respect to queries and complexity, it's up to the application to know about these intrinsic relationships.  


### Secondary Indexes

{{% note %}}
Secondary indexes in Riak KV require a sorted backend: [Memory]({{<baseurl>}}riak/kv/2.9.1/setup/planning/backend/memory) or [LevelDB]({{<baseurl>}}riak/kv/2.9.1/setup/planning/backend/leveldb). [Bitcask]({{<baseurl>}}riak/kv/2.9.1/setup/planning/backend/bitcask) does not support secondary indexes.

See [Using Secondary Indexes (2i)]({{<baseurl>}}riak/kv/2.9.1/developing/usage/secondary-indexes) for more information on developing with secondary indexes.
{{% /note %}}

If you're coming from a SQL world, Secondary Indexes (2i) are a lot like SQL indexes. They are a way to quickly look up objects based on a secondary key, without scanning through the whole dataset. This makes it very easy to find groups of related data by values or ranges of values. To properly show this off, we will add some more data to our application, and add some secondary index entries at the same time:

```golang
util.Log.Println("Adding Index Data")

// fetch orders to add index data
cmds = cmds[:0]

for _, order := range orders {
  cmd, err = riak.NewFetchValueCommandBuilder().
    WithBucket(ordersBucket).
    WithKey(order.Id).
    Build()
  if err != nil {
    util.ErrExit(err)
  }
  cmds = append(cmds, cmd)
}

errored = false
for _, cmd := range cmds {
  a := &riak.Async{
    Command: cmd,
    Done:    doneChan,
  }
  if eerr := c.ExecuteAsync(a); eerr != nil {
    errored = true
    util.ErrLog.Println(eerr)
  }
}
if errored {
  util.ErrExit(errors.New("error, exiting!"))
}

errored = false
for i := 0; i < len(cmds); i++ {
  select {
  case d := <-doneChan:
    if fv, ok := d.(*riak.FetchValueCommand); ok {
      obj := fv.Response.Values[0]
      switch obj.Key {
      case "1":
        obj.AddToIntIndex("SalespersonId_int", 9000)
        obj.AddToIndex("OrderDate_bin", "2013-10-01")
      case "2":
        obj.AddToIntIndex("SalespersonId_int", 9001)
        obj.AddToIndex("OrderDate_bin", "2013-10-15")
      case "3":
        obj.AddToIntIndex("SalespersonId_int", 9000)
        obj.AddToIndex("OrderDate_bin", "2013-11-03")
      }
      scmd, serr := riak.NewStoreValueCommandBuilder().
        WithContent(obj).
        Build()
      if serr != nil {
        util.ErrExit(serr)
      }
      a := &riak.Async{
        Command: scmd,
        Wait:    wg,
      }
      if eerr := c.ExecuteAsync(a); eerr != nil {
        errored = true
        util.ErrLog.Println(eerr)
      }
    } else {
      util.ErrExit(fmt.Errorf("unknown response command type: %v", reflect.TypeOf(d)))
    }
  case <-time.After(5 * time.Second):
    util.ErrExit(errors.New("fetch operations took too long"))
  }
}

if errored {
  util.ErrExit(errors.New("error, exiting!"))
}

wg.Wait()
close(doneChan)
```

As you may have noticed, ordinary key/value data is opaque to 2i, so we have to add entries to the indexes at the application level.

Now let's find all of Jane Appleseed's processed orders. We'll lookup the orders by searching the `saleperson_id_int` index for Jane's id of `9000`:

```golang
util.Log.Println("Index Queries")

cmd, err = riak.NewSecondaryIndexQueryCommandBuilder().
  WithBucket(ordersBucket).
  WithIndexName("SalespersonId_int").
  WithIndexKey("9000").
  Build()
if err != nil {
  util.ErrExit(err)
}

if eerr := c.Execute(cmd); eerr != nil {
  util.ErrExit(eerr)
}

qcmd := cmd.(*riak.SecondaryIndexQueryCommand)
for _, rslt := range qcmd.Response.Results {
  util.Log.Println("Jane's Orders, key: ", string(rslt.ObjectKey))
}
```

Which returns:

```sh
2015/12/29 09:44:10 Jane's Orders, key:  3
2015/12/29 09:44:10 Jane's Orders, key:  1
```

Jane processed orders 1 and 3.  We used an *integer* index to reference Jane's id, next let's use a *binary* index.

Let's say that the VP of Sales wants to know how many orders came in during October 2013. In this case, we can exploit 2i's range queries. Let's search the `order_date_bin` index for entries between `20131001` and `20131031`:  

```golang
cmd, err = riak.NewSecondaryIndexQueryCommandBuilder().
  WithBucket(ordersBucket).
  WithIndexName("OrderDate_bin").
  WithRange("2013-10-01", "2013-10-31").
  Build()
if err != nil {
  util.ErrExit(err)
}

if eerr := c.Execute(cmd); eerr != nil {
  util.ErrExit(eerr)
}

qcmd = cmd.(*riak.SecondaryIndexQueryCommand)
for _, rslt := range qcmd.Response.Results {
  util.Log.Println("October's Orders, key: ", string(rslt.ObjectKey))
}
```

Which returns:

```sh
2015/12/29 09:44:10 October's Orders, key:  1
2015/12/29 09:44:10 October's Orders, key:  2
```

Easy!  We used 2i's range feature to search for a range of values, and demonstrated binary indexes.  

So to recap:

* You can use Secondary Indexes to quickly lookup an object based on a secondary id other than the object's key.
* Indexes can have either Integer or Binary(String) keys.
* You can search for specific values or a range of values.
* Riak will return a list of keys that match the index query.
