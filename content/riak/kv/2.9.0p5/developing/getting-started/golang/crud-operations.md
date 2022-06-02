---
title_supertext: "Getting Started:"
tiGetting Started: CRUD Operations with Go"
description: ""
project: "riak_kv"
project_version: "2.9.0p5"
menu:
  riak_kv-2.9.0p5:
    name: "CRUD Operations"
    identifier: "getting_started_go_crud"
    weight: 100
    parent: "getting_started_go"
toc: true
aliases:
  - /riak/2.9.0p5/developing/getting-started/golang/crud-operations/
  - /riak/2.9.0/developing/getting-started/golang/crud-operations/
  - /riak/kv/2.9.0/developing/getting-started/golang/crud-operations/
  - /riak/kv/2.9.0p1/developing/getting-started/golang/crud-operations/
  - /riak/kv/2.9.0p2/developing/getting-started/golang/crud-operations/
  - /riak/kv/2.9.0p3/developing/getting-started/golang/crud-operations/
  - /riak/kv/2.9.0p4/developing/getting-started/golang/crud-operations/
---


## Creating Objects

First let’s create a few objects and a bucket to keep them in:

```golang
  val1 := uint32(1)
  val1buf := make([]byte, 4)
  binary.LittleEndian.PutUint32(val1buf, val1)

  val2 := "two"

  val3 := struct{ MyValue int }{3} // NB: ensure that members are exported (i.e. capitalized)
  var val3json []byte
  val3json, err = json.Marshal(val3)
  if err != nil {
    util.ErrExit(err)
  }

  bucket := "test"

  util.Log.Println("Creating Objects In Riak...")

  objs := []*riak.Object{
    {
      Bucket:      bucket,
      Key:         "one",
      ContentType: "application/octet-stream",
      Value:       val1buf,
    },
    {
      Bucket:      bucket,
      Key:         "two",
      ContentType: "text/plain",
      Value:       []byte(val2),
    },
    {
      Bucket:      bucket,
      Key:         "three",
      ContentType: "application/json",
      Value:       val3json,
    },
  }

  var cmd riak.Command
  wg := &sync.WaitGroup{}

  for _, o := range objs {
    cmd, err = riak.NewStoreValueCommandBuilder().
      WithContent(o).
      Build()
    if err != nil {
      util.ErrLog.Println(err)
      continue
    }
    a := &riak.Async{
      Command: cmd,
      Wait:    wg,
    }
    if err := c.ExecuteAsync(a); err != nil {
      util.ErrLog.Println(err)
    }
  }

  wg.Wait()
```

In our first object, we have stored the integer 1 with the lookup key
of `one`:

```golang
{
  Bucket:      bucket,
  Key:         "one",
  ContentType: "application/octet-stream",
  Value:       val1buf,
}
```

For our second object, we stored a simple string value of `two` with a
matching key:

```golang
{
  Bucket:      bucket,
  Key:         "two",
  ContentType: "text/plain",
  Value:       []byte(val2),
}
```

Finally, the third object we stored was a bit of JSON:

```golang
{
  Bucket:      bucket,
  Key:         "three",
  ContentType: "application/json",
  Value:       val3json,
}
```

## Reading Objects

Now that we have a few objects stored, let’s retrieve them and make sure
they contain the values we expect.

Requesting the objects by key:

```golang
var cmd riak.Command
wg := &sync.WaitGroup{}

for _, o := range objs {
  cmd, err = riak.NewStoreValueCommandBuilder().
    WithContent(o).
    Build()
  if err != nil {
    util.ErrLog.Println(err)
    continue
  }
  a := &riak.Async{
    Command: cmd,
    Wait:    wg,
  }
  if err := c.ExecuteAsync(a); err != nil {
    util.ErrLog.Println(err)
  }
}

wg.Wait()

util.Log.Println("Reading Objects From Riak...")

d := make(chan riak.Command, len(objs))

for _, o := range objs {
  cmd, err = riak.NewFetchValueCommandBuilder().
    WithBucket(bucket).
    WithKey(o.Key).
    Build()
  if err != nil {
    util.ErrLog.Println(err)
    continue
  }
  a := &riak.Async{
    Command: cmd,
    Wait:    wg,
    Done:    d,
  }
  if err := c.ExecuteAsync(a); err != nil {
    util.ErrLog.Println(err)
  }
}

wg.Wait()
close(d)
```

Converting to JSON to compare a string key to a symbol
key:

```golang
for done := range d {
  f := done.(*riak.FetchValueCommand)
  /* un-comment to dump fetched object as JSON
  if json, jerr := json.MarshalIndent(f.Response, "", "  "); err != nil {
    util.ErrLog.Println(jerr)
  } else {
    util.Log.Println("fetched value: ", string(json))
  }
  */
  obj := f.Response.Values[0]
  switch obj.Key {
  case "one":
    if actual, expected := binary.LittleEndian.Uint32(obj.Value), val1; actual != expected {
      util.ErrLog.Printf("key: %s, actual %v, expected %v", obj.Key, actual, expected)
    }
  case "two":
    if actual, expected := string(obj.Value), val2; actual != expected {
      util.ErrLog.Printf("key: %s, actual %v, expected %v", obj.Key, actual, expected)
    }
  case "three":
    obj3 = obj
    val3.MyValue = 0
    if jerr := json.Unmarshal(obj.Value, &val3); jerr != nil {
      util.ErrLog.Println(jerr)
    } else {
      if actual, expected := val3.MyValue, int(3); actual != expected {
        util.ErrLog.Printf("key: %s, actual %v, expected %v", obj.Key, actual, expected)
      }
    }
  default:
    util.ErrLog.Printf("unrecognized key: %s", obj.Key)
  }
}
```

## Updating Objects

While some data may be static, other forms of data need to be
updated. 

Let’s update some values:

```golang
util.Log.Println("Updating Object Three In Riak...")

val3.MyValue = 42
obj3.Value, err = json.Marshal(val3)
if err != nil {
  util.ErrExit(err)
}

cmd, err = riak.NewStoreValueCommandBuilder().
  WithContent(obj3).
  WithReturnBody(true).
  Build()
if err != nil {
  util.ErrLog.Println(err)
} else {
  if err := c.Execute(cmd); err != nil {
    util.ErrLog.Println(err)
  }
}

svcmd := cmd.(*riak.StoreValueCommand)
svrsp := svcmd.Response
obj3 = svrsp.Values[0]
val3.MyValue = 0
if jerr := json.Unmarshal(obj3.Value, &val3); jerr != nil {
  util.ErrLog.Println(jerr)
} else {
  if actual, expected := val3.MyValue, int(42); actual != expected {
    util.ErrLog.Printf("key: %s, actual %v, expected %v", obj3.Key, actual, expected)
  }
}
util.Log.Println("updated object key: ", obj3.Key)
util.Log.Println("updated object value: ", val3.MyValue)
```

## Deleting Objects

As a last step, we’ll demonstrate how to delete data. You’ll see that
the delete message can be called against either the bucket or the
object.

```golang
for _, o := range objs {
  cmd, err = riak.NewDeleteValueCommandBuilder().
    WithBucket(o.Bucket).
    WithKey(o.Key).
    Build()
  if err != nil {
    util.ErrLog.Println(err)
    continue
  }
  a := &riak.Async{
    Command: cmd,
    Wait:    wg,
  }
  if err := c.ExecuteAsync(a); err != nil {
    util.ErrLog.Println(err)
  }
}

wg.Wait()
```

## Working With Complex Objects

Since the world is a little more complicated than simple integers and
bits of strings, let’s see how we can work with more complex objects.

For example, this `struct` that represents some information about
a book:

```golang
type Book struct {
  ISBN        string
  Title       string
  Author      string
  Body        string
  CopiesOwned uint16
}

book := &Book{
    ISBN:        "1111979723",
    Title:       "Moby Dick",
    Author:      "Herman Melville",
    Body:        "Call me Ishmael. Some years ago...",
    CopiesOwned: 3,
}
```

We now have some information about our Moby Dick collection
that we want to save. Storing this to Riak should look familiar by now:

```golang
var jbook []byte
jbook, err = json.Marshal(book)
if err != nil {
  util.ErrExit(err)
}

bookObj := &riak.Object{
  Bucket:      "books",
  Key:         book.ISBN,
  ContentType: "application/json",
  Value:       jbook,
}

cmd, err = riak.NewStoreValueCommandBuilder().
  WithContent(bookObj).
  WithReturnBody(false).
  Build()
if err != nil {
  util.ErrLog.Println(err)
} else {
  if err := c.Execute(cmd); err != nil {
    util.ErrLog.Println(err)
  }
}
```

If we fetch our book back and print the data:

```golang
cmd, err = riak.NewFetchValueCommandBuilder().
  WithBucket("books").
  WithKey(book.ISBN).
  Build()
if err != nil {
  util.ErrExit(err)
}
if err := c.Execute(cmd); err != nil {
  util.ErrLog.Println(err)
}

fcmd := cmd.(*riak.FetchValueCommand)
bookObj = fcmd.Response.Values[0]
util.Log.Println(string(bookObj.Value))
```

The result is:

```json
{"isbn":"1111979723","title":"Moby Dick","author":"Herman Melville",
"body":"Call me Ishmael. Some years ago...","copies_owned":3}
```

Now, let’s delete the book:

```golang
...
```
