---
title: "Taste of Riak: Go"
project: riak
version: 1.4.0+
document: guide
toc: true
audience: beginner
keywords: [developers, client, go, golang]
---

If you haven't set up a Riak Node and started it, please visit the
[[Prerequisites|Taste of Riak: Prerequisites]] first and ensure you have
[a working installation of Go](http://golang.org/doc/install).

## Client Setup

First install the [Riak Go client](https://github.com/basho/riak-go-client):

```bash
go get github.com/basho/riak-go-client
```

Next download the [Taste of Riak - Go](https://github.com/basho/taste-of-riak/tree/master/go) utilities:

```bash
go get github.com/basho/taste-of-riak/go/util
```

If you are using a single local Riak node, use the following to create a
new client instance:

```golang
package main

import (
  "encoding/binary"
  "encoding/json"
  "sync"

  riak "github.com/basho/riak-go-client"
  util "github.com/basho/taste-of-riak/go/util"
)

func main() {
  var err error

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
}
```

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

In our first object we have stored the integer 1 with the lookup key
of `one`:

```golang
{
  Bucket:      bucket,
  Key:         "one",
  ContentType: "application/octet-stream",
  Value:       val1buf,
}
```

For our second object we stored a simple string value of `two` with a
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
the delete message can be called either against the bucket or the
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

For example, this `struct` that representing some information about
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

All right, so we have some information about our Moby Dick collection
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

## Next Steps

More complex use cases can be composed from these initial create, read,
update, and delete (CRUD) operations. [[In the next chapter|Taste of
Riak: Querying]] we look at how to store and query more complicated and
interconnected data.
