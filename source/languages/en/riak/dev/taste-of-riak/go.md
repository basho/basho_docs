---
title: "Taste of Riak: Go"
project: riak
version: 1.4.0+
document: guide
toc: true
audience: beginner
keywords: [developers, client, go]
---

If you haven't set up a Riak cluster, please visit the [[Prerequisites|Taste
of Riak: Prerequisites]] first.

To try this flavor of Riak, [a working installation of Go](http://golang.org/doc/install) is required.

### Client Setup

[riakpbc](https://github.com/mrb/riakpbc) is a community-maintained Riak
client library for Go.

First, install riakpbc:

```bash
$ go get github.com/mrb/riakpbc
```

Next, create the following directory structure in your `GOPATH` and download [taste_of_riak.go](https://github.com/basho/basho_docs/raw/master/source/data/taste_of_riak.go):

```bash
$ mkdir -p $GOPATH/src/basho.com/tasteofriak
$ cp ~/taste_of_riak.go $GOPATH/src/basho.com/tasteofriak
```

You can now compile and run this via the command line:

```bash
$ cd $GOPATH/src/basho.com/tasteofriak
$ go install
$ $GOPATH/bin/tasteofriak
```

Running it should return:

```text
Creating objects...
Reading objects...
1
true
two
true
Deleting objects...
Creating, reading, and deleting complex objects...
3
true
```

Now we'll walk through the code to see what it actually does at each step.

### Creating Objects

The first thing to do is create a new client instance targeting each of the
Riak nodes in your cluster. If you happen to have the cluster behind a load
balancer, simply provide the IP and port your load balancer is listening on.

```go
client := riakpbc.NewClient([]string{
    "127.0.0.1:10017",
    "127.0.0.1:10027",
    "127.0.0.1:10037"
})
```

From there, we make a connection to all of the nodes (using `Dial`) and
specify a client ID:

```go
if err := client.Dial(); err != nil {
    log.Fatalf("Dialing failed: %v", err)
}

if _, err := client.SetClientId("taste-of-riak"); err != nil {
    log.Fatalf("Setting client ID failed: %v", err)
}
```

Now, let's store the integer `1` with a key of `one`:

```go
if _, err := riak.StoreObject("test", "one", 1); err != nil {
    log.Print(err.Error())
}
```

Next, let’s store a simple string value of `two` with a matching key of `two`.

```go
if _, err := client.StoreObject("test", "two", "two"); err != nil {
    log.Print(err.Error())
}
```

### Reading Objects

Now that we have a few objects stored, let’s retrieve them and make sure they
return the values we expect.

```go
one, err := client.FetchObject("test", "one")
if err != nil {
    log.Print(err.Error())
}
fmt.Println(string(one.GetContent()[0].GetValue()))

one_value, err := strconv.ParseInt(string(one.GetContent()[0].GetValue()), 10, 64)
if err != nil {
    log.Print(err.Error())
}
fmt.Println(one_value == 1)

two, err := client.FetchObject("test", "two")
if err != nil {
    log.Print(err.Error())
}
fmt.Println(string(two.GetContent()[0].GetValue()))
fmt.Println(string(two.GetContent()[0].GetValue()) == "two")
```

<div class="note">
<div class="title">strconv.ParseInt</div>

In order to parse the integer value back from a `[]byte`, we use Go's
[strconv.ParseInt](http://golang.org/pkg/strconv/#ParseInt).

The second argument to `ParseInt` is `10` (signifying base 10) and the third
is `64` (for the bit size).

</div>

### Deleting Objects

To clean up after ourselves, here's how to delete data:

```go
if _, err := client.DeleteObject("test", "one"); err != nil {
    log.Print(err.Error())
}

if _, err := client.DeleteObject("test", "two"); err != nil {
    log.Print(err.Error())
}
```

### Working with Complex Objects

Since the world is a little more complicated than simple integers and strings,
let’s see how we can work with more complex objects.

How about a Go `struct`!

First, ensure that you have the `ExampleData` `struct` definition:

```go
type ExampleData struct {
    Three int `json:"three"`
}
```

This `struct` contains an integer property `Three` that will be serialized as
JSON (because of the `json` tag).

Next, create a coder that marshalls/unmarshalls JSON and associate it with a
new client:

```go
coder := riakpbc.NewCoder("json", riakpbc.JsonMarshaller, riakpbc.JsonUnmarshaller)
coder_client := riakpbc.NewClientWithCoder([]string{
    "127.0.0.1:10017",
    "127.0.0.1:10027",
    "127.0.0.1:10037",
}, coder)
```

After connecting to each node (using `Dial`) and setting a client ID, create
an `ExampleData` `struct` and store it in Riak:

```go
data := ExampleData{
    Three: 3,
}
if _, err := coder_client.StoreStruct("test", "three", &data); err != nil {
    log.Print(err.Error())
}
```

Retrieving a property from a `struct` looks like this:

```go
out := &ExampleData{}
if _, err := coder_client.FetchStruct("test", "three", out); err != nil {
    log.Print(err.Error())
}
fmt.Println(out.Three)
fmt.Println(out.Three == 3)
```

Finally, let’s clean up our mess:

```go
if _, err := client.DeleteObject("test", "three"); err != nil {
    log.Print(err.Error())
}
```
