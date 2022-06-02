---
title: "Getting Started with Go"
description: ""
project: "riak_kv"
project_version: "2.2.2"
menu:
  riak_kv-2.2.2:
    name: "Go"
    identifier: "getting_started_go"
    weight: 107
    parent: "developing_getting_started"
toc: true
aliases:
  - /riak/2.2.2/dev/taste-of-riak/golang
  - /riak/kv/2.2.2/dev/taste-of-riak/golang
---

If you haven't set up a Riak Node and started it, please visit [Running A Cluster]({{<baseurl>}}riak/kv/2.2.2/using/running-a-cluster) first and ensure you have
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

We are now ready to start interacting with Riak.

## Next Steps

[CRUD Operations]({{<baseurl>}}riak/kv/2.2.2/developing/getting-started/golang/crud-operations)
