---
title: "Getting Started with C Sharp"
description: ""
project: "riak_kv"
project_version: "2.2.6"
menu:
  riak_kv-2.2.6:
    name: "C Sharp"
    identifier: "getting_started_csharp"
    weight: 103
    parent: "developing_getting_started"
toc: true
aliases:
  - /riak/2.2.6/dev/taste-of-riak/csharp
  - /riak/kv/2.2.6/dev/taste-of-riak/csharp
---



If you haven't set up a Riak Node and started it, please visit [Running A Cluster]({{<baseurl>}}riak/kv/2.2.6/using/running-a-cluster) first.

To try this flavor of Riak, a working installation of the .NET Framework or Mono is required. 

### Client Setup

Install [the Riak .NET Client](https://github.com/basho/riak-dotnet-client/wiki/Installation) through [NuGet](http://nuget.org/packages/RiakClient) or the Visual Studio NuGet package manager.

{{% note title="Configuring for a remote cluster" %}}
By default, the Riak .NET Client will add a section to your `app.config` file
for a four node local cluster. If you are using a remote cluster, open up
`app.config` and change the `hostAddress` values to point to nodes in your
remote cluster.
{{% /note %}}

### Connecting to Riak

Connecting to Riak with the Riak .NET Client requires creating a cluster object and then creating a new client object.

```csharp
using System;
using RiakClient;

namespace TasteOfRiak
{
    class Program
    {
        static void Main(string[] args)
        {
          // don't worry, we'll use this string later
          const string contributors = "contributors";
            IRiakEndpoint cluster = RiakCluster.FromConfig("riakConfig");
            IRiakClient client = cluster.CreateClient();
        }
    }
}
```

This creates a new `RiakCluster` which is used to create a new `RiakClient`. A `RiakCluster` object handles all the details of tracking active nodes and also provides load balancing. The `RiakClient` is used to send commands to Riak. *Note:* the `IRiakEndpoint` object implements `IDisposable` and should be correctly disposed when you're done communicating with Riak.

Let's make sure the cluster is online. Add this to your `Main` method:

```csharp
var pingResult = client.Ping();

if (pingResult.IsSuccess)
{
    Console.WriteLine("pong");
}
else
{
    Console.WriteLine("Are you sure Riak is running?");
    Console.WriteLine("{0}: {1}", pingResult.ResultCode, pingResult.ErrorMessage);
}
```

This is some simple code to test that a node in a Riak cluster is online - we send a simple ping message. Even if the cluster isn't present, the Riak .NET Client will return a response message. It's important to check that your activity was successful by using the `IsSuccess` property and then checking any errors and result codes.

We are now ready to start interacting with Riak.

## Next Steps

[CRUD Operations]({{<baseurl>}}riak/kv/2.2.6/developing/getting-started/csharp/crud-operations)
