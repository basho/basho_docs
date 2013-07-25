---
title: "Taste of Riak: C#"
project: riak
version: 1.4.0+
document: guide
toc: true
audience: beginner
keywords: [developers, client, csharp]
---

If you haven't set up a Riak Node and started it, please visit the [[Prerequisites|Taste of Riak: Prerequisites]] first.

To try this flavor of Riak, a working installation of the .NET Framework or Mono is required. 

### Client Setup

Install [CorrugatedIron](http://corrugatediron.org) through [Nuget](http://nuget.org/packages/corrugatediron) or the Visual Studio Nuget package manager.

<div class="note">
<div class="title">Configuring for a remote cluster</div>
By default, CorrugatedIron will add a section to your `app.config` file for a four node local cluster. If you are using a remote cluster, open up `app.config` and change lines 12-16 to point to your remote cluster.

A working version of this demo code is available in the [CorrugatedIron sample projects](http://github.com/DistributedNonsense/CorrugatedIron.Samples) on GitHub.
</div>


### Connecting to Riak

Connecting to Riak with CorrugatedIron is as easy as creating a cluster object and then creating a new client object.

Drop the following code a brand new file:

```csharp
using System;
using CorrugatedIron;

namespace TasteOfRiak
{
    class Program
    {
        static void Main(string[] args)
        {
        	// don't worry, we'll use this string later
	        const string contributors = "contributors";
            var cluster = RiakCluster.FromConfig("riakConfig");
            var client = cluster.CreateClient();
        }
    }
}
```

This creates a new `RiakCluster` which is used to create a new `RiakClient`. A `RiakCluster` object handles all the details of tracking active nodes and also provides load balancing. The `RiakClient` is used to send commands to Riak.

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

This is some simple code to test that a node in a Riak cluster is online - we send a simple ping message. Even if the cluster isn't present, CorrugatedIron will return a response message. It's important to check that your activity was successful by using the `IsSuccess` property and then checking any errors and result codes.

### Saving Objects to Riak

Pinging a Riak cluster sounds like a lot of fun, but eventually someone is going to want us to do productive work. Let's create a class to represent some data and save some objects into Riak.

CorrugatedIron makes use of a `RiakObject` class to encapsulate Riak functionality. At the most basic, a `RiakObject` is responsible for identifying your object and for translating it into a format that can be easily saved to Riak.

Add the `CorrugatedIron.Models` namespace to your using directive. Your usings should look like this:

```csharp
using System;
using System.Collections.Generic;
using CorrugatedIron;
using CorrugatedIron.Models;
```

Add the `Person` class to the `TasteOfRiak` namespace:

```csharp
public class Person {
    public string EmailAddress { get; set; }
    public string FirstName { get; set; }
    public string LastName { get; set; }
}
```

Now let's create some people!

```csharp
var people = new List<Person>
{
    new Person {EmailAddress = "oj@buffered.io", FirstName = "OJ", LastName = "Reeves"},
    new Person {EmailAddress = "jeremiah@brentozar.com", FirstName = "Jeremiah", LastName = "Peschka"}
};

foreach (var person in people)
{
    var o = new RiakObject(contributors, person.EmailAddress, person);
    var putResult = client.Put(o);

    if (putResult.IsSuccess)
    {
        Console.WriteLine("Successfully saved {1} to bucket {0}", o.Key, o.Bucket);
    }
    else
    {
        Console.WriteLine("Are you *really* sure Riak is running?");
        Console.WriteLine("{0}: {1}", putResult.ResultCode, putResult.ErrorMessage);
    }
}
```

In this sample, we create a `List<Person>` and then save each `Person` to Riak. 

Before saving, we need to create a `RiakObject` that encapsulates the bucket, key, and object to be saved. Once we've created a `RiakObject` from our `Person` object, we can save it to Riak using `Client.Put()`.

Once again, we check the response from Riak. If things are successful, you'll see a helpful message letting you know that your object has been saved to Riak. If things didn't go as planned, there will be an error message displaying the result code and a helpful error message.

### Reading from Riak

Let's find a person!

```csharp
var ojResult = client.Get(contributors, "oj@buffered.io");
var oj = new Person();

if (ojResult.IsSuccess)
{
    oj = ojResult.Value.GetObject<Person>();
    Console.WriteLine("I found {0} in {1}", oj.EmailAddress, contributors);
}
else
{
    Console.WriteLine("Something went wrong!");
    Console.WriteLine("{0}: {1}", ojResult.ResultCode, ojResult.ErrorMessage);
}
```

We use `RiakClient.Get` to retrieve an object from Riak. This returns a `RiakResult<RiakObject>` which, like other RiakResults, helpfully encapsulates the communication with Riak.

After verifying that we've been able to communicate with Riak *and* that we have a successful result, we use `GetObject<T>` to deserialize our object. 

### Modifying Existing Data

It's easy to modify or even delete existing data.

Let's say that OJ has decided to be known as Oliver:

```csharp
oj.FirstName = "Oliver";

var o = new RiakObject(contributors, oj.EmailAddress, oj);
var updateResult = client.Put(o);

if (updateResult.IsSuccess)
{
    Console.WriteLine("Successfully updated {0} in {1}", oj.EmailAddress, contributors);
}
else
{
    Console.WriteLine("Something went wrong!");
    Console.WriteLine("{0}: {1}", updateResult.ResultCode, updateResult.ErrorMessage);
}
```

Updating an object is as simply as creating a new object - just use `RiakClient.Put` to save the existing object. 

### Deleting Data

Deletes are equally as easy:

```csharp
var deleteResult = client.Delete(contributors, "jeremiah@brentozar.com");

if (deleteResult.IsSuccess)
{
    Console.WriteLine("Successfully got rid of a devious person");
}
else
{
    Console.WriteLine("Something went wrong!");
    Console.WriteLine("{0}: {1}", deleteResult.ResultCode, deleteResult.ErrorMessage);
}
```

Just like other operations, we check the results that have come back from Riak to make sure the object was successfully deleted. Of course, if you don't care about that, you can just ignore the result.

### Next Steps

CorrugatedIron has a lot of additional functionality that makes it easy to build rich, complex applications with Riak. Check out the [documentation](http://corrugatediron.org/) or [sample projects](http://github.com/DistributedNonsense/CorrugatedIron.Samples) to learn more about working with CorrugatedIron and Riak.