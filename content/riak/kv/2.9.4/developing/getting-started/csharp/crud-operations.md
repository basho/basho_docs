---
title_supertext: "Getting Started:"
title: "CRUD Operations with C Sharp"
description: ""
project: "riak_kv"
project_version: 2.9.4
menu:
  riak_kv-2.9.4:
    name: "CRUD Operations"
    identifier: "getting_started_csharp_crud"
    weight: 100
    parent: "getting_started_csharp"
toc: true
---

### Creating Objects In Riak

Pinging a Riak cluster sounds like a lot of fun, but eventually someone is going to want us to do productive work. Let's create a class to represent some data and save some objects into Riak.

The Riak .NET Client makes use of a `RiakObject` class to encapsulate Riak key/value objects. At the most basic, a `RiakObject` is responsible for identifying your object and for translating it into a format that can be easily saved to Riak.

Add the `RiakClient.Models` namespace to your using directive. Your usings should look like this:

```csharp
using System;
using System.Collections.Generic;
using RiakClient;
using RiakClient.Models;
```

Add the `Person` class to the `TasteOfRiak` namespace:

```csharp
public class Person
{
    public string EmailAddress { get; set; }
    public string FirstName { get; set; }
    public string LastName { get; set; }
}
```

Now let's create some people!

```csharp
var people = new[]
{
    new Person {
        EmailAddress = "bashoman@basho.com",
        FirstName = "Basho",
        LastName = "Man"
    },
    new Person {
        EmailAddress = "johndoe@gmail.com",
        FirstName = "John",
        LastName = "Doe"
    }
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

In this sample, we create a collection of `Person` objects and then save each `Person` to Riak.

Before saving, we need to create a `RiakObject` that encapsulates the bucket, key, and object to be saved. Once we've created a `RiakObject` from our `Person` object, we can save it to Riak using `Client.Put()`.

Once again, we check the response from Riak. If things are successful, you'll see a helpful message letting you know that your object has been saved to Riak. If things didn't go as planned, there will be an error message displaying the result code and a helpful error message.

### Reading from Riak

Let's find a person!

```csharp
var result = client.Get(contributors, "bashoman@basho.com");
if (result.IsSuccess)
{
    bashoman = result.Value.GetObject<Person>();
    Console.WriteLine("I found {0} in {1}", bashoman.EmailAddress, contributors);
}
else
{
    Console.WriteLine("Something went wrong!");
    Console.WriteLine("{0}: {1}", result.ResultCode, result.ErrorMessage);
}
```

We use `RiakClient.Get` to retrieve an object from Riak. This returns a `RiakResult<RiakObject>` which, like other RiakResults, helpfully encapsulates the communication with Riak.

After verifying that we've been able to communicate with Riak *and* that we have a successful result, we use `GetObject<T>` to deserialize our object.

### Modifying Existing Data

Let's say that Basho Man has decided to be known as Riak Man:

```csharp
bashoman.FirstName = "Riak";

var o = new RiakObject(contributors, bashoman.EmailAddress, bashoman);
var updateResult = client.Put(o);
if (updateResult.IsSuccess)
{
    Console.WriteLine("Successfully updated {0} in {1}", bashoman.EmailAddress, contributors);
}
else
{
    Console.WriteLine("Something went wrong!");
    Console.WriteLine("{0}: {1}", updateResult.ResultCode, updateResult.ErrorMessage);
}
```

Updating an object involves creating a new `RiakObject` then using `RiakClient.Put` to save the existing object.

### Deleting Data

```csharp
var deleteResult = client.Delete(contributors, "johndoe@gmail.com");
if (deleteResult.IsSuccess)
{
    Console.WriteLine("Successfully got rid of John Doe");
}
else
{
    Console.WriteLine("Something went wrong!");
    Console.WriteLine("{0}: {1}", deleteResult.ResultCode, deleteResult.ErrorMessage);
}
```

Just like other operations, we check the results that have come back from Riak to make sure the object was successfully deleted.

The Riak .NET Client has a lot of additional functionality that makes it easy to build rich, complex applications with Riak. Check out the [documentation](https://github.com/basho/riak-dotnet-client/wiki) to learn more about working with the Riak .NET Client and Riak.

