---
title: "Getting Started with Java"
description: ""
project: "riak_kv"
project_version: "2.0.5"
menu:
  riak_kv-2.0.5:
    name: "Java"
    identifier: "getting_started_java"
    weight: 100
    parent: "developing_getting_started"
toc: true
aliases:
  - /riak/2.0.5/dev/taste-of-riak/java
  - /riak/kv/2.0.5/dev/taste-of-riak/java
---



If you haven't set up a Riak Node and started it, please visit [Running A Cluster]({{<baseurl>}}riak/kv/2.0.5/using/running-a-cluster) first.

To try this flavor of Riak, a working installation of Java is required.

## Client Setup

To include the Riak Java client in your project, add it to your
project's dependencies. Here is a Maven example:

```xml
<dependencies>
  <dependency>
    <groupId>com.basho.riak</groupId>
    <artifactId>riak-client</artifactId>
    <version>2.1.1</version>
  </dependency
</dependencies>
```

Next, download
[`TasteOfRiak.java`](https://github.com/basho/basho_docs/raw/master/extras/code-examples/TasteOfRiak.java)
source code for this tutorial, and save it to your working directory.

{{% note title="Configuring for a local cluster" %}}
The `TasteOfRiak.java` file that you downloaded is set up to communicate with
a 1-node Riak cluster listening on `localhost` port 10017. We recommend
modifying the connection info directly within the `setUpCluster()` method.
{{% /note %}}

If you execute the `TasteOfRiak.java` file within your IDE, you should
see the following:

```
Basic object created
Location object created for quote object
StoreValue operation created
Client object successfully created
Object storage operation successfully completed
Success! The object we created and the object we fetched have the same value
Quote object successfully deleted
Book object created
Moby Dick information now stored in Riak
Book object successfully fetched
Success! All of our tests check out
```

Since Java doesn’t have a REPL environment, let's walk through the code
to see what it actually did at each step.

## Setting Up the Cluster

The first step in using the Riak Java client is to create a cluster
object to facilitate all interactions with Riak. You'll see this on line
72:

```java
RiakCluster cluster = setUpCluster();
```

This calls the private `setUpCluster` method which begins on line 25.
Using that `cluster` object, we can instantiate a client object which
will execute all Riak interactions:

```java
RiakClient client = new RiakClient(cluster);
```

## Next Steps

[CRUD Operations]({{<baseurl>}}riak/kv/2.0.5/developing/getting-started/java/crud-operations)
