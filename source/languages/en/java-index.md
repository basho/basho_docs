---
title: Riak Java Client
project: java
version: 0.10.0+
document: tutorial
toc: false
index: true
audience: beginner
keywords: []
body_id: riak-java-client
versions: true
---

## Quick start

Assuming you’re running Riak on localhost on the default ports getting started is as simple as:

```java
// create a client (see Configuration below in this README for more details)
IRiakClient riakClient = RiakFactory.pbcClient(); //or RiakFactory.httpClient();

// create a new bucket
Bucket myBucket = riakClient.createBucket("myBucket").execute();

// add data to the bucket
myBucket.store("key1", "value1").execute();

//fetch it back
IRiakObject myData = myBucket.fetch("key1").execute();

// you can specify extra parameters to the store operation using the
// fluent builder style API
myData = myBucket.store("key1", "value2").returnBody(true).execute();

// delete
myBucket.delete("key1").rw(3).execute();
```

## Some History

This riak-java-client API is new. Prior to this version there were two separate clients, one for protocol buffers, one for REST, both in the same library and both with quite different APIs.

### Deprecated

The REST client (which you can still use directly) has been moved to

```java
com.basho.riak.client.http.RiakClient
```

Though a deprecated RiakClient still exists at

```java
com.basho.riak.client.RiakClient
```

for another release or two to ease transition. All the REST client’s HTTP specific classes have been moved to

```java
com.basho.riak.client.http.*
```

and the originals retained but deprecated. If you want to use the legacy, low-level client directly please use the newly packaged version. The deprecated classes will be deleted in the next or following release to clean up the namespaces.

At that time `IRiak*` will become `Riak*` and any `I*` names will be deprecated then dropped. We're sorry about the unpleasant naming conventions in the short term.
