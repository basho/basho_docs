---
title: Riak Java Client Buckets
project: java
version: 0.10.0+
document: cookbook
toc: false
audience: beginner
keywords: [java, client]
---

## Creating or modifying a bucket in Riak

```java
import com.basho.riak.client.IRiakClient;
import com.basho.riak.client.RiakException;
import com.basho.riak.client.RiakFactory;
import com.basho.riak.client.bucket.Bucket;

public class App
{
    public static void main(String[] args) throws RiakException
    {
        riakClient = RiakFactory.httpClient();
  
        // If the bucket does not exist in Riak, it will be created with the default properties
        // when you use the fetchBucket() method.
        Bucket myBucket = riakClient.fetchBucket("TestBucket").execute();

        // By using the createBucket() method you can specify properties' values. If the bucket
        // already exists in Riak the bucket properties will be updated.
        Bucket myOtherBucket = riakClient.createBucket("TestBucket").nVal(2).r(1).execute();

        // If you have an existing Bucket object, you can modify the values and update Riak
        Bucket existingBucket = riakClient.fetchBucket("TestBucket").execute();
        existingBucket = riakClient.updateBucket(existingBucket).nVal(3).r(2).execute();

        riakClient.shutdown();
    }
}
```
