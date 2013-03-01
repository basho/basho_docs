---
title: Riak Java Client Factory
project: java
version: 0.10.0+
document: cookbook
toc: false
audience: beginner
keywords: [java, client]
---

## HTTP or PB Client with default settings

```java
import com.basho.riak.client.IRiakClient;
import com.basho.riak.client.RiakFactory;
import com.basho.riak.client.RiakException;

public class App
{
    public static void main(String[] args) throws RiakException
    { 
        // Riak HTTP client with defaults
        IRiakClient myDefaultHttpClient = RiakFactory.httpClient();
        // Riak HTTP client using supplied URL
        IRiakClient myHttpClient = RiakFactory.httpClient("http://172.16.1.34:8098/riak");
        // Riak Protocol Buffers client with defaults
        IRiakClient myDefaultPbClient = RiakFactory.pbcClient();
        // Riak Protocol Buffers client with supplied IP and Port
        IRiakClient myPbClient = RiakFactory.pbcClient("172.16.1.34", 8087);

        myDefaultHttpClient.shutdown();
        myHttpClient.shutdown();
        myDefaultPbClient.shutdown();
        myPbClient.shutdown();
    }   
}
```

## HTTP or PB client with supplied configuration

```java
import com.basho.riak.client.IRiakClient;
import com.basho.riak.client.RiakFactory; 
import com.basho.riak.client.raw.http.HTTPClientConfig;
import com.basho.riak.client.raw.pbc.PBClientConfig;
import com.basho.riak.client.RiakException;

public class App
{
    public static void main(String[] args) throws RiakException
    {
        // See Javadoc for HTTPClientConfig.Builder for all options
        HTTPClientConfig cc = HTTPClientConfig.Builder().withPort(8088).withMaxConnections(20).build();
        IRiakClient myHttpClient = RiakFactory.newClient(cc);

        // see Javadoc for PBClientConfig.Builder for all options
        PBClientConfig pbcc = PBClientConfig.Builder().withConnectionTimeoutMillis(30000).withHost("192.168.1.22").build();
        IRiakClient myPbClient = RiakFactory.newClient(pbcc);

        myHttpClient.shutdown();
        myPbClient.shutdown();
    }
}
```

## HTTP or PB cluster client

By using a cluster client (HTTPClusterClient or PBClusterClient) you would get not only fail-over on an operation failure but also round-robin balancing of operations across your nodes. 

When you create either a HTTPClusterClient or a PBClusterClient you provide a list of Raik nodes. When performing an operation there's a Retrier object that is used by the Bucket object. The default Retrier re-tries an operation 3 times before reporting a failure. On each try (the initial and each re-try), a different node is selected from the list of Riak nodes. 

```java
import com.basho.riak.client.IRiakClient;
import com.basho.riak.client.RiakFactory; 
import com.basho.riak.client.raw.http.HTTPClusterConfig;
import com.basho.riak.client.raw.pbc.PBClusterConfig;
import com.basho.riak.client.RiakException;

public class App
{
    public static void main(String[] args) throws RiakException
    {
        int maxConnections = 50;

        /* There is currently a bug in the <= 1.0.4 client in creating a HTTPClusterClient which
         * causes the addHosts() method to not work correctly. Please use the method demonstrated below
         * to add your hosts to the HTTPClusterConfig or upgrade to 1.1.0 +
         */

        HTTPClusterConfig myHttpClusterConfig = new HTTPClusterConfig(maxConnections);
        // See example above for client config options
        HTTPClientConfig myHttpClientConfig = new HTTPClientConfig.Builder().withHost("172.16.1.34").build();
        HTTPClientConfig myHttpClientConfig2 = new HTTPClientConfig.Builder().withHost("172.16.1.35").build();
        myHttpClusterConfig.addClient(myHttpClientConfig);
        myHttpClusterConfig.addClient(myHttpClientConfig2);
        /*******************************************************************/


        /* 
         * That bug has been fixed in 1.1.0 - use the following in >= 1.1.0
         * for adding hosts to a HTTPClusterClient
         */
        myHttpClusterConfig = new HTTPClusterConfig(maxConnections);
        // See example above for client config options
        HTTPClientConfig myHttpClientConfig3 = HTTPClientConfig.defaults();
        myHttpClusterConfig.addHosts(myHttpClientConfig3, "192.168.1.10","192.168.1.11","192.168.1.12");

        IRiakClient myHttpClient = RiakFactory.newClient(myHttpClusterConfig);


        PBClusterConfig myPbClusterConfig = new PBClusterConfig(maxConnections);
        // See above examples for client config options
        PBClientConfig myPbClientConfig = PBClientConfig.defaults();
        myPbClusterConfig.addHosts(myPbClientConfig, "192.168.1.10","192.168.1.11","192.168.1.12");
        IRiakClient myPbClient = RiakFactory.newClient(myPbClusterConfig);

        myHttpClient.shutdown();
        myPbClient.shutdown();
    }
}
```
