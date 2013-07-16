---
title: Riak Java Client Fetching Data
project: java
version: 0.10.0+
document: cookbook
toc: false
audience: beginner
keywords: [java, client]
---

### A word or two about Riak, CAP Theorem and eventual consistency

Unless you're already familiar with CAP Theorem and eventual consistency, taking the time to read through at least [[CAP Controls]] would be well worth your while. 

It's ok, we'll wait.

Ok! Now that you've read through that and understand that Riak is a system that favors AP with eventual C, this might make some sense to you. 

### Fetching data in Riak with the Java client

In Riak data is stored in buckets. Those buckets have a number of options and tunable parameters, one of which is whether or not to allow sibling records. By default, a bucket does **not** allow sibling creation. The Riak Java client is somewhat built around this in that at the most basic level, you can simply say "fetch the data associated with this key" and the single object that is currently in Riak referenced by that key will be returned.

This of course does not reflect how you must use the client if your application is doing a typical read/modify/write cycle and you have multiple threads or instances of your application causing concurrency. We'll discuss that in the [[advanced section|Riak Java Client Fetching Data#advanced]] below.

With that in mind, the following basic examples show how you can retrieve data from Riak.

## Basic Fetch as a String

```java
import com.basho.riak.client.IRiakClient;
import com.basho.riak.client.IRiakObject;
import com.basho.riak.client.RiakException;
import com.basho.riak.client.RiakFactory;
import com.basho.riak.client.bucket.Bucket;

public class App
{
    public static void main(String[] args) throws RiakException
    {
        IRiakClient riakClient = RiakFactory.httpClient();
        Bucket myBucket = riakClient.fetchBucket("TestBucket").execute();
        IRiakObject myObject = myBucket.fetch("TestKey").execute();
        // note that getValueAsString() will return null here if there's no value in Riak
        System.out.println(myObject.getValueAsString());

        riakClient.shutdown();
    }
}
```

## Fetch JSON data, map to POJO

By default, the Riak Java client provides a default Converter (see the [[advanced section|Riak Java Client Fetching Data#advanced]] below for more on this) that will automatically map JSON stored in Riak to a POJO class you provide. 

```java
import com.basho.riak.client.IRiakClient;
import com.basho.riak.client.IRiakObject;
import com.basho.riak.client.RiakException;
import com.basho.riak.client.RiakFactory;
import com.basho.riak.client.bucket.Bucket;

public class App
{
    // { "foo":"Some String", "bar":"some other string","foobar":5 }
    class Pojo {
        public String foo;
        public String bar;
        public int foobar;
    }

    public static void main(String[] args) throws RiakException
    {
        IRiakClient riakClient = RiakFactory.httpClient();
        Bucket myBucket = riakClient.fetchBucket("TestBucket").execute();
        // Note that myObject will be null if it doesn't exist in Riak
        Pojo myObject = myBucket.fetch("TestKey", Pojo.class).execute();
        System.out.println(myObject.foo);

        riakClient.shutdown();
    }
}
```

## Fetch data, changing query parameters for just this request

```java
import com.basho.riak.client.IRiakClient;
import com.basho.riak.client.IRiakObject;
import com.basho.riak.client.RiakException;
import com.basho.riak.client.RiakFactory;
import com.basho.riak.client.bucket.Bucket;
import com.basho.riak.client.cap.Quora;
import com.basho.riak.client.operations.FetchObject;

public class App
{

    public static void main(String[] args) throws RiakException
    {
        IRiakClient riakClient = RiakFactory.httpClient();
        Bucket myBucket = riakClient.fetchBucket("TestBucket").execute();
        FetchObject<IRiakObject> fetchObject = myBucket.fetch("TestKey");
        IRiakObject myObject = fetchObject.r(Quora.ONE).pr(Quora.ONE).execute();
        // Note getValueAsString() will return null if the object does not exist in Riak
        System.out.println(myObject.getValueAsString());
        riakClient.shutdown();
    }
}
```

<hr>
<a name="advanced"></a>
# The Hard Way

## Eventual Consistency; Resolvers, and Converters

In many environments, you're going to configure your buckets to allow siblings and write the code that deals with them. 

It's worth mentioning here that there are two ways to handle a read/modify/write cycle with the Java client:

* Do a fetch, modify the object, then store it back to Riak in a separate store operation
* Encapsulate the entire read/modify/write cycle in the store operation

Note that if you do the former with your own POJO you must include a `byte[]` or `VClock` field annotated with the `@RiakVClock` annotation. This preserves the vector clock in your POJO and is used during the subsequent store operation. For more information on this please see the section on [[Riak Java Client Storing Data]]. **(Note: This is a new feature of the 1.0.6 client release. Previous versions do not have this functionality and the read/modify/write cycle must be encapsulated in the store operation.)**

There are two interfaces you'll use:

* [ConflictResolver&lt;T&gt;](http://basho.github.com/riak-java-client/1.1.0/com/basho/riak/client/cap/ConflictResolver.html)<BR>
    This interface is used to resolve sibling records returned by Riak
* [Converter&lt;T&gt;](http://basho.github.com/riak-java-client/1.1.0/com/basho/riak/client/convert/Converter.html)<br>
    This interface is used to serialize/deserialize data to/from Riak

Here's the anatomy of making a fetch request using the Bucket interface and the returned FetchObject:

<a name="figure1"></a>
### Figure 1
![Riak fetch](http://dl.dropbox.com/u/74693818/RJC-fetch-v2.png)

There are three versions of fetch() available via the Bucket interface. 

The first takes only a (String) key as an argument and returns a FetchObject&lt;IRiakObject&gt;. If you don't wish to do conversion to/from a POJO and instead deal with the raw data, this is what you want to use.

The second takes a (String) key and your POJO class (e.g. MyPojo.class) as arguments. The &lt;T&gt; Generic is inferred from this (see above), providing you with a FetchObject&lt;MyPojo&gt;. 

The third takes an instance of your POJO class with a String field annotated with @RiakKey which contains the key. Again, the &lt;T&gt; is inferred from this and you are returned a FetchObject&lt;MyPojo&gt;.

If you do not provide a ConflictResolver, an instance of [DefaultResolver&lt;T&gt;](http://basho.github.com/riak-java-client/1.1.0/com/basho/riak/client/cap/DefaultResolver.html) is provided. This is actually not really a resolver at all; it throws an exception if siblings are present. If you do not provide a Converter, the [JSONConverter&lt;T&gt;](http://basho.github.com/riak-java-client/1.1.0/com/basho/riak/client/convert/JSONConverter.html) is provided. This Converter uses the Jackson JSON library to deserialize your POJO from JSON stored in Riak. For an example of implementing a customer converter that uses a different serialization library, check out [[using a customer converter|Riak Java Client Custom Converter]]. 

The following example demonstrates the use of these interfaces and your own POJO. These are the same implementations we use for our [[Advanced Examples for storing data in Riak|Riak Java Client Storing Data#advanced]] and model a game "leaderboard" system.

### App.java

```java
import com.basho.riak.client.IRiakClient;
import com.basho.riak.client.RiakException;
import com.basho.riak.client.RiakFactory;
import com.basho.riak.client.bucket.Bucket;
import java.util.Random;

public class App 
{
    public static void main( String[] args ) throws RiakException, InterruptedException
    {
        // We need some data, of course
        String playerNames[] = {"Steve","Brian","Bob" };
        Random generator = new Random();
        GameLeaderboard gl = new GameLeaderboard("SuperCoolGame");

        for (int i = 0; i < 5; i++)
        {
            NameScorePair nsp = new NameScorePair(playerNames[(i+3)%3], generator.nextInt(100));
            gl.addScore(nsp);
        }   

        // Store our initial leaderboard in Riak
        IRiakClient myDefaultHttpClient = RiakFactory.httpClient();
        Bucket b = myDefaultHttpClient.createBucket("demo_bucket").allowSiblings(true).execute();
        b.store(gl).withResolver(new GameLeaderboardResolver()).execute();

      
        // Fetch the data from Riak and output it
        gl = b.fetch("SuperCoolGame", GameLeaderboard.class)
                 .withResolver(new GameLeaderboardResolver())
                 .execute();

        // Output the results!
        for ( NameScorePair n : gl.getScoreList())
        {
            System.out.println(n.getName() + " " + n.getScore());
        }
    }
}
```

### GameLeaderboardResolver.java

```java
import com.basho.riak.client.cap.ConflictResolver;
import java.util.Collection;
import java.util.Iterator;

public class GameLeaderboardResolver implements ConflictResolver<GameLeaderboard>
{

    /*  
     * Riak hands us a list of GameLeaderboard objects. Our job is to reconcile
     * those objects and return a single, resolved GameLeaderboard
     *   
     * In this example, the logic is pretty straightforward. in our GameLeaderboard
     * class we created a addScores(Collection<NameScorePair>) method that will do the 
     * heavy lifting for us. By adding all the lists into one GameLeaderboard
     * via that method, we end up with the top 5 scores from all the siblings
     *   
     * Worth noting is that your ConflictResolver is *always* called, even if  
     * there are no siblings, or even if there is no object in Riak
     */  
        
    public GameLeaderboard resolve(Collection<GameLeaderboard> siblings)
    {   
        if (siblings.size() > 1)
        {       
            // We have siblings, need to resolve them
            Iterator<GameLeaderboard> i = siblings.iterator();

            GameLeaderboard resolvedLeaderboard = new GameLeaderboard(i.next());
                        
            while (i.hasNext())
            {           
                resolvedLeaderboard.addScores(i.next().getScoreList());
            }           
                        
            return resolvedLeaderboard;
        }       
        else if (siblings.size() == 1)
        {       
            // Only one object - just return it
            return siblings.iterator().next();
        }       
        else    
        {       
            // No object returned - return null 
            return null;
        }       
    }   
}
```

### GameLeaderboard.java

```java
import com.basho.riak.client.convert.RiakKey;
import java.util.ArrayList;
import java.util.Collection;
import java.util.TreeSet;

public final class GameLeaderboard
{
    @RiakKey private String gameName;
    private TreeSet<NameScorePair> scoreList = new TreeSet<NameScorePair>();
        
    // required by Jackson for JSON serialization
    public GameLeaderboard() {}
        
    public GameLeaderboard(String gameName)
    {   
        this.gameName = gameName;
    }   
        
    public GameLeaderboard(GameLeaderboard other)
    {   
        this.gameName = other.getGameName();
        this.addScores(other.getScoreList());
    }   
        
    public void addScore(NameScorePair s)
    {   
        scoreList.add(s);
        if (scoreList.size() > 5)
            scoreList.pollFirst();
    }   
        
    public void addScores(Collection<NameScorePair> scores)
    {   
        scoreList.addAll(scores);
        while (scoreList.size() > 5)
            scoreList.pollFirst();
                
    }   
        
    public String getGameName()
    {   
        return gameName;
    }   
        
    public ArrayList<NameScorePair> getScoreList()
    {   
        return new ArrayList<NameScorePair>(scoreList.descendingSet());
    }   
}
```

### NameScorePair.java

```java
public class NameScorePair implements Comparable<NameScorePair>
{
   private String name;
   private int score;

   // Required by Jackson for JSON serialization
   public NameScorePair() {}
   
   public NameScorePair(String name, int score)
   {    
      this.name = name;
      this.score = score;
   }    
        
   public int compareTo(NameScorePair t)
   {    
      if (this.getScore() < t.getScore())
         return -1;     
      else if (this.getScore() > t.getScore())
         return 1;      
      else if (this.getName().equalsIgnoreCase(name))
         return 0;      
      else      
         return -1;     
   }    

   @Override
   public int hashCode()
   {    
      int hash = 3; 
      hash = 47 * hash + (this.name != null ? this.name.hashCode() : 0);
      hash = 47 * hash + this.score;
      return hash;
   }    

   @Override
   public boolean equals(Object o)
   {    
      if (o == null)
      {         
         return false;  
      }         
      else if (o instanceof NameScorePair)
      {         
         return ((name.equalsIgnoreCase(((NameScorePair)o).getName())) &&
            (score == ((NameScorePair)o).getScore()));
      }         
      else      
         return false;  
   }    
        
   public int getScore()
   {    
      return score;
   }    
   
   public String getName()
   {
      return name;
   }
}
```
