---
title: Riak Java Client Storing Data
project: java
version: 0.10.0+
document: cookbook
toc: false
audience: beginner
keywords: [java, client]
---

## Storing data in Riak with the Java client

In Riak data is stored in buckets. Those buckets have a number of options and tunable parameters, one of which is whether or not to allow sibling records. By default, a bucket does **not** allow sibling creation. The Riak Java client is somewhat built around this in that at the most basic level, you can simply say "store this data using this key" and anything that is currently in Riak referenced by that key will be overwritten. There are, however, some issues with attempting to do that. 

If you have any type of contention/concurrency where multiple threads or processes are doing read/modify/write operations on those key/values, you are likely to lose writes if the operations interleave. One will overwrite the other. At that point you need to enable siblings and deal with conflict resolution.  

With that in mind, the following basic examples illustrate using Riak with the default bucket options and just storing some data. 

For a more detailed example of how you would store data in Riak in an environment with concurrency, jump down to the [[Advanced Examples|Riak Java Client Storing Data#advanced]] section. 


## Basic Store, data is a String

Using the Bucket.store(String, String) method, your String is stored in Riak as bytes representing UTF-8 text

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
        String myData = "This is my data";
        IRiakClient riakClient = RiakFactory.httpClient();
        Bucket myBucket = riakClient.fetchBucket("TestBucket").execute();
        myBucket.store("TestKey", myData).execute();
        riakClient.shutdown();
    }
}
```

## Store POJO (serialized to JSON)

By passing a POJO to the Bucket.store(String, T) method, your POJO is serialized to JSON using the Jackson library and stored in Riak as UTF-8 text.

```java
import com.basho.riak.client.IRiakClient;
import com.basho.riak.client.IRiakObject;
import com.basho.riak.client.RiakException;
import com.basho.riak.client.RiakFactory;
import com.basho.riak.client.bucket.Bucket;

public class App
{
    class Pojo 
    {
        public String foo;
        public String bar;
        public int foobar;
    }

    public static void main(String[] args) throws RiakException
    {
        Pojo myPojo = new Pojo();
        myPojo.foo = "My foo data";
        myPojo.bar = "My Bar data";
        myPojo.foobar = 5;

        IRiakClient riakClient = RiakFactory.httpClient();
        Bucket myBucket = riakClient.fetchBucket("TestBucket").execute();
        myBucket.store("TestKey", myPojo).execute();
 
        riakClient.shutdown();
    }
}
```

## Store data, changing query parameters for just this request

To override the default parameters in the Bucket, you can specify them prior to calling the execute() method. 

```java
import com.basho.riak.client.IRiakClient;
import com.basho.riak.client.IRiakObject;
import com.basho.riak.client.RiakException;
import com.basho.riak.client.RiakFactory;
import com.basho.riak.client.bucket.Bucket;
import com.basho.riak.client.cap.Quora;
import com.basho.riak.client.operations.StoreObject;

public class App
{
    public static void main(String[] args) throws RiakException
    {
        IRiakClient riakClient = RiakFactory.httpClient();
        Bucket myBucket = riakClient.fetchBucket("TestBucket").execute();
        StoreObject<IRiakObject> storeObject = myBucket.store("TestKey", "TestData");
        storeObject.w(Quora.ONE).pw(Quora.ONE).execute();
        riakClient.shutdown();
    }
}
```

<hr>
<a name="advanced"></a>
# The Hard Way
## Eventual Consistency; Resolvers, Mutators, and Converters

In most environments, you're going to configure your buckets to allow siblings and write the code that deals with them. There are three Interfaces you're going to be using:

* [ConflictResolver&lt;T&gt;](http://basho.github.com/riak-java-client/1.1.0/com/basho/riak/client/cap/ConflictResolver.html)<BR>
    This Interface is used to resolve sibling records returned by Riak
* [Mutation&lt;T&gt;](http://basho.github.com/riak-java-client/1.1.0/com/basho/riak/client/cap/Mutation.html)<br>
    This interface is used to modify an object in Riak
* [Converter&lt;T&gt;](http://basho.github.com/riak-java-client/1.1.0/com/basho/riak/client/convert/Converter.html)<br>
    This interface is used to serialize/deserialize data to/from Riak

One thing worth noting is that the current IRiakClient and its various interfaces aren't likely what you're used to if you've used other datastores' APIs. When using the above classes the current Java client design expects your entire read/modify/write cycle to be encapsulated entirely within the store operation. 

If you do not wish to completely encapsulate the read/modify/write inside the store operation, see [[Storing previously fetched and modified data|Riak Java Client Storing Data#iriakobject]] below. 

The following diagram outlines the anatomy of a read/modify/write cycle using the Bucket interface, your own domain object (T), and the StoreObject it returns:

<a name="figure1"></a>
### Figure 1
![StoreObject anatomy](http://dl.dropbox.com/u/74693818/RJC-store-v4.png)

There are four versions of the store() method available in the Bucket interface:

* StoreObject&lt;IRiakObject&gt; store(String key, byte[] value)
* StoreObject&lt;IRiakObject&gt; store(String key, String value)
* &lt;T&gt; StoreObject&lt;T&gt; store(String key, T o) 
* &lt;T&gt; StoreObject&lt;T&gt; store(T o) 

The first two are only useful if you want to overwrite anything currently in Riak associated with the key you're passing in. Be aware, however, that there is a caveat. An anonymous Mutation instance is created and used. It replaces the data portion of whatever is currently stored in Riak, but _not_ links, secondary indexes, or user metadata. If you want to overwrite everything you will need to supply your own Mutation or Converter that does so. 

The second two are actually what you will most likely use if you are performing a read/modify/write cycle. As noted in [figure 1](#figure1) above, the interface is slightly clunky in that the object being passed in is going to be discarded when you supply the Mutation; it's only used to infer the type. The fourth version will extract the key from the object being passed in before doing so by referencing a String field annotated with @RiakKey.

The following example is a "leader board" for various games. Imagine you were providing a service where game developers would have their games send you a score every time a player completed a game. You are required to store the top 5 scores for each game. We're going to rely on the default JSONConverter to serialize/deserialize our POJO to/from Riak. If you're interested in seeing how you would implement a converter to use a different serialization library, check out [[Using a custom converter|Using-a-custom-Converter]] for an example.

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

        gl = b.fetch("SuperCoolGame", GameLeaderboard.class)
                 .withResolver(new GameLeaderboardResolver())
                 .execute();

        // Output the results!
        for ( NameScorePair n : gl.getScoreList())
        {
            System.out.println(n.getName() + " " + n.getScore());
        }
        System.out.println();
        
        /* 
         * Now that we have a leaderboard in Riak, lets modify it! 
         * This simulates a new name/score pair coming in, and we're going
         * to modify the leaderboard in Riak using the GamLeaderboardMutation
         * We know our sample data only has scores to 100, so using 1000 ensures
         * we'll modify the object
         */
        NameScorePair nsp = new NameScorePair("John", 1000);
        GameLeaderboardMutation glbm = new GameLeaderboardMutation(nsp);
        /* Note that as mentioned in the cookbook, the GameLeaderboard object 
         * passed to Bucket.store() is discarded after the type is inferred 
         * and the key extracted - all modification is done by your Mutation
         * 
         * Note also that we're calling returnBody(true) in order to get
         * the current data back
         */
        gl = b.store(new GameLeaderboard("SuperCoolGame"))
            .withMutator(glbm)
            .withResolver(new GameLeaderboardResolver())
            .returnBody(true)
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

### GameLeaderboardMutation.java

```java
import com.basho.riak.client.cap.Mutation;

public class GameLeaderboardMutation implements Mutation<GameLeaderboard>
{
    private NameScorePair nsp;
        
    public GameLeaderboardMutation(NameScorePair nsp)
    {   
        this.nsp = nsp;
    }   

    /*
     * And at the heart of things is this method. After the data in Riak has
     * been converted to GameLeaderboard Objects and any siblings resolved, 
     * Mutation.apply() is called and it is where you will do any and all modifications
     *
     * Here we add the NameScorePair we passed to the constructor to the 
     * GameLeaderboard object. After this our modified data will be stored back
     * to Riak
     */
    public GameLeaderboard apply(GameLeaderboard original)
    {   
        original.addScore(nsp);
        return original;
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
    /*
     * The @RiakKey annotation allows the StoreObject to extract the key you wish to use
     * from your POJO. If you're using the default JSONConverter, this is excluded
     * from serialization
     */
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

<a name="iriakobject"></a>
## Storing previously fetched and modified data

Prior to the 1.0.6 release of the Java client if you wanted to do a read/modify/write cycle outside of the store operation, you needed to deal with IRiakObjects directly and still could not avoid an unnecessary fetch during the store operation. As mentioned in [[fetching data from Riak|Fetching-Data-from-Riak#wiki-advanced]]  as of the 1.0.6 client release this problem has been eliminated. 

The `StoreObject` now has a method `withoutFetch()` which can be called prior to `execute()`. This eliminates the fetch (and its associated conflict resolution) during the store operation. 

If you're using your own POJO you must include a `byte[]` or `VClock` field annotated with the `@RiakVClock` annotation. This preserves the vector clock in your POJO and is used during the subsequent store operation. 

The code below shows how our previous example from above would be done to fit this pattern. 

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

        /* 
         * Now that we have a leaderboard in Riak, lets modify it! 
         * This simulates a new name/score pair coming in, and we're going
         * to fetch the existing leaderboard, modify it, then store it back.
         * We know our sample data only has scores to 100, so using 1000 ensures
         * we'll modify the object
         */
        gl = b.fetch("SuperCoolGame", GameLeaderboard.class)
                 .withResolver(new GameLeaderboardResolver())
                 .execute();

        // Output the results!
        for ( NameScorePair n : gl.getScoreList())
        {
            System.out.println(n.getName() + " " + n.getScore());
        }
        System.out.println();

        NameScorePair nsp = new NameScorePair("John", 1000);
 
        gl.addScore(nsp);
        
        /* 
         * Using the withoutFetch() method means our data is going to be stored in Riak
         * using the vector clock from the earlier fetch. No fetch/resolve/mutate is performed. 
         */
        b.store(gl)
            .withoutFetch()
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
     * In this example, the logic is pretty straightforard. in our GameLeaderboard
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

            GameLeaderboard resolvedLeaderboard = i.next();
                        
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
    /*
     * The @RiakKey annotation allows the StoreObject to extract the key you wish to use
     * from your POJO. If you're using the default JSONConverter, this is excluded
     * from serialization
     */
    @RiakKey private String gameName;


    /* The @RiakVClock annotation allows the fetch to store the vector clock in your POJO
     * which is then used by the store operation
     */
    @RiakVClock private byte[] vClock;

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
