---
title: Riak Java Client Custom Converter
project: java
version: 0.10.0+
document: cookbook
toc: false
audience: beginner
keywords: [java, client]
---

### Why would I need a custom Converter?

When storing data in or retrieving data from Riak, a Converter is used to serialize the Java object passed in. When no Converter is specified and you're using your own Domain Object (POJO) the default JSONConverter is used. Behind the scenes this takes your object, uses the Jackson JSON library to serialize it, and stores the resulting JSON text in Riak. This can be advantageous if, for example, you are planning on accessing this data from other (non-java) applications. 

If you wanted to use a different serialization library, a custom Converter would be required. Let's look at how you would write a Converter to use the popular Kryo serialization library to serialize and deserialize your objects.

To build and run these examples you'll need to include both the kryo and riak-client libraries. The easiest way to accomplish this is via Maven:

```xml
<dependencies>
  <dependency>
    <groupId>com.basho.riak</groupId>
    <artifactId>riak-client</artifactId>
    <version>1.0.4</version>
  </dependency>
  <dependency>
    <groupId>com.googlecode</groupId>
    <artifactId>kryo</artifactId>
    <version>1.04</version>
  </dependency>
</dependencies>
```

## A Basic Converter

Note this example is _very_ basic. We're just covering the use of Kryo in a Converter. In this example any links, user metadata, indexes, etc. would be lost in translation. We'll cover that in the next example.

### App.java

```java
import com.basho.riak.client.IRiakClient;
import com.basho.riak.client.RiakException;
import com.basho.riak.client.RiakFactory;
import com.basho.riak.client.bucket.Bucket;

public class App 
{
    public static void main( String[] args ) throws RiakException
    {
        
        IRiakClient client = RiakFactory.httpClient();
        
        Person p = new Person("Brian Roach", "1111 Basho Drive", "555-1212");
        
        Bucket bucket = client.fetchBucket("PersonBucket").execute();
        
        /* One shortcoming of the current Converter<T> Interface is that the bucket name is not passed
         * in automatically. We will be improving this in the next release (1.1.0)
         */
        bucket.store(p).withConverter(new KryoPersonConverter("PersonBucket")).execute();
        
        p = new Person();
        p.setName("Brian Roach");
        p = bucket.fetch(p).withConverter(new KryoPersonConverter("PersonBucket")).execute();
        
        System.out.println(p.getName());
        System.out.println(p.getAddress());
        System.out.println(p.getPhone());
        
        client.shutdown();
    }
}
```

### Person.java

```java
import com.basho.riak.client.convert.RiakKey;

public class Person
{
    
    // The @RiakKey annotation marks the field you want to use as the Key in Riak
    @RiakKey private String name;
    
    private String address;
    private String phone;
    
    public Person() {}
    
    public Person(String name, String address, String phone)
    {
        this.name = name;
        this.address = address;
        this.phone = phone;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    public String getAddress()
    {
        return address;
    }

    public void setAddress(String address)
    {
        this.address = address;
    }

    public String getPhone()
    {
        return phone;
    }

    public void setPhone(String phone)
    {
        this.phone = phone;
    }
 
}
```

### KryoPersonConverter.java

```java
import com.basho.riak.client.http.util.Constants;
import com.basho.riak.client.builders.RiakObjectBuilder;
import com.esotericsoftware.kryo.ObjectBuffer;
import com.esotericsoftware.kryo.Kryo;
import com.basho.riak.client.IRiakObject;
import com.basho.riak.client.cap.VClock;
import com.basho.riak.client.convert.ConversionException;
import com.basho.riak.client.convert.Converter;

import com.basho.riak.client.convert.NoKeySpecifedException;

import static com.basho.riak.client.convert.KeyUtil.getKey;

public class KryoPersonConverter implements Converter<Person>
{

    private String bucket;
    
    /* One shortcoming of the current Converter<T> Interface is that the bucket name is not passed
     * in automatically. We will be improving this in the next release (1.1.0)
     */
    public KryoPersonConverter(String bucket)
    {
        this.bucket = bucket;
    }
    
    public IRiakObject fromDomain(Person domainObject, VClock vclock) throws ConversionException
    {
        String key = getKey(domainObject);
        
        if (key == null)
        {
            throw new NoKeySpecifedException(domainObject);
        }
        
        Kryo kryo = new Kryo();
        kryo.register(Person.class);
        
        ObjectBuffer ob = new ObjectBuffer(kryo);
        byte[] value = ob.writeObject(domainObject);
        
        return RiakObjectBuilder.newBuilder(bucket, key)
            .withValue(value)
            .withVClock(vclock)
            .withContentType(Constants.CTYPE_OCTET_STREAM)
            .build();
    }

    public Person toDomain(IRiakObject riakObject) throws ConversionException
    {
        if (riakObject == null)
            return null;
        
        Kryo kryo = new Kryo();
        kryo.register(Person.class);
        ObjectBuffer ob = new ObjectBuffer(kryo);
        
        return ob.readObject(riakObject.getValue(), Person.class);
    }
    
}
```
## Converter with secondary indexes, links, and user metadata

This Converter preserves all the Riak data when reading/writing. This example also demonstrates how to use [[Secondary Indexes]] with your serialized objects.

### App.java

```java
import com.basho.riak.client.IRiakClient;
import com.basho.riak.client.RiakException;
import com.basho.riak.client.RiakFactory;
import com.basho.riak.client.bucket.Bucket;
import com.basho.riak.client.query.indexes.BinIndex;
import java.util.List;

public class App 
{
    public static void main( String[] args ) throws RiakException
    {
        
        IRiakClient client = RiakFactory.httpClient();
        
        Person p = new Person("Brian", "Roach", "1111 Basho Drive", "555-1212", "engineer");
        Person p2 = new Person("Joe", "Smith", "1111 Basho Drive", "555-1211", "engineer");
        
        Bucket bucket = client.fetchBucket("PersonBucket").execute();
        
        bucket.store(p).withConverter(new KryoPersonConverter("PersonBucket")).execute();
        bucket.store(p2).withConverter(new KryoPersonConverter("PersonBucket")).execute();
    
        // Get the list of keys using the index name we declared in our Person Object
        List<String> engineers = bucket.fetchIndex(BinIndex.named("job_title")).withValue("engineer").execute();
    
  for (String s : engineers)
  {
            p = new Person();
            p.setLastName(s);
            p = bucket.fetch(p).withConverter(new KryoPersonConverter("PersonBucket")).execute();
            System.out.println(p.getFullName());
            System.out.println(p.getAddress());
            System.out.println(p.getPhone());
            System.out.println(p.getJobTitle());
            System.out.println();
        }
        client.shutdown();
    }
}
```

###Person.java

```java
import com.basho.riak.client.RiakLink;
import com.basho.riak.client.convert.RiakIndex;
import com.basho.riak.client.convert.RiakKey;
import com.basho.riak.client.convert.RiakLinks;
import com.basho.riak.client.convert.RiakUsermeta;
import com.esotericsoftware.kryo.Optional;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Map;

public class Person
{
    
    @RiakKey private String lastName;

    // Marked transient so kryo doesn't serialize them
    // The KryoPersonConverter will inject these from Riak
    @RiakIndex(name = "full_name") transient private String fullName;
    @RiakIndex(name = "job_title") transient private String jobTitle;
    @RiakLinks transient private Collection<RiakLink> links;
    @RiakUsermeta transient private Map<String, String> usermetaData;
    
    private String firstName;
    private String address;
    private String phone;
    
    public Person() {}
    
    public Person(String firstName, String lastName, String address, String phone, String title)
    {
        this.firstName = firstName;
        this.lastName = lastName;
        this.address = address;
        this.phone = phone;
        this.fullName = firstName + " " + lastName;
        this.jobTitle = title;
    }

    public String getFirstName()
    {
        return firstName;
    }

    public void setFirstName(String firstName)
    {
        this.firstName = firstName;
        this.setFullName(firstName + " " + lastName);
    }

    public String getLastName()
    {
        return lastName;
    }
    
    public void setLastName(String lastName)
    {
        this.lastName = lastName;
        this.setFullName(firstName + " " + lastName);
    }
    
    public String getAddress()
    {
        return address;
    }

    public void setAddress(String address)
    {
        this.address = address;
    }

    public String getPhone()
    {
        return phone;
    }

    public void setPhone(String phone)
    {
        this.phone = phone;
    }

    public String getJobTitle()
    {
        return jobTitle;
    }

    public void setJobTitle(String jobTitle)
    {
        this.jobTitle = jobTitle;
    }

    public String getFullName()
    {
        return fullName;
    }

    public void setFullName(String fullName)
    {
        this.fullName = fullName;
    }
}
```

###KryoPersonConverter.java

```java
import com.basho.riak.client.RiakLink;
import java.util.Collection;
import com.basho.riak.client.query.indexes.RiakIndexes;
import java.util.Map;
import com.basho.riak.client.convert.RiakLinksConverter;
import com.basho.riak.client.convert.RiakIndexConverter;
import com.basho.riak.client.convert.UsermetaConverter;
import com.basho.riak.client.http.util.Constants;
import com.basho.riak.client.builders.RiakObjectBuilder;
import com.esotericsoftware.kryo.ObjectBuffer;
import com.esotericsoftware.kryo.Kryo;
import com.basho.riak.client.IRiakObject;
import com.basho.riak.client.cap.VClock;
import com.basho.riak.client.convert.ConversionException;
import com.basho.riak.client.convert.Converter;

import com.basho.riak.client.convert.NoKeySpecifedException;

import static com.basho.riak.client.convert.KeyUtil.getKey;

public class KryoPersonConverter implements Converter<Person>
{

    private String bucket;
    private final UsermetaConverter<Person> usermetaConverter;
    private final RiakIndexConverter<Person> riakIndexConverter;
    private final RiakLinksConverter<Person> riakLinksConverter;
    
    public KryoPersonConverter(String bucket)
    {
        this.bucket = bucket;
        this.usermetaConverter = new UsermetaConverter<Person>();
        this.riakIndexConverter = new RiakIndexConverter<Person>();
        this.riakLinksConverter = new RiakLinksConverter<Person>();
    }
    
    
    public IRiakObject fromDomain(Person domainObject, VClock vclock) throws ConversionException
    {
        //throw new UnsupportedOperationException("Not supported yet.");
        
        String key = getKey(domainObject);
        
        if (key == null)
        {
            throw new NoKeySpecifedException(domainObject);
        }
        
        Kryo kryo = new Kryo();
        kryo.register(Person.class);
        
        ObjectBuffer ob = new ObjectBuffer(kryo);
        byte[] value = ob.writeObject(domainObject);
        
        Map<String, String> usermetaData = usermetaConverter.getUsermetaData(domainObject);
        RiakIndexes indexes = riakIndexConverter.getIndexes(domainObject);
        Collection<RiakLink> links = riakLinksConverter.getLinks(domainObject);
        
        return RiakObjectBuilder.newBuilder(bucket, key)
            .withValue(value)
            .withVClock(vclock)
            .withUsermeta(usermetaData)
            .withIndexes(indexes)
            .withLinks(links)
            .withContentType(Constants.CTYPE_OCTET_STREAM)
            .build();
        
        
    }

    public Person toDomain(IRiakObject riakObject) throws ConversionException
    {
        
        if (riakObject == null)
            return null;
        
        Kryo kryo = new Kryo();
        kryo.register(Person.class);
        ObjectBuffer ob = new ObjectBuffer(kryo);
        
        Person domainObject = ob.readObject(riakObject.getValue(), Person.class);
        
        usermetaConverter.populateUsermeta(riakObject.getMeta(), domainObject);
        riakIndexConverter.populateIndexes(new RiakIndexes(riakObject.allBinIndexes(), riakObject.allIntIndexes()),
                                               domainObject);
        riakLinksConverter.populateLinks(riakObject.getLinks(), domainObject);
        
        return domainObject;
        
    }
}
```
