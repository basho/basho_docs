---
title: "Taste of Riak: Querying with Ruby"
project: riak
version: 1.3.1+
document: tutorials
toc: true
audience: beginner
keywords: [developers, client, 2i, search, linkwalking, ruby]
---

####A Quick Note on Querying and Schemas
_Schemas_? Yes I said that correctly, S-C-H-E-M-A-S. It's not a dirty word.  
Even with a Key/Value store, you will still have a logical database schema of how all the data relates to one another. This can be as simple as using the same key across multiple buckets for different types of data, to having fields in your data that are related by name.  These querying methods will introduce you to some ways of laying out your data in Riak, along with how to query it back.

###Same Keys - Different Buckets

The simplest way to split up data would be to use the same identity key across different buckets. While denomalizing your data eases most of the burden of splitting up your data into multiple tables, sometimes it's best to still keep some things separate.  A good example of this would be a generic "User Account" object, and the user's avatar picture.  

<code> Account / picture in code. </code>

While we could store them together, it's a good idea to split them up because the account data is stored as a structured json document and the picture is stored as a byte array. If we used the same key for both buckets though, it would make fetching both very easy, since we only need to know one key.  

<code> Lookup both values with riak. </code>

While this pattern is very easy and extremely fast query-wise, it is up to the application to know about these intrinsic relationships.  


###Links

[Links](http://docs.basho.com/riak/1.2.0/references/appendices/concepts/Links/) are literally a way to "link" one object to another.  They are simple one-way named relationships. 
Let's take our previous example and make a link from our account to it's avatar.

<code> link from account to avatar </code>

Now if we wanted to lookup the avatar object from our account, we simply just look in the account's `links` collection for the correct entry.

<code> lookup link </code>

You can also lookup multiple links at once.  

<code> lookup all the links </code>

While links are great for modeling simple relationships, they do increase the object's size. Because of this (and combinatorial explosions) we recommend limiting the number of links on an object to a few dozen.  


###Secondary Indexes

If you're coming from a SQL world, Secondary Indexes (2i) are a lot like SQL indexes.  They are a way to lookup objects based on a secondary key, without scanning through the whole dataset.  This makes it very easy to find groups of related data by values, or even ranges of values.  To properly show this off, we will now add some more data to our application, and add some secondary index entries at the same time.

<code> Add  </code>

As you may have noticed, ordinary Key/Value data is opaque to 2i, so we have to add entries to the indices at the application level. 
Now let's find all the bugs that have been assigned to the user Johnny Appleseed.  In this example we will look up his tickets by searching the `assigned_to` index for Johnny's UUID key.

<code> Lookup by uuid </code>

Now, let's say that Johnny's manager wants to know how many tickets were opened during May 2013.  In this case we can exploit 2i's range queries.  Let's search for created_date index entries between "20130501" and "20130531".

<code> </code>

Boom, easy-peasy.  This was possible because we stored our created dates as an integer - allowing us to use the range feature of 2i.  

###Search

Riak Search 



<h6>Code Scratchpad - Ignore Me</h6>
<code>

require 'riak'

client = Riak::Client.new(:protocol=>"pbc")

bucket = client.bucket("users")

alex = {:name=>"Alex Moore", :hobbies=>["Skiing", "Coding"], :occupation=>"Programming"}

sean = {:name=>"Sean Cribbs", :hobbies=>["Music", "Cats"], :occupation=>"Engineer"}

robj_alex = bucket.new("alex")

robj_sean = bucket.new("sean")

robj_alex.data = alex

robj_sean.data = sean

robj_alex.store()

robj_sean.store()

robj_alex.links << robj_sean.to_link("collegue")

robj_alex.store()

linker = client['users']['alex']
 
collegues = linker.walk(:keep => true)

collegues.flatten.first

</code>