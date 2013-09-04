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



Code:

1.9.3p385 :001 > require 'riak'
 => true
1.9.3p385 :002 > client = Riak::Client.new
 => #<Riak::Client [#<Node 127.0.0.1:8098:8087>]>
1.9.3p385 :003 > ls
NameError: undefined local variable or method `ls' for main:Object
	from (irb):3
	from /Users/alex/.rvm/rubies/ruby-1.9.3-p385/bin/irb:16:in `<main>'
1.9.3p385 :004 > client = Riak::Client.new(:protocol=>"pbc")
 => #<Riak::Client [#<Node 127.0.0.1:8098:8087>]>
1.9.3p385 :005 > bucket = client.bucket("users")
 => #<Riak::Bucket {users}>
1.9.3p385 :006 > alex = {:name=>"Alex Moore", :hobbies=>["Skiing", "Coding"], :occupation=>"Programming"}
 => {:name=>"Alex Moore", :hobbies=>["Skiing", "Coding"], :occupation=>"Programming"}
1.9.3p385 :007 > alex[:hobbies]
 => ["Skiing", "Coding"]
1.9.3p385 :008 > sean = {:name=>"Sean Cribbs", :hobbies=>["Music", "Cats"], :occupation=>"Engineer"}
 => {:name=>"Sean Cribbs", :hobbies=>["Music", "Cats"], :occupation=>"Engineer"}
1.9.3p385 :009 > robj_alex = bucket.new("alex")
 => #<Riak::RObject {users,alex} [#<Riak::RContent [application/json]:nil>]>
1.9.3p385 :010 > robj_sean = bucket.new("sean")
 => #<Riak::RObject {users,sean} [#<Riak::RContent [application/json]:nil>]>
1.9.3p385 :011 > robj_alex.
Display all 111 possibilities? (y or n)



                                       robj_alex.__id__
robj_alex.__send__                     robj_alex.attempt_conflict_resolution
robj_alex.blank?                       robj_alex.bucket
robj_alex.bucket=                      robj_alex.class
robj_alex.clone                        robj_alex.conflict?
robj_alex.content                      robj_alex.content_type
robj_alex.content_type=                robj_alex.data
robj_alex.data=                        robj_alex.define_singleton_method
robj_alex.delete                       robj_alex.deserialize
robj_alex.display                      robj_alex.dup
robj_alex.enum_for                     robj_alex.eql?
robj_alex.equal?                       robj_alex.escape
robj_alex.etag                         robj_alex.etag=
robj_alex.extend                       robj_alex.fetch
robj_alex.freeze                       robj_alex.frozen?
robj_alex.hash                         robj_alex.i18n_scope
robj_alex.indexes                      robj_alex.indexes=
robj_alex.initialize_clone             robj_alex.initialize_dup
robj_alex.inspect                      robj_alex.instance_eval
robj_alex.instance_exec                robj_alex.instance_of?
robj_alex.instance_variable_defined?   robj_alex.instance_variable_get
robj_alex.instance_variable_set        robj_alex.instance_variables
robj_alex.is_a?                        robj_alex.key
robj_alex.key=                         robj_alex.kind_of?
robj_alex.last_modified                robj_alex.last_modified=
robj_alex.links                        robj_alex.links=
robj_alex.load_from_mapreduce          robj_alex.maybe_escape
robj_alex.maybe_unescape               robj_alex.meta
robj_alex.meta=                        robj_alex.method
robj_alex.methods                      robj_alex.nil?
robj_alex.object_id                    robj_alex.present?
robj_alex.prevent_stale_writes         robj_alex.prevent_stale_writes=
robj_alex.private_methods              robj_alex.protected_methods
robj_alex.psych_to_yaml                robj_alex.psych_y
robj_alex.public_method                robj_alex.public_methods
robj_alex.public_send                  robj_alex.raw_data
robj_alex.raw_data=                    robj_alex.reload
robj_alex.respond_to?                  robj_alex.respond_to_missing?
robj_alex.send                         robj_alex.serialize
robj_alex.siblings                     robj_alex.siblings=
robj_alex.singleton_class              robj_alex.singleton_methods
robj_alex.store                        robj_alex.t
robj_alex.taint                        robj_alex.tainted?
robj_alex.tap                          robj_alex.to_enum
robj_alex.to_json                      robj_alex.to_link
robj_alex.to_param                     robj_alex.to_query
robj_alex.to_s                         robj_alex.to_yaml
robj_alex.to_yaml_properties           robj_alex.trust
robj_alex.unescape                     robj_alex.untaint
robj_alex.untrust                      robj_alex.untrusted?
robj_alex.vclock                       robj_alex.vclock=
robj_alex.vector_clock                 robj_alex.vector_clock=
robj_alex.walk
1.9.3p385 :011 > robj_alex.d
robj_alex.data                     robj_alex.data=
robj_alex.define_singleton_method  robj_alex.delete
robj_alex.deserialize              robj_alex.display
robj_alex.dup
1.9.3p385 :011 > robj_alex.d
robj_alex.data                     robj_alex.data=
robj_alex.define_singleton_method  robj_alex.delete
robj_alex.deserialize              robj_alex.display
robj_alex.dup
1.9.3p385 :011 > robj_alex.data = alex
 => {:name=>"Alex Moore", :hobbies=>["Skiing", "Coding"], :occupation=>"Programming"}
1.9.3p385 :012 > robj_sean.data = sean
 => {:name=>"Sean Cribbs", :hobbies=>["Music", "Cats"], :occupation=>"Engineer"}
1.9.3p385 :013 > robj_alex.store()
 => #<Riak::RObject {users,alex} [#<Riak::RContent [application/json]:{"name"=>"Alex Moore", "hobbies"=>["Skiing", "Coding"], "occupation"=>"Programming"}>]>
1.9.3p385 :014 > robj_sean.store()
 => #<Riak::RObject {users,sean} [#<Riak::RContent [application/json]:{"name"=>"Sean Cribbs", "hobbies"=>["Music", "Cats"], "occupation"=>"Engineer"}>]>
1.9.3p385 :015 > robj_alex.
Display all 111 possibilities? (y or n)
1.9.3p385 :015 > robj_alex.
Display all 111 possibilities? (y or n)



                                       robj_alex.__id__
robj_alex.__send__                     robj_alex.attempt_conflict_resolution
robj_alex.blank?                       robj_alex.bucket
robj_alex.bucket=                      robj_alex.class
robj_alex.clone                        robj_alex.conflict?
robj_alex.content                      robj_alex.content_type
robj_alex.content_type=                robj_alex.data
robj_alex.data=                        robj_alex.define_singleton_method
robj_alex.delete                       robj_alex.deserialize
robj_alex.display                      robj_alex.dup
robj_alex.enum_for                     robj_alex.eql?
robj_alex.equal?                       robj_alex.escape
robj_alex.etag                         robj_alex.etag=
robj_alex.extend                       robj_alex.fetch
robj_alex.freeze                       robj_alex.frozen?
robj_alex.hash                         robj_alex.i18n_scope
robj_alex.indexes                      robj_alex.indexes=
robj_alex.initialize_clone             robj_alex.initialize_dup
robj_alex.inspect                      robj_alex.instance_eval
robj_alex.instance_exec                robj_alex.instance_of?
robj_alex.instance_variable_defined?   robj_alex.instance_variable_get
robj_alex.instance_variable_set        robj_alex.instance_variables
robj_alex.is_a?                        robj_alex.key
robj_alex.key=                         robj_alex.kind_of?
robj_alex.last_modified                robj_alex.last_modified=
robj_alex.links                        robj_alex.links=
robj_alex.load_from_mapreduce          robj_alex.maybe_escape
robj_alex.maybe_unescape               robj_alex.meta
robj_alex.meta=                        robj_alex.method
robj_alex.methods                      robj_alex.nil?
robj_alex.object_id                    robj_alex.present?
robj_alex.prevent_stale_writes         robj_alex.prevent_stale_writes=
robj_alex.private_methods              robj_alex.protected_methods
robj_alex.psych_to_yaml                robj_alex.psych_y
robj_alex.public_method                robj_alex.public_methods
robj_alex.public_send                  robj_alex.raw_data
robj_alex.raw_data=                    robj_alex.reload
robj_alex.respond_to?                  robj_alex.respond_to_missing?
robj_alex.send                         robj_alex.serialize
robj_alex.siblings                     robj_alex.siblings=
robj_alex.singleton_class              robj_alex.singleton_methods
robj_alex.store                        robj_alex.t
robj_alex.taint                        robj_alex.tainted?
robj_alex.tap                          robj_alex.to_enum
robj_alex.to_json                      robj_alex.to_link
robj_alex.to_param                     robj_alex.to_query
robj_alex.to_s                         robj_alex.to_yaml
robj_alex.to_yaml_properties           robj_alex.trust
robj_alex.unescape                     robj_alex.untaint
robj_alex.untrust                      robj_alex.untrusted?
robj_alex.vclock                       robj_alex.vclock=
robj_alex.vector_clock                 robj_alex.vector_clock=
robj_alex.walk
1.9.3p385 :015 > robj_alex.
1.9.3p385 :016 >   l
NoMethodError: undefined method `l' for #<Riak::RObject:0x007ff26ca49e38>
	from (irb):15
	from /Users/alex/.rvm/rubies/ruby-1.9.3-p385/bin/irb:16:in `<main>'
1.9.3p385 :017 > alex.links
NoMethodError: undefined method `links' for #<Hash:0x007ff26ca33e30>
	from (irb):17
	from /Users/alex/.rvm/rubies/ruby-1.9.3-p385/bin/irb:16:in `<main>'
1.9.3p385 :018 > robj_alex.links << robj_sean.to_link("collegue")
 => #<Set: {</riak/users/sean>; riaktag="collegue"}>
1.9.3p385 :019 > robj_alex.store()
 => #<Riak::RObject {users,alex} [#<Riak::RContent [application/json]:{"name"=>"Alex Moore", "hobbies"=>["Skiing", "Coding"], "occupation"=>"Programming"}>]>
1.9.3p385 :020 > linker = client
client
1.9.3p385 :020 > linker = client['users']['alex']
 => #<Riak::RObject {users,alex} [#<Riak::RContent [application/json]:{"name"=>"Alex Moore", "hobbies"=>["Skiing", "Coding"], "occupation"=>"Programming"}>]>
1.9.3p385 :021 > linker.walk(:keep => true)
 => [[#<Riak::RObject {users,sean} [#<Riak::RContent [application/json; charset=UTF-8]:(73 bytes)>]>]]
1.9.3p385 :022 > collegues = linker.walk(:keep => true)
 => [[#<Riak::RObject {users,sean} [#<Riak::RContent [application/json; charset=UTF-8]:(73 bytes)>]>]]
1.9.3p385 :023 > collegues
 => [[#<Riak::RObject {users,sean} [#<Riak::RContent [application/json; charset=UTF-8]:(73 bytes)>]>]]
1.9.3p385 :024 > co
collegues  conf       context
1.9.3p385 :024 > collegues[0]
 => [#<Riak::RObject {users,sean} [#<Riak::RContent [application/json; charset=UTF-8]:(73 bytes)>]>]
1.9.3p385 :025 > collegues.first()
 => [#<Riak::RObject {users,sean} [#<Riak::RContent [application/json; charset=UTF-8]:(73 bytes)>]>]
1.9.3p385 :026 > collegues.first().first()
 => #<Riak::RObject {users,sean} [#<Riak::RContent [application/json; charset=UTF-8]:(73 bytes)>]>
1.9.3p385 :027 > collegues.first().first().data
 => {"name"=>"Sean Cribbs", "hobbies"=>["Music", "Cats"], "occupation"=>"Engineer"}
1.9.3p385 :028 > collegues.flatten
 => [#<Riak::RObject {users,sean} [#<Riak::RContent [application/json; charset=UTF-8]:{"name"=>"Sean Cribbs", "hobbies"=>["Music", "Cats"], "occupation"=>"Engineer"}>]>]
1.9.3p385 :029 > collegues.flatten.first
 => #<Riak::RObject {users,sean} [#<Riak::RContent [application/json; charset=UTF-8]:{"name"=>"Sean Cribbs", "hobbies"=>["Music", "Cats"], "occupation"=>"Engineer"}>]>