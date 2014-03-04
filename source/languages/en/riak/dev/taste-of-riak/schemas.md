---
title: "Taste of Riak: More Schemas"
project: riak
version: 1.3.1+
document: tutorials
toc: true
audience: beginner
keywords: [developers, client, schema, search]
---

As a developer you may be nonplussed to see that Secondary Indexes(2i) don't work with the Bitcask backend. You may also be wondering how to work with "sets" of data.
In this chapter we'll cover how to deal with both, and introduce some important distributed systems concepts such as siblings, eventual consistency, and sibling resolution that you'll need to know to become a pro developer. 

For the remainder of the tutorials, we will be implementing pieces of an internal messaging app.  
This app, codenamed "Msgy" (we couldn't afford all the vowels) will be "a way to incentivize long-tail watercooler networking for employees". In short it will allow us to post little snippets of text (Msgs) to each other at work without all the hassle of email, while we block traditional social media. 

At the data level, we only have a few objects to work with:

 - **User**: A user object. Contains a username, full name, and a list of "friends" whose posts they'd like to see.
 - **Msg**: A blob of text that a user wrote.
 - **User-Timeline**: The list of Msgs that a user has posted.
 
 The `User-Timeline` object could easily be implemented with a 2i query across the `Msg` bucket, but in this chapter we'll show how to do it with just Key-Value operations. 	

###Choose Your Programming Language
Please select the language you'd like to proceed with.

<ul class="planguages">
<li><a href="/dev/taste-of-riak/schemas-java/"><img src="/images/plangs/java.jpg" alt="Java"></a></li>
<li><a href="/dev/taste-of-riak/schemas-erlang/"><img src="/images/plangs/erlang.jpg" alt="Erlang"></a></li>
<li><a href="/dev/taste-of-riak/schemas-ruby/"><img src="/images/plangs/ruby.jpg" alt="Ruby"></a></li>
<li><a href="/dev/taste-of-riak/schemas-python/"><img src="/images/plangs/python.png" alt="Python"></a></li>
</ul>




