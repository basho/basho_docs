---
title: "Taste of Riak: Object Modeling"
project: riak
version: 1.4.8+
document: tutorials
toc: true
audience: beginner
keywords: [developers, client, modeling]
---

As a developer, you may be nonplussed to see that Secondary Indexes (2i) don't work with the Bitcask backend. You may also be wondering how to work with "sets" of data.

In this chapter, we'll cover how to deal with both, and in the next chapter we'll introduce some important distributed systems concepts such as [[siblings|Vector Clocks#Siblings]], [[eventual consistency]], and [[sibling resolution|Vector Clocks#Siblings]] that you'll need to know to become a pro Riak developer. 

For the remainder of the tutorials, we will be implementing pieces of an internal messaging app. This app, codenamed "Msgy" (we couldn't afford all the vowels) will provide "a way to incentivize long-tail watercooler networking for employees." In short, it will allow us to post little snippets of text (Msgs) to each other at work without all the hassle of email, while we block traditional social media. 

At the data level, we only have a few objects to work with:

 - `User` --- A user object, containing a `user_name`, a `full_name`, and an `email` address.
 - `Msg` --- A message that a user has sent, containing `text`, `sender` and `recipient` addresses, and a `created` date.
 - `Timeline` --- A list of `Msg`s, containing an `owner` and a `msg_type`.  The type can be one of two things:
   - `Inbox`: A user's inbox of personal messages, or a group's public Msg list.
   - `Sent`: A list of a user's sent messages.
 
 The `Timeline` objects could easily be implemented with a 2i query across the `Msg` bucket, but in this chapter we'll show how to do it using only key/value operations. 	

###Choose Your Programming Language

Please select the language you'd like to proceed with.

<ul class="planguages">
<li><a href="/dev/taste-of-riak/object-modeling-java/"><img src="/images/plangs/java.jpg" alt="Java"></a></li>
<li><a href="/dev/taste-of-riak/object-modeling-erlang/"><img src="/images/plangs/erlang.jpg" alt="Erlang"></a></li>
<li><a href="/dev/taste-of-riak/object-modeling-ruby/"><img src="/images/plangs/ruby.jpg" alt="Ruby"></a></li>
<li><a href="/dev/taste-of-riak/object-modeling-python/"><img src="/images/plangs/python.png" alt="Python"></a></li>
</ul>




