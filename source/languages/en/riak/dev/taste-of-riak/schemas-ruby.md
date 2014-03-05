---
title: "Taste of Riak: More Schemas with Ruby"
project: riak
version: 1.3.1+
document: tutorials
toc: true
audience: beginner
keywords: [developers, client, 2i, search, ruby, schema]
---

####Getting Started with the Models
To get started, let's create the models we'll be using.
Since the Ruby Riak Client uses hashes when converting to and from JSON, we will be using the library [Hashie](http://rdoc.info/github/intridea/hashie) to help automatically coerce class properties to and from hashes. You can install this library with `gem install hashie`.

```ruby
class User < Hashie::Dash
	property :user_name
	property :full_name
	property :email
end

class Msg < Hashie::Dash
	property :from
	property :to
	property :created
	property :text
end

class Timeline < Hashie::Dash
	property :owner
	property :type
	property :msgs
end
```


So to use these classes to store data, we will first have to create a user. Then when a user creates a message we will append that message to one or more timelines. If it's a private message, we'll append it to the Recipient's `Inbox` timeline and the User's own `Sent` timeline.  If it's a group message, we'll append it to the Group's timeline, as well as the User's `Sent` timeline.  

Now that we've worked out how we will differentiate data in the system, let's figure out our bucket and key names.

The bucket names are straightforward, we can use `User`, `Msg`, and `Timeline`.  The key names however are a little more tricky.  In past examples we've used sequential integers (much like a typical Sql database), but this presents a problem - we would need a secondary service to hand out these IDs. This service could easily be a future bottleneck in the system, so let's use a natural key.  

For the `User`, we can be certain that we will want each username to be unique, so let's use the `username` as the key.  For the `Msg` bucket, lets use a combination of the username and the posting datetime in an [ISO 8601 Long](http://en.wikipedia.org/wiki/ISO_8601) Format. This combination gives us the pattern `<username>_<datetime>`, for something like `joeuser_2014-03-05T23:20:28Z`.

Now for `Timeline`s, we need to differentiate between `Inbox` and `Sent` timelines, so we can simply add that type into the keyname.  We will also want to partition each collection object into some time period, that way the object doesn't grow too big******.  So for Timelines, let's use the pattern `<username>_<type>_<date>` for users, and `<groupname>_inbox_<date>`, which will look like `joeuser_sent_2014-03-05Z` or `engineering_group_inbox_2014-03-05Z`.

******Riak prefers objects under 1-2MB, objects bigger than that can hurt performance, especially if you start creating lots of siblings (which we will cover in the next chapter).

Now that we've figured out our schema, let's write some repositories to enforce them.

```ruby

```

As you can see, the repository pattern helps us with a few things:
 - It helps us to see if an object exists before creating a new one
 - It keeps our buckets and keynames consistent
 - It provides us with a consistent pattern to work with. 

So to recap, in this chapter we learned:
 - How to choose bucket names
 - How to choose natural keys based on how we want to partition our data.

In the next chapter, we will learn about siblings, how to deal with them, and some other edge cases to check for while working with Riak.
