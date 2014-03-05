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

Now that we've worked out how we will differntiate data in the system, let's figure out our bucket and key names.


