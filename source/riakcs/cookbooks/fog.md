---
title: Fog on RiakCS
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: beginner
keywords: [developer]
---

Fog is a general cloud services library written in Ruby. It is built to
support as many providers as possible, from most AWS services, to Rackspace
and Linode, and this includes an extension for RiakCS.

You must install the fog gem with riak-cs code. Currently it's a [branch on
github](https://github.com/basho/fog/tree/riak_cs).

```bash
$ git clone -b riak-cs git@github.com:basho/fog.git
$ gem build fog.gemspec
$ gem install fog-1.3.1.gem
```

Or in Bundler:

```
gem "fog", :git => 'git://github.com/basho/fog.git', :branch => 'rails-cs'
```

We are working to have this merged into the official `fog` gem.

## User Management

The first activity is to create a new user. But before we can do that, you must
create connections to your RiakCS server to handle communication to different
services.

### Setup

First, create a new instance of the provisioning object (capitalized constants
are to be set by you).

```ruby
client = Fog::RiakCS::Provisioning.new(
  :riakcs_access_key_id     => RIAK_CS_ADMIN_KEY,
  :riakcs_secret_access_key => RIAK_CS_ADMIN_SECRET,
  :host                     => RIAK_CS_HOST,
  :port                     => RIAK_CS_PORT
)
```

### Create user

The following command creates a user, given an email or name. It will return a
response object---or raise an error if the operation fails. The response `body`
will contain a JSON document containing the user informations, while the
`key_id` is required for further operations on the user.

```ruby
response = client.create_user(email, name)
```

### List Users

You can list the users in this cluster, optionally filtering by the user's
status. It's body is an array of hashes representing each matching user.

```ruby
users = client.list_users(:status => 'enabled')
```

### Get User

With the user's `key_id` (returned above, a `riakcs_access_key_id`). `get_user`
returns a response whose `body` is a JSON document describing the user, or
raises and error if the user doesn't exist.

```ruby
user = client.get_user(key_id)
user.body == {"key_secret"=>"XXX", "display_name"=>"dizzy", "email"=>"dizzy@basho.com", "status"=>"enabled", "name"=>"Eric Redmond", "key_id"=>"YYY", "id"=>"ZZZ"}
```

### Manage User

You can also enable or disable users access using the following commands.

```ruby
client.enable_user(key_id)
client.disable_user(key_id)
```

You can also revoke their current credentials and grant new credentials.
`regrant_secret` returns a JSON document with the users refreshed credentials.

```ruby
client.regrant_secret(key_id)
```

## Usage Retrieval

To get information about RiakCS requests, 

### Setup

First, create a new instance of the Usage object.

```ruby
client = Fog::RiakCS::Usage.new(
  :riakcs_access_key_id     => RIAK_CS_ADMIN_KEY,
  :riakcs_secret_access_key => RIAK_CS_ADMIN_SECRET,
  :host                     => RIAK_CS_HOST,
  :port                     => RIAK_CS_PORT
)
```

usage = Fog::RiakCS::Usage.new(
  :riakcs_access_key_id     => 'XXXPRQ_MVWUC7QZ5OBHF',
  :riakcs_secret_access_key => 'Hhti-b9YFBjYkFgFFq5PbrOs2pFgBIhu3LF6Aw==',
  :host                     => 'data.riakcs.net'
)

*Note: You may use regular (non-admin) credentials for usage retrieval, if you
are accessing your own usage.*

### Get usage

The `get_usage` method returns usage information for the `requested_key_id`.
You can choose which type of usage you want via the `:types` attribute:
`:access` or `:storage` (defaults to both). You may also specify a
`:start_time` and an `:end_time` (this defaults to the previous 24 hour
window). You'll receive a response object, whose `body` is a nested set of
hashes containing usage data broken down by `type`, and further by `node`.

```ruby
response = client.get_usage(requested_key_id,
                 :types      => [:access, :storage],
                 :start_time => start_time,
                 :end_time   => end_time)
```

If user access is denied, it will return a `Excon::Errors::Forbidden`
error.
