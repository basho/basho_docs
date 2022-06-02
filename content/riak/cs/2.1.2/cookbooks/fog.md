---
title: "Fog on Riak CS"
description: ""
menu:
  riak_cs-2.1.2:
    name: "Fog on Riak CS"
    identifier: "cookbook_fog"
    weight: 102
    parent: "api_s3"
project: "riak_cs"
project_version: "2.1.2"
aliases:
  - /riakcs/2.1.2/cookbooks/fog/
  - /riak/cs/latest/cookbooks/fog/
---

Fog is a general cloud services library written in Ruby. It is built to
support as many cloud providers as possible, ranging from most AWS
services to Rackspace, Linode, Joyent, and beyond, and this includes an
extension for Riak CS.

You can install it via [RubyGems](http://rubygems.org/):

```bash
gem install fog
```

Or using [Bundler](http://gembundler.com/):

```ruby
gem "fog", "~> 1.10.1"
```

## User Management

The first thing that needs to be done when using Fog is creating a new
user. Before you can do that, however, you must create connections to
your Riak CS server to handle communication to different services.

### Setup

First, create a new instance of the provisioning object (capitalized
constants are to be set by you).

```ruby
client = Fog::RiakCS::Provisioning.new(
  :riakcs_access_key_id     => RIAK_CS_ADMIN_KEY,
  :riakcs_secret_access_key => RIAK_CS_ADMIN_SECRET,
  :host                     => RIAK_CS_HOST,
  :port                     => RIAK_CS_PORT
)
```

### Create User

The following command creates a user, given an email or name. This will
either return a response object or raise an error if the operation
fails. The response body will contain a JSON document containing the
user's information, while the `key_id` is required for further
operations on the user.

```ruby
response = client.create_user(email, name)
```

### List Users

You can list the users in the current Riak CS cluster, optionally
filtering by the user's status. The response body is an array of hashes
representing each matching user.

```ruby
users = client.list_users(:status => 'enabled')
```

### Get User

With the user's `key_id` (`riakcs_access_key_id`), `get_user` either
returns a JSON document describing the user or raises and error if the
user doesn't exist.

```ruby
user = client.get_user(key_id)
user.body
# {"key_secret"=>"XXX", "display_name"=>"dizzy", "email"=>"dizzy@basho.com", "status"=>"enabled", "name"=>"Eric Redmond", "key_id"=>"YYY", "id"=>"ZZZ"}
```

### Manage User

You can enable or disable users' access with the following commands.

```ruby
client.enable_user(key_id)
client.disable_user(key_id)
```

You can also revoke users' current credentials and grant new
credentials. The `regrant_secret` function returns a JSON document with
the users' refreshed credentials.

```ruby
client.regrant_secret(key_id)
```

## Usage Retrieval

Fetches information about Riak CS requests.

### Setup

First, create a new instance of the Usage object.

```ruby
usage = Fog::RiakCS::Usage.new(
  :riakcs_access_key_id     => RIAK_CS_ADMIN_KEY,
  :riakcs_secret_access_key => RIAK_CS_ADMIN_SECRET,
  :host                     => RIAK_CS_HOST,
  :port                     => RIAK_CS_PORT
)
```

The example below is targeted at [riakcs.net](https://www.riakcs.net):

```ruby
usage = Fog::RiakCS::Usage.new(
  :riakcs_access_key_id     => 'XXXPRQ_MVWUC7QZ5OBHF',
  :riakcs_secret_access_key => 'Hhti-b9YFBjYkFgFFq5PbrOs2pFgBIhu3LF6Aw==',
  :host                     => 'data.riakcs.net',
  :port                     => 8080
)
```

**Note**: You may use regular (non-admin) credentials for usage
retrieval if you are accessing your own usage.

### Get usage

The `get_usage` method returns usage information for the
`requested_key_id`. You can choose which type of usage you want via the
`:types` attribute: `:access` or `:storage` (defaults to both). You may
also specify a `:start_time` and an `:end_time` (this defaults to the
previous 24-hour window). You'll receive a response object, whose `body`
is a nested set of hashes containing usage data broken down by `type`,
and further by `node`.

```ruby
response = client.get_usage(requested_key_id,
                 :types      => [:access, :storage],
                 :start_time => start_time,
                 :end_time   => end_time)
```

If user access is denied, it will return a `Excon::Errors::Forbidden`
error.
