---
title: Using Bucket Types
project: riak
version: 2.0.0+
document: tutorial
audience: intermediate
keywords: [developers, buckets]
---

Bucket types allow groups of buckets to share configuration details and for Riak users to manage bucket properties in a way that is often more efficient than an ad hoc approach.

## How Bucket Types Work

The ad hoc approach to bucket configuration involves setting bucket properties for a specific bucket either through [[HTTP|HTTP Set Bucket Properties]] or [[Protocol Buffers|PBC Set Bucket Properties]]. Using bucket types also means dealing with bucket properties, but in a more abstracted way. 

It is important to note that buckets are not assigned types in the same way that they are configured using `props`. You cannot simply take a bucket `test_bucket` and assign it a type the way that you would, say, set `allow_mult` to `false`. In other words, there is no `type` parameter contained within `props`. Instead, bucket types are applied to buckets _on the basis of how those buckets are queried_.

Queries involving bucket types take the following form:

```curl
GET/PUT/DELETE /types/<type>/buckets/<type>/keys/<key>
```

If you query a bucket without the `/types/<type>` prefix in the bucket's locator, 

Thus, if you have created the bucket type `larger_n_val` (with the `n_val` set to 5) and would like that type to be applied to the bucket `sensitive_user_data`, you would need to run operations on that bucket in accordance with the format above. Here is an example HTTP query:

```curl
curl -XPUT \
  -H "Content-Type: application/json" \
  -d "{ ... user data ... }" \
  http://localhost:8098/types/larger_n_val/buckets/sensitive_user_data/keys/user19735
```

In this example, the bucket `sensitive_user_data` bears the configuration established by the `larger_n_val` bucket type on the basis of the URL employed.

The advantage of requiring that bucket types be used in this way is that types can be dynamically applied to buckets that do not yet exist. As an example, let's say that the bucket `current_memes` exists and bears the type `larger_n_val`.

Now, let's say that our application needs to create a new bucket called `old_memes` to store memes that have gone woefully out of fashion, but that bucket also needs to bear the type `larger_n_val`. The following request made by the application will ensure that the `old_memes` bucket indeed bears the type `larger_n_val`:

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "all your base are belong to us" \
  http://localhost:8098/types/larger_n_val/buckets/old_memes/keys/all_your_base
```

A non-dynamic way of setting the bucket's properties would be to set the bucket's properties in advance:

```curl
curl -XPUT \
  -H "Content-Type: application/json" \
  -d '{"props":{"n_val":5}}' \
  http://localhost:8098/buckets/old_memes/props
```

This way of doing things works just fine for many use cases. If, however, you do not know in advanced which buckets will be required by your application but you still need to apply types to them _upon creation_, then you should strongly consider assigning bucket types dynamically.

## The `riak-admin bucket-type` Command

Bucket types can be created, updated, activated, and more through the `riak-admin bucket-type` interface.

Below is a full list of available commands:

Command | Action | Form |
:-------|:-------|:-----|
`list` | List all currently available bucket types and their activation status | `list` |
`status` | Display the status and properties of a specific bucket type | `status <type>` |
`activate` | Activate a bucket type | `activate <type>` |
`create` | Create or modify a bucket type before activation | `create <type> <json>` |
`update` | Update a bucket type after activation | `update <type> <json>` |

### Listing Bucket Types

You can list currently available bucket types using the `list` command:

```bash
riak-admin bucket-type list
```

This will return a simple list of types along with their current status (either `active` or `not active`). Here is an example console output:

```bash
type1 (active)
type2 (not active)
type3 (active)
```

### Checking a Type's Status

You can check on the status of a bucket type using the `status <type>` command:

```bash
riak-admin bucket-type status my_bucket_type
```

The console will output two things if the type exists:

1. Whether or not the type is active
2. The bucket properties associated with the type.

If you check the status of a currently active type called `my_bucket_type` that simply bears a default bucket configuration, the output will be as follows:

```bash
my_bucket_type is active

active: true
allow_mult: true

...

w: quorum
young_vclock:20
```

### Activating a Bucket Type

Simply run the `activate` command to activate a type:

```bash
riak-admin bucket-type activate my_bucket_type
```

### Creating a Bucket Type

Creating new bucket types simply involves using the `create <type> <json>`, where `<type>` is the name of the type and `<json>` is a JSON object of the following form:

```json
{
  "props": {
    "prop1": "val1",
    "prop2": "val2"
  }
}
```

If you'd like to create a bucket type that simply extends Riak's defaults, for example, pass an empty object to the `props` parameter:

```bash
riak-admin bucket-type create default '{"props":{}}'
```

A more fully-fleshed-out example can be found [[above|Using Bucket Types#Bucket-Properties-Example]].

## Bucket Properties Example

Below are the default properties for Riak buckets:

```json
{
  "props": {
    "allow_mult": true,
    "basic_quorum": false,
    "big_vclock": 50,
    "chash_keyfun": {
      "fun": "chash_std_keyfun",
      "mod": "riak_core_util"
    },
    "dw": "quorum",
    "last_write_wins": false,
    "linkfun": {
      "fun": "mapreduce_linkfun",
      "mod": "riak_kv_wm_link_walker"
    },
    "n_val": 3,
    "notfound_ok": true,
    "old_vclock": 86400,
    "postcommit": [],
    "pr": 0,
    "precommit": [],
    "pw": 0,
    "r": "quorum",
    "rw": "quorum",
    "small_vclock": 50,
    "w": "quorum",
    "young_vclock": 20
  }
}
```

Let's say that you'd like to create a bucket type called `user_account_bucket` with a [[pre-commit|Pre-Commit Hooks]] hook called `syntax_check` and two [[post-commit hooks|Post-Commit Hooks]] called `welcome_email` and `update_registry`. This would involve three steps:

1. Creating a JSON object with the appropriate `props`:

  ```json
  {
    "props": {
      "precommit": ["syntax_check"],
      "postcommit": ["welcome_email", "update_registry"]
    }
  }
  ```

1. Passing that JSON to the `bucket-type create` command:

  ```bash
  riak-admin bucket-type create user_account_bucket '{"props":{"precommit": ... }}'
  ```

If creation is successful, the console will return `user_account_bucket created`.

1. Activating the new bucket type:

```bash
riak-admin bucket-type activate user_account_bucket
```

If activation is successful, the console will return `user_account_bucket bas been activated`.

## Managing Bucket Types Through HTTP and Protocol Buffers

The simplest way of managing bucket types is via the interface provided by the `riak-admin bucket-type` command. A number of bucket type-related commands, however, may be issued through the HTTP and Protocol Buffers interfaces.

The most important thing to remember when managing bucket types outside of the `riak-admin` command interface is that bucket types must first be _activated_ in order to be manageable.

If you would like to manage the bucket type `strongly_consistent` via HTTP, for example, you would first need to create and activate the type. This can be done using `riak-admin`:

```bash
riak-admin bucket-type create strongly_consistent '{"props":{}}'
riak-admin bucket-type activate strongly_consistent
```

Now, you can view the properties (`props`) associated with that type via HTTP:

```curl
curl http://localhost:8098/types/strongly_consistent/props
```

If the type has not been activated or has not been created, you will receive `404 Not Found` errors.

But if the type _has_ been activated and you'd like to change its `props`---e.g. setting `allow_mult` to `false`---you can do so with a `PUT` request:

```curl
curl -XPUT \
  -H "Content-Type: application/json" \
  -d '{"props":{"allow_mult":false}}' \
  http://localhost:8098/types/strongly_consistent/props
```

This change would then be reflected in the console output if you ran either a `GET` request on the URL above _or_ by running the `bucket-type status` command:

```bash
riak-admin bucket-type status strongly_consistent
```

## Scratchpad

How can bucket types be deactivated? Possible without a workaround? Is that even advisable?

```bash
riak-admin deactivate useless_bucket_type
```

https://github.com/basho/riak/issues/362
https://github.com/basho/riak_core/blob/develop/src/riak_core_bucket_type.erl#L21-L89

