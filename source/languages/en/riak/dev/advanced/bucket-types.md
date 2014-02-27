---
title: Using Bucket Types
project: riak
version: 2.0.0+
document: tutorial
audience: intermediate
keywords: [developers, buckets]
---

Bucket types allow groups of buckets to share configuration details and for Riak users to manage bucket properties in a more efficient way.

## How Bucket Types Work

The ad hoc approach to bucket configuration involves setting bucket properties for specific buckets either through [[HTTP|HTTP Set Bucket Properties]] or [[Protocol Buffers|PBC Set Bucket Properties]]. With this approach, you can take a bucket `my_bucket` and modify any number of its properties, from `n_val` to `allow_mult` and far beyond.

Using bucket *types* also involves dealing with bucket properties, but with a few crucial differences:

* Bucket types enable you to assign a total set of properties to buckets _at the time of their creation_ (instead of setting buckets' properties and then using those buckets)
* Bucket types must be both created _and_ activated before they can be used (whereas bucket properties can be modified at any time)
* Nearly all bucket properties can be updated using bucket types, with two exceptions: the `datatype` and `consistent` properties

It is important to note that buckets are not assigned types in the same way that they are configured when using [[bucket properties|The Basics#Bucket-Properties-and-Operations]]. You cannot simply take a bucket `my_bucket` and assign it a type the way that you would, say, set `allow_mult` to `false` or `n_val` to `5`, because there is no `type` parameter contained within the bucket's properties (i.e. `props`).

Instead, bucket types are applied to buckets _on the basis of how those buckets are queried_. Queries involving bucket types take the following form:

```curl
GET/PUT/DELETE /types/<type>/buckets/<bucket>/keys/<key>
```

If you have created the bucket type `no_siblings` (with `allow_mult` set to `false`) and would like that type to be applied to the bucket `sensitive_user_data`, you would need to run operations on that bucket in accordance with the format above. Here is an example HTTP query:

```curl
curl -XPUT \
  -H "Content-Type: application/json" \
  -d "{ ... user data ... }" \
  http://localhost:8098/types/no_siblings/buckets/sensitive_user_data/keys/user19735
```

In this example, the bucket `sensitive_user_data` bears the configuration established by the `no_siblings` bucket type, and it bears that configuration _on the basis of the query's structure_.

This is because buckets act as a separate namespace in Riak, in addition to buckets and keys. More on this in the section directly below.

## Buckets as Namespaces

In versions of Riak prior to 2.0, all queries are made to a bucket/key pair, as in the following example URL:

```curl
curl http://localhost:8098/buckets/my_bucket/keys/my_key
```

With the addition of bucket types in Riak 2.0, bucket types can be used as _an additional namespace_ on top of buckets and keys. The same bucket name can be associated with completely different data if it used in accordance with a different type. Thus, the following two requests will be made to _completely different keys_, even though the bucket and key names are the same:

```curl
curl http://localhost:8098/types/type1/my_bucket/my_key
curl http://localhost:8098/types/type2/my_bucket/my_key
```

<div class="note">
<div class="title">Note</div>
In Riak 2.x, <em>all_ requests</em> must be made to a location specified by a bucket type, bucket, and key rather than to a bucket/key pair, as in previous versions.
</div>

If requests are made to a bucket/key pair without a specified bucket type, the `default` bucket type will be used. The following queries are thus identical:

```curl
curl http://localhost:8098/buckets/my_bucket/keys/my_key
curl http://localhost:8098/types/default/my_bucket/keys/my_key
```

Below is a listing of the `props` associated with the `default` bucket type:

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

## Usage Example

Let's say that we're using Riak to store internet memes. We've been using a bucket called `current_memes` using the bucket type `no_siblings` (from above). At a certain point, we decide that our application needs to use a new bucket called `old_memes` to store memes that have gone woefully out of fashion, but that bucket also needs to bear the type `no_siblings`.

The following request seeks to add the meme "all your base are belong to us" to the `old_memes` bucket. If the bucket type `no_siblings` has been created and activated, the request will ensure that the `old_memes` bucket inherits all of the properties from the type `no_siblings`:

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "all your base are belong to us" \
  http://localhost:8098/types/no_siblings/buckets/old_memes/keys/all_your_base
```

This query would both create the bucket `old_memes` and ensure that the configuration contained in the `no_siblings` is applied to the bucket all at once.

If we wished, we could also store store some both old and new memes in buckets with different types. We could use the `no_siblings` bucket from above if we didn't want to deal with siblings, vclocks, and the like, and we could use a `siblings_allowed` bucket type (with all of the default properties except `allow_mult` set to `true`). This would give use four bucket type/bucket pairs:

* `no_siblings` / `old_memes`
* `no_siblings` / `new_memes`
* `siblings_allowed` / `old_memes`
* `siblings_allowed` / `new_memes`

All four of these pairs are isolated keyspaces. The key `favorite_meme` could hold separate values in all four bucket type/bucket spaces.

## Managing Bucket Types Through the Command Line

Bucket types are created, updated, activated, and more through the `riak-admin bucket-type` interface.

Below is a full list of available sub-commands:

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
riak-admin bucket-type list

# Response:
type1 (active)
type2 (not active)
type3 (active)
```

### Checking a Type's Status

You can check on the status---i.e. the configuration details---of a bucket type using the `status <type>` command:

```bash
riak-admin bucket-type status my_bucket_type
```

The console will output two things if the type exists:

1. Whether or not the type is active
2. The bucket properties associated with the type

If you check the status of a currently active type called `my_bucket_type` that simply bears a default bucket configuration, the output will be as follows:

```bash
my_bucket_type is active

active: true
allow_mult: true

... other properties ...

w: quorum
young_vclock:20
```

### Activating a Bucket Type

Simply run the `activate` command to activate a type:

```bash
riak-admin bucket-type activate my_bucket_type
```

<div class="note">
<div class="title">Note</div>
A bucket type can be activated only when it is considered ready by Riak (i.e. when the type has been propagated to all running nodes). You can check on the type's readiness by running `riak-admin bucket-type status <type_name>`. The first line of output will indicate whether or not the type is ready.

In a stable cluster, bucket types should propagate very quickly. If, however, a cluster is experiencing network partitions or other issues, you will need to resolve those issues before bucket types can be activated.
</div>

### Creating a Bucket Type

Creating new bucket types involves using the `create <type> <json>` command, where `<type>` is the name of the type and `<json>` is a JSON object of the following form:

```json
{
  "props": {
    "prop1": "val1",
    "prop2": "val2",
    ...
  }
}
```

Any property/value pair that is contained in the `props` object will either add a property that is not currently specified or override a default config. 

If you'd like to create a bucket type that simply extends Riak's defaults, for example, pass an empty JavaScript object to the `props` parameter:

```bash
riak-admin bucket-type create type_using_defaults '{"props":{}}'
```

**Note**: The `create` command can be run multiple times prior to a bucket type being activated. Riak will persist only those properties contained in the final call of the command.

Creating bucket types _always_ involves passing stringified JSON to the `create` command. One way to do that is to simply pass a string directly, as above, but you can also do so by passing the contents of a file, such as a `.json` file:

```bash
riak-admin bucket-type create from_json_file '`cat props.json`'
```

### Updating a Bucket Type

The `bucket-type update` command functions much like the `bucket-type create` command. It simply involves specifying the name of the bucket type that you wish to modify and a JSON object containing the properties of the type:

```bash
riak-admin bucket-type update type_to_update '{"props":{ ... }}'
```

<div class="note">
<div class="title">Note</div>
Any bucket properties associated with a type can be modified after a bucket is created, with two important exceptions: <tt>consistent</tt> and <tt>type</tt>. If a bucket type entails strong consistency (requiring that <tt>consistent</tt> be set to <tt>true</tt>) or is set up as a [[CRDT]]&mdash;<tt>map</tt>, <tt>set</tt>, or <tt>counter</tt>&mdash;then this will be true of the bucket type once and for all.

If you need to change one of these properties, it is recommended that you simply create a new bucket type.
</div>

## Bucket Type Example

Let's say that you'd like to create a bucket type called `user_account_bucket` with a [[pre-commit hook|Pre-Commit Hooks]] called `syntax_check` and two [[post-commit hooks|Post-Commit Hooks]] called `welcome_email` and `update_registry`. This would involve four steps:

1. Creating a JavaScript object containing the appropriate `props` settings:

```json
{
  "props": {
    "precommit": ["syntax_check"],
    "postcommit": ["welcome_email", "update_registry"]
  }
}
```

2. Passing that JSON to the `bucket-type create` command:

```bash
riak-admin bucket-type create user_account_bucket '{"props":{"precommit": ["syntax_check"], ... }}'
```

If creation is successful, the console will return `user_account_bucket created`.

3. Verifying that the type is ready to be activated:

Once the type is created, you can check whether your new type is ready to be activated by running:

```bash
riak-admin bucket-type status user_account_bucket
```

If the first line reads `user_account_bucket has been created and may be activated`, then you can proceed to the next step. If it reads `user_account_bucket has been created and is not ready to activate`, then wait a moment and try again. If it still does not work, then there may be network partition or other issues that need to be addressed in your cluster.

4. Activating the new bucket type:

```bash
riak-admin bucket-type activate user_account_bucket
```

If activation is successful, the console will return `user_account_bucket has been activated`.