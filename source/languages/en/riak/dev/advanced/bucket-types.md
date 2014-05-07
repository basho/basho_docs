---
title: Using Bucket Types
project: riak
version: 2.0.0+
document: tutorial
audience: intermediate
keywords: [developers, buckets]
---

Bucket types allow groups of buckets to share configuration details and for Riak users to manage bucket properties in a more efficient way.

<div class="note">
<div class="title">Important note on cluster downgrades</div>
If you upgrade a Riak to version 2.0 or later, you can still downgrade the cluster to a pre-2.0 version <em>if and only if you have not created and activated a bucket type in the cluster</em>. Once any bucket type has been created and activated, you can no longer downgrade the cluster to a pre-2.0 version.
</div>

## How Bucket Types Work

The ad hoc approach to bucket configuration involves setting bucket properties for specific buckets either through [[HTTP|HTTP Set Bucket Properties]] or [[Protocol Buffers|PBC Set Bucket Properties]]. With this approach, you can take a bucket `my_bucket` and modify any number of its properties, from `n_val` to `allow_mult` and far beyond.

Using bucket *types* also involves dealing with bucket properties, but with a few crucial differences:

* Bucket types enable you to assign a total set of properties to buckets _at the time of their creation_ (instead of setting buckets' properties and then using those buckets)
* Bucket types must be both created _and_ activated before they can be used (whereas bucket properties can be modified at any time)
* Nearly all bucket properties can be updated using bucket types, with two exceptions: the `datatype` and `consistent` properties

It is important to note that buckets are not assigned types in the same way that they are configured when using [[bucket properties|The Basics#Bucket-Properties-and-Operations]]. You cannot simply take a bucket `my_bucket` and assign it a type the way that you would, say, set `allow_mult` to `false` or `n_val` to `5`, because there is no `type` parameter contained within the bucket's properties (i.e. `props`).

Instead, bucket types are applied to buckets _on the basis of how those buckets are queried_. Queries involving bucket types take the following form:

```
GET/PUT/DELETE /types/<type>/buckets/<bucket>/keys/<key>
```

<div class="note">
<div class="title">When to use bucket types</div>
In many respects, bucket types are a major improvement over the older system of bucket configuration, including the following:
<ul>
<li>Bucket types are more flexible because they enable you to define total configurations of bucket properties all at once and then change them if you need to.</li>
<li>Bucket types are more reliable because the buckets that bear a given type only have their properties changed when the type is changed. Previously, it was possible to change the properties of a bucket only through client requests.</li>
<li>Whereas bucket properties can only be altered by clients interacting with Riak, bucket types are more of an operational concept. The <tt>riak-admin bucket-type</tt> interface (discussed in depth below) enables you to manage bucket configurations without recourse to clients.</li>
</ul>

For these reasons, we recommend <tt>always</tt> using bucket types.
</div>

## Usage Example

If you have created the bucket type `no_siblings` (with the property `allow_mult` set to `false`) and would like that type to be applied to the bucket `sensitive_user_data`, you would need to run operations on that bucket in accordance with the format above. Here is an example write:

```ruby
bucket = client.bucket('sensitive_user_data')
obj = Riak::RObject.new(bucket, 'user19735')
obj.content_type = 'application/json'
obj.raw_data = '{ ... user data ... }'
obj.store(type: 'no_siblings')
```

```java
Location key = new Location("sensitive_user_data")
        .setBucketType("no_siblings")
        .setKey("user19735");
RiakObject obj = new RiakObject()
        .setContentType("application/json")
        .setValue(BinaryValue.create("{ ... user data ... }"));
StoreValue store = new StoreValue.Builder(obj).build();
client.execute(store);
```

```python
bucket = client.bucket('sensitive_user_data', bucket_type='no_siblings')
obj = RiakObject(client, bucket, 'user19735')
obj.content_type = 'application/json'
obj.data = '{ ... user data ... }'
obj.store()
```

```erlang
Object = riakc_obj:new({<<"no_siblings">>, <<"sensitive_user_data">>},
                       <<"user19735">>,
                       <<"{ ... user data ... }">>,
                       <<"application/json">>),
riakc_pb_socket:put(Pid, Object).
```

```curl
curl -XPUT \
  -H "Content-Type: application/json" \
  -d "{ ... user data ... }" \
  http://localhost:8098/types/no_siblings/buckets/sensitive_user_data/keys/user19735
```

In this example, the bucket `sensitive_user_data` bears the configuration established by the `no_siblings` bucket type, and it bears that configuration _on the basis of the query's structure_.

This is because buckets act as a separate namespace in Riak, in addition to buckets and keys.

## Buckets as Namespaces

In versions of Riak prior to 2.0, all queries are made to a bucket/key pair, as in the following example URL:

```ruby
bucket = client.bucket('my_bucket')
bucket.get('my_key')
```

```java
Location myKey = new Location("my_bucket")
        .setKey("my_key");
FetchValue fetch = new FetchValue.Builder(myKey).build();
client.execute(fetch);
```

```python
bucket = client.bucket('my_bucket')
bucket.get('my_key')
```

```erlang
{ok, Object} = riakc_pb_socket:get(Pid,
                                   <<"my_bucket">>,
                                   <<"my_key">>).
```

```curl
curl http://localhost:8098/buckets/my_bucket/keys/my_key
```

With the addition of bucket types in Riak 2.0, bucket types can be used as _an additional namespace_ on top of buckets and keys. The same bucket name can be associated with completely different data if it used in accordance with a different type. Thus, the following two requests will be made to _completely different keys_, even though the bucket and key names are the same:

```ruby
bucket = client.bucket('my_bucket')

bucket.get('my_key', type: 'type1')
bucket.get('my_key', type: 'type2')
```

```java
Location key1 = new Location("my_bucket")
        .setBucketType("type1")
        .setKey("my_key");
Location key2 = new Location("my_bucket")
        .setBucketType("type2")
        .setKey("my_key");
FetchValue fetch1 = new FetchValue.Builder(key1).build();
FetchValue fetch2 = new FetchValue.Builder(key2).build();
client.execute(fetch1);
client.execute(fetch2);
```

```python
bucket1 = client.bucket('my_bucket', bucket_type='type1')
bucket2 = client.bucket('my_bucket', bucket_type='type2')
bucket1.get('my_key')
bucket2.get('my_key')
```

```erlang
{ok, Obj1} = riakc_pb_socket:get(Pid,
                                 {<<"type1">>, <<"my_bucket">>},
                                 <<"my_key">>),
{ok, Obj2} = riakc_pb_socket:get(Pid,
                                 {<<"type2">>, <<"my_bucket">>},
                                 <<"my_key">>).                               
```

```curl
curl http://localhost:8098/types/type1/my_bucket/my_key
curl http://localhost:8098/types/type2/my_bucket/my_key
```

<div class="note">
<div class="title">Note</div>
In Riak 2.x, <em>all requests</em> must be made to a location specified by a bucket type, bucket, and key rather than to a bucket/key pair, as in previous versions.
</div>

If requests are made to a bucket/key pair without a specified bucket type, the `default` bucket type will be used. The following queries are thus identical:

```ruby
bucket = client.bucket('my_bucket')

bucket.get('my_key')
bucket.get('my_key', type: 'default')
```

```java
Location withDefaultBucketType = new Location("my_bucket")
        .setBucketType("default")
        .setKey("my_key");
Location noBucketType = new Location("my_bucket")
        .setKey("my_key");
FetchValue fetch1 = new FetchValue.Builder(withDefaultBucketType).build();
FetchValue fetch2 = new FetchValue.Builder(noBucketType).build();
client.execute(fetch1);
client.execute(fetch2);
```

```python
bucket1 = client.bucket('my_bucket', bucket_type='default')
bucket2 = client.bucket('my_bucket')
bucket1.get('my_key')
bucket2.get('my_key')
```

```erlang
{ok, Obj1} = riakc_pb_socket:get(Pid,
                                 {<<"default">>, <<"my_bucket">>},
                                 <<"my_key">>),
{ok, Obj2} = riakc_pb_socket:get(Pid,
                                 <<"my_bucket">>},
                                 <<"my_key">>). 
```

```curl
curl http://localhost:8098/buckets/my_bucket/keys/my_key
curl http://localhost:8098/types/default/my_bucket/keys/my_key
```

Below is a listing of the `props` associated with the `default` bucket type:

```json
{
  "props": {
    "allow_mult": false,
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

## Bucket Types and the `allow_mult` Setting

Prior to Riak 2.0, Riak created [[siblings|Vector Clocks#Siblings]] in the case of conflicting updates only when explicitly instructed to do so, by setting `allow_mult` to `true`. The default `allow_mult` setting was thus `false`.

In version 2.0, this is changing in a subtle way. Now, there are two different default settings for `allow_mult` in play:

* For the `default` bucket type, `allow_mult` is set to `false` by default, as in previous versions of Riak
* For all newly-created bucket types, the default is now `true`. It is possible to set `allow_mult` to `false` if you wish to avoid resolving sibling conflicts, but this needs to be done explicitly.

This means that applications that have previously ignored conflict resolutions in certain buckets (or all buckets) can continue to do so. New applications, however, are encouraged to retain and resolve siblings with the appropriate application-side business logic.

To give an example, let's have a look at the properties associated with the `default` bucket type:

```bash
riak-admin bucket-type status default | grep allow_mult
```

The output:

```
allow_mult: false
```

Now, let's create a new bucket type called `n_val_of_2`, which sets the `n_val` to 2 but doesn't explicitly set `allow_mult`:

```bash
riak-admin bucket-type create n_val_of_2 '{"props":{"n_val":2}}'
```

When specifying this bucket type's properties as above, the `allow_mult` parameter was not changed. However, if we view the bucket type's properties, we can see in the console output that `allow_mult` is set to `true`:

```bash
riak-admin bucket-type status n_val_of_2 | grep allow_mult
```

The output:

```
allow_mult: true
```

This is important to bear in mind when using versions of Riak 2.0 and later any time that you create, activate, and use your own bucket types. It is still possible to set `allow_mult` to `false` in any given bucket type, but it must be done explicitly. If we wanted to to set `allow_mult` to `false` in our `n_val_of_2` bucket type from above, we would need to create or modify the already existing type as follows:

```bash
riak-admin bucket-type update n_val_of_2 '{"props":{"allow_mult":false}}'
```

## Usage Example

Let's say that we're using Riak to store internet memes. We've been using a bucket called `current_memes` using the bucket type `no_siblings` (from above). At a certain point, we decide that our application needs to use a new bucket called `old_memes` to store memes that have gone woefully out of fashion, but that bucket also needs to bear the type `no_siblings`.

The following request seeks to add the meme "all your base are belong to us" to the `old_memes` bucket. If the bucket type `no_siblings` has been created and activated, the request will ensure that the `old_memes` bucket inherits all of the properties from the type `no_siblings`:


```ruby
bucket = client.bucket('old_memes')
obj = Riak::RObject.new(bucket, 'all_your_base')
obj.content_type = 'text/plain'
obj.raw_data = 'all your base are belong to us'
obj.store(type: 'no_siblings')
```

```java
Location allYourBaseKey = new Location("old_memes")
        .setBucketType("no_siblings")
        .setKey("all_your_base");
RiakObject obj = new RiakObject()
        .setContentType("text/plain")
        .setValue(BinaryValue.create("all your base are belong to us"));
StoreValue store = new StoreValue.Builder(obj).build();
client.execute(store);
```

```python
bucket = client.bucket('old_memes', bucket_type='no_siblings')
obj = RiakObject(client, bucket, 'all_your_base')
obj.content_type = 'text/plain'
obj.data = 'all your base are belong to us'
obj.store()
```

```erlang
Object = riakc_obj:new({<<"no_siblings">>, <<"old_memes">>},
                       <<"all_your_base">>,
                       <<"all your base are belong to us">>,
                       <<"text/plain">>),
riakc_pb_socket:put(Pid, Object).
```

```curl
curl -XPUT \
  -H "Content-Type: text/plain" \
  -d "all your base are belong to us" \
  http://localhost:8098/types/no_siblings/buckets/old_memes/keys/all_your_base
```

This query would both create the bucket `old_memes` and ensure that the configuration contained in the `no_siblings` bucket type is applied to the bucket all at once.

If we wished, we could also store store both old and new memes in buckets with different types. We could use the `no_siblings` bucket from above if we didn't want to deal with siblings, vclocks, and the like, and we could use a `siblings_allowed` bucket type (with all of the default properties except `allow_mult` set to `true`). This would give use four bucket type/bucket pairs:

* `no_siblings` / `old_memes`
* `no_siblings` / `new_memes`
* `siblings_allowed` / `old_memes`
* `siblings_allowed` / `new_memes`

All four of these pairs are isolated keyspaces. The key `favorite_meme` could hold different values in all four bucket type/bucket spaces.

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
Any bucket properties associated with a type can be modified after a bucket is created, with two important exceptions: <tt>consistent</tt> and <tt>datatype</tt>. If a bucket type entails strong consistency (requiring that <tt>consistent</tt> be set to <tt>true</tt>) or is set up as a <tt>map</tt>, <tt>set</tt>, or <tt>counter</tt>, then this will be true of the bucket type once and for all.

If you need to change one of these properties, it is recommended that you simply create a new bucket type.
</div>

## Bucket Type Example

Let's say that you'd like to create a bucket type called `user_account_bucket` with a [[pre-commit hook|Using Commit Hooks#Pre-Commit-Hooks]] called `syntax_check` and two [[post-commit hooks|Using Commit Hooks#Post-Commit-Hooks]] called `welcome_email` and `update_registry`. This would involve four steps:

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

    If activation is successful, the console will return `user_account_bucket has been activated`. The bucket type is now fully ready to be used.
