---
title: Using Bucket Types
project: riak
version: 2.0.0+
document: tutorial
audience: intermediate
keywords: [developers, buckets]
---

https://github.com/basho/riak/issues/362  

`riak-admin bucket-type`

Essentially a simplification of `curl`

All bucket-configuring JSON is of the form:

```json
{
  "props": {
    "prop1": "val1",
    "prop2": "val2"
  }
}
```

Command | Action | Form |
:-------|:-------|:-----|
`list` | List all currently available bucket types and their activation status | `list` |
`status` | Display the status and properties of a specific bucket type | `status <type>` |
`activate` | Activate a bucket type | `activate <type>` |
`create` | Create or modify a bucket type before activation | `create <type> <json>` |
`update` | Update a bucket type after activation | `update <type> <json>` |

## Assigning a Type to a Bucket

Activating a bucket type does _not_ mean that any buckets are actually assigned to that type

Buckets are not assigned types the way that they are configured using `props`; you don't simply take a bucket `test_bucket` and "assign it a type"; instead, buckets _end up_ having a type based on how they are queried. Via HTTP:

`GET /types/<type>/buckets/<type>/keys/<key>`

More concrete example:

`GET /types/consistent_bucket/buckets/sensitive_user_data/keys/dave_mitchell_data`

In this example, the bucket `sensitive_user_data` bears the configuration established by the `consistent_bucket` bucket type.

The downside is that you can't assign a bucket to a type and then query it normally, i.e. `GET /buckets/<bucket>/keys/<key>` without the `/types/<type>` prefix (although this is easy enough using the normal method, i.e. `{"props":{ ... }}`. The upside is that you can _dynamically_ assign bucket types to buckets. If you run a `PUT` request on the `types/my_custom_type/buckets/my_bucket/keys/test_key`, then the bucket `my_bucket` will be created in accordance with the type configuration for `my_custom_type` _all at once_. This means that you don't have to know in advance which buckets you will need. This allows bucket creation to remain dynamic while also harnessing the power of bucket types.

## Managing Bucket Types Via HTTP

**Note**: You can only configure bucket types via HTTP if the bucket has been activated using `riak-admin bucket-type activate <type>`.

`GET /types/<type>/props`
`PUT /types/<type>/props`
`POST /types/<type>/props`

## Questions

How can bucket types be deactivated? Possible without a workaround?

```bash
riak-admin deactivate useless_bucket_type
```

