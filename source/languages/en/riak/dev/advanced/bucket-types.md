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

