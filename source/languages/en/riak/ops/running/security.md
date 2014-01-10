---
title: Riak Security
project: riak
version: 2.0.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator]
---

<!---
RESOURCES:
https://gist.github.com/lukebakken/1dcf90bf2a6d4009c6db
http://vagabond.github.io/2013/11/06/ricon-west-2013-talk-writeup/
-->

### Enable security

```
riak-admin security enable
```

### Disable security

Disabling security only disables the various permission checks when
executing operations against Riak. Users, roles and other security
attributes remain untouched.

```
riak-admin security disable
```

### Status

```
# riak-admin security status
Enabled
```

### Add user

```
riak-admin security add-user riakuser
```

Add a user with a pre-defined password:

```
# riak-admin security add-user riakuser 'password=Test1234'
# riak-admin security print-users
+----------+---------------+----------------------------------------+------------------------------+
| username |     roles     |                password                |           options            |
+----------+---------------+----------------------------------------+------------------------------+
| riakuser |               |983e8ae1421574b8733824453c4bc8c9b531028c|              []              |
+----------+---------------+----------------------------------------+------------------------------+
```

### Add source

A user won't have access to resources simply because they have been
added via `add-user`. You must add a source as well as grants to give
access to Riak securables. Sources may apply to all users, or to a
specific user or role.

Available sources:

|---------------------------------------------------------------------|
|Source   | Description                                               |
|---------------------------------------------------------------------|
|trust    | Always authenticates successfully                         |
|---------------------------------------------------------------------|
|password | Use user's password against the one stored in Riak        |
|---------------------------------------------------------------------|
|pam      | Authenticate against given PAM service                    |
|---------------------------------------------------------------------|
|certificate | Authenticate using client certificate                  |
|---------------------------------------------------------------------|

#### Adding a trusted source

The following creates a source giving all users trusted access to
securables when requests come from `localhost`:

```
# riak-admin security add-source all 127.0.0.1/32 trust
# riak-admin security print-sources
+--------------------+------------+----------+----------+
|       users        |    cidr    |  source  | options  |
+--------------------+------------+----------+----------+
|        all         |127.0.0.1/32|  trust   |    []    |
+--------------------+------------+----------+----------+
```

#### Adding a "default" source

The following source requires a password for users connecting from any
host.

```
# riak-admin security add-source all 0.0.0.0/0 password
# riak-admin security print-sources
+--------------------+------------+----------+----------+
|       users        |    cidr    |  source  | options  |
+--------------------+------------+----------+----------+
|        all         |127.0.0.1/32|  trust   |    []    |
|        all         | 0.0.0.0/0  | password |    []    |
+--------------------+------------+----------+----------+
```

If a user connects from `127.0.0.1` they will be trusted, because that
source is more specific than the `0.0.0.0/0 - password` source.

### Delete user

```
# riak-admin security del-user riakuser
# riak-admin security print-users
+----------+---------------+----------------------------------------+------------------------------+
| username |     roles     |                password                |           options            |
+----------+---------------+----------------------------------------+------------------------------+
```

### Delete source

```
# riak-admin security del-source all 0.0.0.0/0
# riak-admin security print-sources
+--------------------+------------+----------+----------+
|       users        |    cidr    |  source  | options  |
+--------------------+------------+----------+----------+
|        all         |127.0.0.1/32|  trust   |    []    |
+--------------------+------------+----------+----------+
```

### Granting permissions

Permissions that can be granted:

* `riak_kv.get` - retrieve an object
* `riak_kv.put` - save or update object
* `riak_kv.delete` - delete object
* `riak_kv.index` - index an object using secondary indexes (2i)
* `riak_kv.list_keys` - list keys in bucket
* `riak_kv.list_buckets` - list buckets

riak_kv.mapreduce

riak_core.get_bucket
riak_core.set_bucket
riak_core.get_bucket_type
riak_core.set_bucket_type

riak_search.query

search.admin
search.query
```



