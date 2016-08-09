---
title_supertext: "Security"
title: "User Management"
description: "Manage user authorization and access to Riak TS."
menu:
  riak_ts-1.4.0:
    name: "User Management"
    identifier: "security_user_management"
    weight: 120
    parent: "security"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/using/security/user-management
canonical_link: "https://docs.basho.com/riak/ts/latest/using/security/user-management/"
---

Riak TS security lets you to control _authorization_ by creating, modifying, and deleting user characteristics and granting users selective access to Riak TS functionality. Users can be assigned one or more of the following characteristics:

* `username`
* `groups`
* `password`

You may also assign users characteristics beyond those listed
above, such as listing email addresses or other information, but those
values will carry no special significance for Riak.

{{% note %}}
The `username` is the one user characteristic that cannot be changed once a user has been created.
{{% /note %}}

## Retrieve a Current User or Group List

A list of currently existing users can be accessed at any time:

```bash
riak-admin security print-users
```

The same goes for groups:

```bash
riak-admin security print-groups
```

Example output, assuming user named `riakuser` with an assigned
password:

```
+----------+--------+----------------------+------------------------------+
| username | groups |       password       |           options            |
+----------+--------+----------------------+------------------------------+
| riakuser |        |983e8ae1421574b8733824|              []              |
+----------+--------+----------------------+------------------------------+
```

{{% note %}}
All passwords are displayed in encrypted form in console output.
{{% /note %}}

If the user `riakuser` were assigned to the group `dev` and a `name` of
`lucius`, the output would look like this:

```bash
+----------+----------------+----------------------+---------------------+
| username |     groups     |       password       |       options       |
+----------+----------------+----------------------+---------------------+
| riakuser |      dev       |983e8ae1421574b8733824| [{"name","lucius"}] |
+----------+----------------+----------------------+---------------------+
```

If you'd like to see which permissions have been assigned to
`riakuser`, you would need to use the `print-grants` command, detailed
below.

The `security print-user` or `security-print-group` (singular) commands
can be used with a name as argument to see the same information as
above, except for only that user or group.

## Permissions Grants For a Single User or Group

You can retrieve authorization information about a specific user or
group using the `print-grants` command, which takes the form of
`riak-admin security print-grants <username>`.

The output will look like this if the user `riakuser` has been
explicitly granted a `riak_ts.get` permission on the table
`shopping_list` and inherits a set of permissions from the `admin`
group:

```bash
Inherited permissions (user/riakuser)

+--------+----------+----------+----------------------------------------+
| group  |   type   |  table   |                 grants                 |
+--------+----------+----------+----------------------------------------+
| admin  |    *     |    *     |      riak_ts.get, riak_ts.delete,      |
|        |          |          |              riak_ts.put               |
+--------+----------+----------+----------------------------------------+

Dedicated permissions (user/riakuser)

+----------+-------------+----------------------------------------+
|   type   |   table     |                 grants                 |
+----------+-------------+----------------------------------------+
|   ANY    |shopping_list|               riak_ts.get              |
+----------+-------------+----------------------------------------+

Cumulative permissions (user/riakuser)

+----------+-------------+----------------------------------------+
|   type   |   table     |                 grants                 |
+----------+-------------+----------------------------------------+
|    *     |      *      |      riak_ts.get, riak_ts.delete,      |
|          |             |               riak_ts.put              |
|   ANY    |shopping_list|               riak_ts.get              |
+----------+-------------+----------------------------------------+
```

{{% note %}}
The term `admin` is not a reserved term in Riak security. It
is used here only for illustrative purposes.
{{% /note %}}

Because the same name can represent both a user and a group, a prefix
(`user/` or `group/`) can be used before the name (e.g., `print-grants
user/admin`). If a name collides and no prefix is supplied, grants for
both will be listed separately.

## Add Group

For easier management of permissions across several users, it is
possible to create groups to be assigned to those users.

```bash
riak-admin security add-group admin
```

## Add User

To create a user with the username `riakuser`, we use the `add-user`
command:

```bash
riak-admin security add-user riakuser
```

Using the command this way will create the user `riakuser` without _any_
characteristics beyond a username, which is the only attribute that you
must assign upon user creation.

Alternatively, a password (or other attributes) can be assigned to the
user upon creation. Here, we'll assign a password:

```bash
riak-admin security add-user riakuser password=Test1234
```

## Assigning a Password and Altering Existing User Characteristics

While passwords and other characteristics can be set upon user creation,
it often makes sense to change user characteristics after the user has
already been created. Let's say that the user `riakuser` was created
without a password (or created _with_ a password that we'd like to
change). The `alter-user` command can be used to modify our `riakuser`
user:

```bash
riak-admin security alter-user riakuser password=opensesame
```

When creating or altering a user, any number of `<option>=<value>`
pairs can be appended to the end of the command. Any non-standard
options will be stored and displayed via the `riak-admin security
print-users` command.

```bash
riak-admin security alter-user riakuser name=bill age=47 fav_color=red
```

Now, the `print-users` command should return this:

```
+----------+--------+----------+--------------------------------------------------+
| username | groups | password |                     options                      |
+----------+--------+----------+--------------------------------------------------+
| riakuser |        |          |[{"fav_color","red"},{"age","47"},{"name","bill"}]|
+----------+--------+----------+--------------------------------------------------+
```

{{% note %}}
Usernames _cannot_ be changed using the `alter-user` command.
If you attempt to do so by running `alter-user riakuser
username=other-name`, for example, this will add the
`{"username","other-name"}` tuple to `riakuser`'s options.
{{% /note %}}

## Managing Groups for a User

If we have a user `jane_goodall` and we'd like to assign her to the
`admin` group, we assign the value `admin` to the option `groups`:

```bash
riak-admin security alter-user jane_goodall groups=admin
```

If we'd like to make the user `jane_goodall` both an `admin` and an
`archoverlord`:

```bash
riak-admin alter-user jane_goodall groups=admin,archoverlord
```

There is no way to incrementally add groups; even if `jane_goodall` was
already an `admin`, it is necessary to list it again when adding the
`archoverlord` group. Thus, to remove a group from a user, use
`alter-user` and list all *other* groups.

If the user should be removed from all groups, use `groups=` with no
list:

```bash
riak-admin alter-user jane_goodall groups=
```

## Managing Groups for Groups

Groups can be added to other groups for cascading permissions.

```bash
riak-admin alter-group admin groups=dev
```

## Deleting a User or Group

If you'd like to remove a user, use the `del-user` command:

```bash
riak-admin security del-user riakuser
```

For groups, use the `del-group` command:

```bash
riak-admin security del-group admin
```

## Adding or Deleting Multiple Users

The `riak-admin security` command does not currently allow you to
add or delete multiple users using a single command. Instead, they must
be added or deleted one by one.

## Managing Permissions

Permission to perform a wide variety of operations against Riak TS can be
granted to, or revoked from, users via the `grant` and `revoke`
commands.

### `grant`

The `grant` command takes one of the following forms:

```bash
riak-admin security grant <permissions> on any to all|{<user>|<group>[,...]}
riak-admin security grant <permissions> on <bucket-type> to all|{<user>|<group>[,...]}
riak-admin security grant <permissions> on <bucket-type> <table> to all|{<user>|<group>[,...]}
```

### `revoke`

The `revoke` command is essentially the same, except that `to` is
replaced with `from` of `to`:

```bash
riak-admin security revoke <permissions> on any from all|{<user>|<group>[,...]}
riak-admin security revoke <permissions> on <bucket-type> from all|{<user>|<group>[,...]}
riak-admin security revoke <permissions> on <bucket-type> <table> from all|{<user>|<group>[,...]}
```

### Selecting `any` and `all`

If you select `any`, this means that the permission (or set of
permissions) is granted/revoked for all tables and [bucket types](/riak/kv/2.1.4/developing/usage/bucket-types). If you specify a bucket type only, then the permission is granted/revoked for all buckets of that type. If you specify a bucket type _and_ a table, the permission is granted/revoked only for that bucket type/table combination.

**Note**: You cannot grant/revoke permissions with respect to a bucket
alone. You must specify either a bucket type by itself or a bucket type
and bucket. This limitation reflects the naming structure underlying
buckets and bucket types.

Selecting `all` grants or revokes a permission (or set of permissions)
for all users in all groups. When specifying the user(s)/group(s) to
which you want to apply a permission (or set of permissions), you may
list any number of users or groups comma-separated with no whitespace.
Here is an example of granting multiple permissions across all buckets
and bucket types to multiple users:

```bash
riak-admin security grant riak_ts.get,riak_search.query on any to jane,ahmed
```

If the same name is used for both a user and a group, the `grant`
command will ask for the name to be prefixed with `user/` or `group/`
to disambiguate.

## Time Series Permissions

Permissions that can be granted for basic time series access
functionality:

Permission | Operation |
:----------|:----------|
`riak_ts.get` | Retrieve time series data via a single key
`riak_ts.put` | Write time series data via a single key or via a SQL INSERT statement
`riak_ts.delete` | Delete a single record via a key
`riak_ts.list_keys` | Stream a list of primary keys for a table
`riak_ts.coverage` | Return coverage information for a given query
`riak_ts.create_table` | Create a time series table
`riak_ts.query_select` | Retrieve data via a SQL SELECT statement
`riak_ts.describe_table` | Retrieve metadata about a single TS table
`riak_ts.show_tables` | Retrieve a list of TS tables
`riak_ts.query_explain` | Diagnostic information about how a query will be executed

{{% note title="Note on Listing Keys and Tables" %}}
`riak_ts.list_keys` and `riak_ts.show_tables` are
both very expensive operations that should be performed very rarely and
never in production. Access to this functionality should be granted very
carefully.
{{% /note %}}

If you'd like to create, for example, a `client` account that is
allowed only to run `GET` and `PUT` requests on all tables:

```bash
riak-admin security add-user client
riak-admin security grant riak_ts.get,riak_ts.put on any to client
```

## MapReduce Permissions

Permission to perform [MapReduce](/riak/kv/2.1.4/developing/usage/mapreduce/) jobs can be assigned
using `riak_kv.mapreduce`. The following example grants MapReduce
permissions to the user `mapreduce-power-user` for all buckets and
bucket types:

```bash
riak-admin security grant riak_kv.mapreduce on any to mapreduce-power-user
```

## Bucket Type Permissions

In versions 2.0 and later, Riak users can manage [bucket types](/riak/kv/2.1.4/developing/usage/bucket-types) in addition to setting bucket properties. `riak-admin
security` allows you to manage the following bucket type-related
permissions:

Permission | Operation
:----------|:---------
`riak_core.get_bucket` | Retrieve the `props` associated with a bucket
`riak_core.set_bucket` | Modify the `props` associated with a bucket
`riak_core.get_bucket_type` | Retrieve the set of `props` associated with a bucket type
`riak_core.set_bucket_type` | Modify the set of `props` associated with a bucket type

### Search Query Permission (Riak Search version 1)

Security is incompatible with the original (and now deprecated) Riak
Search. Riak Search version 1 will stop working if security is enabled.

### Search Query Permissions (Riak Search version 2, aka Yokozuna)

If you are using the new Riak Search, i.e. the Solr-compatible search
capabilities included with Riak versions 2.0 and greater, the following
search-related permissions can be granted/revoked:

Permission | Operation
:----------|:---------
`search.admin` | The ability to perform search admin-related tasks, such as creating and deleting indexes and adding and modifying search schemas
`search.query` | The ability to query an index

{{% note title="Note on Search Permissions" %}}
Search must be enabled in order to successfully grant/revoke Search
permissions. If you attempt to grant/revoke permissions while Search is
disabled, you will get the following error:

`{error,{unknown_permission,"search.query"}}`

More information on Riak Search and how to enable it can be found in the
[Riak Search Settings](/riak/kv/2.1.4/configuring/search/) document.
{{% /note %}}

#### Usage Examples

To grant the user `riakuser` the ability to query all indexes:

```bash
riak-admin security grant search.query on index to riakuser

# To revoke:
# riak-admin security revoke search.query on index from riakuser
```

To grant the user `riakuser` the ability to query all schemas:

```bash
riak-admin security grant search.query on schema to riakuser

# To revoke:
# riak-admin security revoke search.query on schema from riakuser
```

To grant the user `riakuser` admin privileges only on the index
`riakusers_index`:

```bash
riak-admin security grant search.admin on index riakusers_index to riakuser

# To revoke:
# riak-admin security revoke search.admin on index riakusers_index from riakuser
```

To grant `riakuser` querying and admin permissions on the index
`riakusers_index`:

```bash
riak-admin security grant search.query,search.admin on index riakusers_index to riakuser

# To revoke:
# riak-admin security revoke search.query,search.admin on index riakusers_index from riakuser
```
