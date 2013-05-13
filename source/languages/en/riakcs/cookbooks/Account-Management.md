---
title: Account Management
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator]
---

## Creating a User Account

Create a user account by performing an HTTP POST or PUT with a unique email address and username. For example:

```bash
curl -H 'Content-Type: application/json' -X POST http://localhost:8080/riak-cs/user --data '{"email":"foobar@example.com", "name":"foo bar"}'
```

<div class="note"><div class="title">Note</div>
By default, only the admin user may create new user accounts. If you need to create a user account without authenticating yourself, you must set <tt>{anonymous_user_creation, true}</tt> in the Riak CS <tt>app.config</tt>.
</div>

The submitted user document may be either JSON or XML, but the type should match the value of the Content-Type header used.

Here are some examples for JSON and XML input formats.

JSON:

```json
{
  "email" : "foobar@example.com",
  "name" : "foo bar"
}
```

XML:

```xml
<User>
  <Email>foobar@example.com</Email>
  <Name>foo bar</Name>
</User>
```

The response will be in JSON or XML, and resembles the following examples.

JSON:

```json
{
    "email": "foobar@example.com",
    "display_name": "foobar"
    "key_id": "324ABC0713CD0B420EFC086821BFAE7ED81442C",
    "key_secret": "5BE84D7EEA1AEEAACF070A1982DDA74DA0AA5DA7",
    "name": "foo bar",
    "id":"8d6f05190095117120d4449484f5d87691aa03801cc4914411ab432e6ee0fd6b",
    "buckets": []
}
```

XML:

```xml
<User>
  <Email>foobar@example.com</Email>
  <DisplayName>foobar</DisplayName>
  <KeyId>324ABC0713CD0B420EFC086821BFAE7ED81442C</KeyId>
  <KeySecret>5BE84D7EEA1AEEAACF070A1982DDA74DA0AA5DA7</KeySecret>
  <Name>foo bar</Name>
  <Id>8d6f05190095117120d4449484f5d87691aa03801cc4914411ab432e6ee0fd6b</Id>
  <Buckets></Buckets>
</User>
```

Once the user account exists, you can use the `key_id` and `key_secret` to authenticate requests with Riak CS. To do that, add the key_id and key_secret values to your s3cmd configuration file, which is located by default in the `~/.s3cmd` folder:

* JSON \`key_id\` -> \`~/.s3cmd\` \`access_key\` key_id_value_here
* JSON \`key_secret\` -> \`~/.s3cmd\` \`secret_key\` key_secret_value_here

The canonical id represented by the `id` field can be used as an alternative to an email address for user identification when granting or revoking ACL permissions, for example with the `--acl-grant` or `--acl-revoke` options to `s3cmd setacl`:

* JSON `id` -> `USER_CANONICAL_ID`

## Retrieving User Account Information
A user may retrieve their account information by sending a properly signed request to the **riak-cs/user** resource. Additionally, the admin user may request the information for any individual user on the system as part of their role as administrator. Users are only permitted to retrieve information for their account.

Assuming the proper credentials were set in the `.s3cfg` file, an s3cmd request to retrieve this information would look like this:

    s3cmd get s3://riak-cs/user -

Using the admin credentials to retrieve another user's info would look like this:

    s3cmd -c ~./s3cfg-admin get s3://riak-cs/user/XQKMYF4UL_MMTDFD6NCN

In this example, XQKMYF4UL_MMTDFD6NCN is the key_id of the user whose information the administrator wishes to retrieve.

## Retrieving a List of All Users
The admin user may retrieve a list of all user accounts on the system. This accomplished via a properly signed HTTP GET request to the **riak-cs/users** resource. Any non-admin user request for the user list is rejected and a 403 Forbidden error is returned. This request does not properly work with s3cmd, but can be performed using a less dogmatic tool such as s3-curl.

<div class="info"><div class="title">Note</div>
  You must modify the <code>@endpoints</code> variable in the
  <code>s3curl.pl</code> script to include your Riak CS hostname so that the
  following example will return the list of users.
</div>

A sample URL for a user listing request looks like this:

    GET http://data.example.com/riak-cs/users -

An example using s3-curl that assumes properly specified credentials for the admin user in the .s3curl configuration file with an id of admin is as follows:

    s3curl --id admin -- http://data.mystorage.me/riak-cs/users

By default the listing of all users includes accounts that are both enabled and disabled. The list can be filtered to only include enabled or disabled accounts by using the status query parameter with a value of enabled or disabled respectively.

## Enabling and Disabling a User Account
A user may use a PUT to **/riak-cs/user** to disabled their account. The put must include a document with a status field whose value is disabled. JSON or XML formats are supported for this document. Samples of each are shown below. The Content-Type header should also be set appropriately. The admin user may also disable or re-enable a user's account via a PUT to **/riak-cs/user/<user-key-id>**. Users may not re-enable their own account once it is disabled.

Sample JSON status update document:

```json
{
  "status" : "enabled"
}
```

Sample XML status update document:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<UserUpdate>
<Status>disabled</Status>
</UserUpdate>
```

## Issuing New User Credentials
The key_secret for a user account can be reissued by a PUT to **/riak-cs/user** with the appropriate JSON or XML document. For admin users the PUT would be to **/riak-cs/user/<key-id>**.

The documents should resemble the following examples.

JSON example:

```json
{"new_key_secret":true}
```

XML example:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<UserUpdate>
  <NewKeySecret>true</NewKeySecret>
</UserUpdate>
```

<div class="note">
<div class="title">Note</div>
The `new_key_secret` field may be combined with other user update fields in the same request. Currently, the only other supported field is status, but more may be added in the future. Unsupported fields are ignored.
</div>
