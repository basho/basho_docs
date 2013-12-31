This document shows you how to configure Riak CS to work with the [OpenStack Keystone](http://docs.openstack.org/developer/keystone/) authentication service.

Riak CS can be configured to use either the OpenStack Object Storage API or the S3 API in conjunction with Keystone for authentication.

## Terminology

In a system that uses Keystone for authentication, there are three main entity types to be aware of: `tenants`, `users`, and `roles`.

* **tenant** --- A tenant is a collection entity that can contain a number of users
* **user** --- A user represents an individual that uses the OpenStack system
* **role** --- A role is used to define a link between a user and a tenant and to indicate permissions of the user within that tenant

The OpenStack `tenant_id` maps to a `key_id` to identify a user account in Riak CS. In OpenStack, only users who are assigned an *operator* role for a tenant may perform operations. Other users that belong to a tenant may be granted access using ACLs.

Currently, Riak CS does not support OpenStack ACLs and only permits access to  tenant operators. ACLs will be supported at a later date.

By default, Riak CS recognizes *admin* and *swiftoperator* as valid operator roles, but that list can be configured.

Riak CS does not currently support the use of multiple authentication servers via *reseller prefixes*, but if this turns out to be important based on user feedback, support may be added in the future.

## Configuration

#### API

Set the API using the `rewrite_module` configuration option. To use the S3 API, insert the following:

```erlang
{rewrite_module, riak_cs_s3_rewrite}
```

To use the OpenStack object storage API:

```erlang
{rewrite_module, riak_cs_oos_rewrite}
```

#### Authentication Module

Set the authentication module to specify the Keystone authentication module:

```erlang
{auth_module, riak_cs_keystone_auth}
```

####  Operator Roles

You may optionally override the default list of valid operator roles. The
default roles are `admin` and `swiftoperator`, but others may be used:

```erlang
{os_operator_roles,  [<<"admin">>, <<"swiftoperator">>, <<"cinnamon">>]}
```

**Note**: Each role should be formatted as shown above, with two angle brackets preceding and following each role value.

#### Root Host

Make sure that the value of the `cs_root_host` key in the Riak CS configuration matches the root host used for the object store in the Keystone configuration.

For example, given the following config snippet from a Keystone configuration file, the value for `cs_root_host` should be set to `object.store.host`:

```bash
catalog.RegionOne.object_store.publicURL = http://object.store.host/v1/AUTH_$(tenant_id)s
catalog.RegionOne.object_store.adminURL = http://object.store.host/
catalog.RegionOne.object_store.internalURL = http://object.store.host/v1/AUTH_$(tenant_id)s
```

The entry in the Riak CS config would be as follows:

```erlang
{cs_root_host, "object.store.host"}
```

#### Admin Token

Riak CS needs to know the administration token so that it can successfully validate user tokens with Keystone. If no value for `os_admin_token` is specified, the default value is `ADMIN`. The value can be set by adding the following to the Riak CS configuration file:

```erlang
{os_admin_token, "SNARFSNARFSNARF"}
```

#### Auth URL

Riak CS also needs to know the authentication URL to use to
communicate with Keystone. The default value is
`"http://localhost:5000/v2.0"`. To override this value add the following
to the Riak CS configuration file:

```erlang
{os_auth_url, "http://host.with.the.most.com/5000/v2.0"}
```

#### Keystone Resources

Riak CS needs to be be aware of a few resources to be able to perform authentication with Keystone. These resources are unlikely to need to be changed from their defaults, but that capability is provided in case the need arises.

* Tokens Resources

The default is `"tokens/"`. To override this, add the following to the Riak CS configuration file:

```erlang
{os_tokens_resource, "mytokens/"}
```

* S3 Tokens Resources

This resource is only used when the S3 API is used in conjunction with
Keystone authentication. The default is `"s3tokens/"`. To override this, add the following to the Riak CS configuration file:

```erlang
{os_s3_tokens_resource, "mys3tokens/"}
```

* Users Resources

The default is `"users/"`. To override this, add the following to the Riak CS configuration file:

```erlang
{os_users_resource, "users/"}
```

## Testing

### Keystone Setup

Follow the procedures given [[here|Keystone-Setup]] to set up and run Keystone.

Create a tenant called `test`:
      
```bash
keystone tenant-create --name test
```

1. Using the tenant id of the tenant created in the previous step and
create a user called `test` that is a member of tenant `test`:
    ```
    keystone user-create --name test --pass test --email test@test.com --tenant-id <tenant-id> --enabled true
    ```
1. Create a role called `swiftoperator`:
    ```
    keystone role-create --name swiftoperator
    ```
1. Add the `swiftoperator` role for user `test`:
    ```
    keystone user-role-add --user-id <user-id>  --role-id <role-id>   --tenant-id <tenant-id>
    ```
1. Create ec2 credentials for the user `test`:
    ```
    keystone ec2-credentials-create --user_id <user-id> --tenant_id <tenant-id>
    ```

### Testing Openstack API and Keystone authentication
1. Start Riak, Riak CS, and Stanchion. Make sure the values for
the`rewrite_module` and `auth_module` options in the Riak CS
app.config file are as follows:
    ```
    {rewrite_module, riak_cs_oos_rewrite},
    {auth_module, riak_cs_keystone_auth},
    ```
1. Get an auth token for the `test` user to use in requests to Riak CS:
    ```
    curl -s -d '{"auth": {"tenantName": "test", "passwordCredentials": {"username": "test", "password": "test"}}}' -H 'Content-type: application/json' http://localhost:5000/v2.0/tokens | python -mjson.tool
    ```

    The value of the `id` field of the `token` object in the response
    is used as the value for the `X-Auth-Token` header in all
    subsequent requests to Riak CS. The `publicURL` for the
    `object-store` service listed in the `serviceCatalog` of the
    response is the base URL used for all API requests to Riak CS.

    Now export the token and public URL, for example:

    ```
    export ID=20f1a9e46ebd42a3bdd03e009722eeb8
    export URL=http://localhost:8080/v1/AUTH_8d84a17ac99d49fcb6f35c767dd562db
    ```

1. Create a bucket (S3 bucket == OpenStack container)
    ```
    curl -X PUT -H 'X-Auth-Token: '$ID $URL/bucket1
    ```
1. List the buckets
    ```
    curl -H 'X-Auth-Token: '$ID $URL
    ```
1. Put an object into the bucket
    ```
    curl -X PUT --data 'abcdefghi123456789' -H 'X-Auth-Token: '$ID $URL/bucket1/object1
    ```
1. List the objects in the bucket
    ```
    curl -H 'X-Auth-Token: '$ID $URL/bucket1
    ```
1. Fetch the object from the bucket
    ```
    curl -H 'X-Auth-Token: '$ID $URL/bucket1/object1
    ```
1. Delete the object
    ```
    curl -X DELETE -H 'X-Auth-Token: '$ID $URL/bucket1/object1
    ```
1. Delete the bucket
    ```
    curl -X DELETE -H 'X-Auth-Token: '$ID $URL/bucket1
    ```

### Testing S3 API and Keystone authentication
1. If Riak and Stanchion are not already running, start them now.
1. Edit the Riak CS app.config file and restart Riak CS. The values
for `rewrite_module` and `auth_module` should be be as follows:
    ```
    {rewrite_module, riak_cs_s3_rewrite},
    {auth_module, riak_cs_keystone_auth},
    ```
1. Use the values of `access` and `secret` from the ec2 credentials
created for the `test` user as the `key_id` and `key_secret` for
signing requests. *e.g.* If using `s3cmd` then use these credentials
for the `access_key` and `secret_key` fields of the `.s3cfg` file. The
subsequent examples are done using `s3cmd` since it is a fairly
ubiquitous tool.
1. Create a sample file to upload
    ```
    echo "ilovechickenilovelivermeowmixmeowmixwilldeliver" > upload.txt
    ```
1. Create a bucket (*i.e.* container)
    ```
    s3cmd mb s3://bucket2
    ```
1. List the buckets
    ```
    s3cmd ls
    ```
1. Put an object into the bucket
    ```
    s3cmd put upload.txt s3://bucket2
    ```
1. Fetch the object from the bucket
    ```
    s3cmd get s3://bucket2/upload.txt download.txt
    ```
1. Delete the object
    ```
    s3cmd del s3://bucket2/upload.txt
    ```
1. Delete the bucket
    ```
    s3cmd rb s3://bucket2
    ```
