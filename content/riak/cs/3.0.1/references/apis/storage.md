---
title: "Riak CS Storage API"
description: ""
menu:
  riak_cs-3.0.1:
    name: "Riak CS Storage API"
    identifier: "api_storage"
    weight: 100
    parent: "api"
project: "riak_cs"
project_version: "3.0.1"
lastmod: 2022-06-10T00:00:00-00:00
sitemap:
  priority: 0.9
aliases:
  - /riakcs/3.0.1/references/apis/storage/
  - /riak/cs/3.0.1/references/apis/storage/
  - /riak/cs/latest/references/apis/storage/
---

The Riak CS storage API is compatible with the Amazon S3 REST API, which
means that any of the operations listed can be executed using any of the
commonly available S3 libraries or tools.

## API Feature Comparison

The following table describes the support status for current Amazon S3
functional features.

Feature | Status | Remark
--------|--------|--------
GET Service (lists all buckets for authenticated user) | <abbr title="Supported" class="supported">✓</abbr> | |
DELETE Bucket | <abbr title="Supported" class="supported">✓</abbr> | |
PUT Bucket | <abbr title="Supported" class="supported">✓</abbr> | |
Bucket Lifecycle | <abbr title="Unsupported" class="unsupported">✗</abbr> | |
Policy (Buckets, Objects) {{1.3.0+}} | <abbr title="Supported" class="supported">✓</abbr> | Supports the "*" principal type and the "Secure Transport" and "IP address" conditions. |
Policy (Buckets, Objects) {{1.3.0-}} | Coming Soon | Planned for future release |
Bucket Website | <abbr title="Unsupported" class="unsupported">✗</abbr> | |
Bucket ACLs (GET, PUT) | <abbr title="Supported" class="supported">✓</abbr> | |
Bucket Location | <abbr title="Unsupported" class="unsupported">✗</abbr> | |
Bucket Notification | <abbr title="Unsupported" class="unsupported">✗</abbr> | |
Bucket Object Versions | <abbr title="Unsupported" class="unsupported">✗</abbr> | |
GET Bucket Info (HEAD) | <abbr title="Supported" class="supported">✓</abbr> | |
Bucket Request Payment | <abbr title="Unsupported" class="unsupported">✗</abbr> | |
PUT Object | <abbr title="Supported" class="supported">✓</abbr> | |
Put Object (Copy) {{1.5.0+}} | <abbr title="Supported" class="supported">✓</abbr> | |
PUT Object (Copy) {{1.3.0-1.5.0}} | <abbr title="Supported" class="supported">✓</abbr> | Support is limited to a 0 byte copy from an object to itself for the purpose of updating metadata. |
PUT Object (Copy) {{1.3.0-}} | Coming Soon | Planned for future release |
DELETE Object {{1.3.0-}} | <abbr title="Supported" class="supported">✓</abbr> | |
DELETE Multiple Objects | <abbr title="Unsupported" class="unsupported">✗</abbr> | Planned for future release |
GET Object {{1.3.0+}} | <abbr title="Supported" class="supported">✓</abbr> | |
GET Object {{1.3.0-}} | <abbr title="Supported" class="supported">✓</abbr> | Range query unimplemented |
Object ACLs (GET, PUT) | <abbr title="Supported" class="supported">✓</abbr> | |
HEAD Object | <abbr title="Supported" class="supported">✓</abbr> | |
POST Object | <abbr title="Unsupported" class="unsupported">✗</abbr> | |
Copy Object | <abbr title="Unsupported" class="unsupported">✗</abbr> | Planned for future release |
Multipart Uploads {{1.3.0+}} | <abbr title="Supported" class="supported">✓</abbr> | UploadPartCopy unimplemented |
Multipart Uploads {{1.3.0-}} | Coming Soon | Planned for future release |

## Service-level Operations

* [GET Service]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/get-service) --- Returns a list of all buckets owned by the user who sent the request

## Bucket-level Operations

* [GET Bucket]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/get-bucket) --- Returns a list of the objects
  within a bucket
* [GET Bucket ACL]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/get-bucket-acl) --- Returns the [Access Control List (ACL)](http://docs.aws.amazon.com/AmazonS3/latest/dev/ACLOverview.html) associated with a bucket
* [GET Bucket policy]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/get-bucket-policy) --- Gets the policy of a bucket
* [PUT Bucket]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/put-bucket) --- Creates a new bucket
* [PUT Bucket ACL]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/put-bucket-acl) --- Sets the ACL permissions
  for a bucket
* [PUT Bucket policy]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/put-bucket-policy) --- Sets the policy for a bucket
* [DELETE Bucket]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/delete-bucket) --- Deletes a bucket
* [DELETE Bucket policy]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/delete-bucket-policy) --- Deletes the policy of a bucket

## Object-level Operations

* [GET Object]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/get-object) --- Retrieves an object
* [GET Object ACL]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/get-object-acl) --- Returns the ACLs associated with an object
* [PUT Object]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/put-object) --- Stores an object to a bucket
* [PUT Object (Copy)]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/put-object-copy) --- Creates a copy of an object
* [PUT Object ACL]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/put-object-acl) --- Sets the ACLs associated with an object
* [HEAD Object]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/head-object) --- Retrieves object metadata (not the full content of the object)
* [DELETE Object]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/delete-object) --- Deletes an object

## Multipart Upload

Multipart upload allows you to upload a single object as a set of parts.
Object parts can be uploaded independently and in any order. After all
parts are uploaded, Riak CS assembles an object out of the parts. When
your object size reaches 100MB, you should consider using multipart
uploads instead of uploading the object in a single operation. Read more
about multipart uploads on the [overview page]({{<baseurl>}}riak/cs/3.0.1/cookbooks/multipart-upload-overview).

* [Initiate Multipart Upload]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/initiate-multipart-upload) --- Initiates a multipart upload and returns an upload ID
* [Upload Part]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/upload-part) --- Uploads a part in a multipart upload
* [Complete Multipart Upload]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/complete-multipart-upload) --- Completes a multipart upload and assembles previously uploaded parts
* [Abort Multipart Upload]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/abort-multipart-upload) --- Aborts a multipart upload and eventually frees storage consumed by previously uploaded parts
* [List Parts]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/list-parts) --- Lists the parts that have been uploaded for a specific multipart upload.
* [List Multipart Uploads]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/list-multipart-uploads) --- Lists multipart uploads that have not yet been completed or aborted.

## Common Headers

* [Common Riak CS Request Headers]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/common-request-headers)
* [Common Riak CS Response Headers]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/common-response-headers)

There are two storage API options for Riak CS. The first and most fully
featured is the S3 API. There is also limited but improving support for
the OpenStack Object Storage API.

Riak CS can present different APIs by using the URL-rewriting
capabilities of [Webmachine](https://github.com/basho/webmachine).
Configuring what API Riak CS uses is done by specifying the proper
*rewrite* module in the configuration file. A rewrite module contains a
set of rules for translating requests made using a particular API to
requests in the native Riak CS API. The native API was designed to
facilitate the organization and maintenance of the Riak CS Webmachine
resource modules.

### S3 API

* Module: `riak_cs_s3_rewrite`
* [Documentation](http://docs.aws.amazon.com/AmazonS3/latest/API/APIRest.html)
* [Mapping]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/mapping-from-s3-api-to-riak-cs-internal-api)

### Openstack Object Storage API (v1)

* Module: `riak_cs_oos_rewrite`
* [Documentation](http://docs.openstack.org/api/openstack-object-storage/1.0/content/index.html)
* [Mapping]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/openstack/mapping-from-oos-api-to-riak-cs-internal-api)

Selecting an API is done by adding or changing the `rewrite_module` key in the
Riak CS `riak-cs.conf` file, or the old-style `advanced.config` or `app.config`
files in the `riak_cs` section. For example, to instruct Riak CS to present the
S3 API, ensure the following is contained in your configuration file:

```riakcsconf
rewrite_module = riak_cs_s3_rewrite
```

```advancedconfig
 {riak_cs, [
            %% Other configs
            {rewrite_module, riak_cs_s3_rewrite},
            %% Other configs
           ]}
```

```appconfig
 {riak_cs, [
            %% Other configs
            {rewrite_module, riak_cs_s3_rewrite},
            %% Other configs
           ]}
```

The S3 API is the default that is set in the configuration that is
included when installing a Riak CS package or building from source.

More details for each option can be found by following one of the
following links:

* [S3 API]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/s3/)
* [OpenStack API]({{<baseurl>}}riak/cs/3.0.1/references/apis/storage/openstack/)
