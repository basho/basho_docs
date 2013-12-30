---
title: Riak CS Storage API
project: riakcs
version: 1.2.0+
document: api
toc: true
index: true
audience: advanced
keywords: [api, http]
---

{{#1.3.2-}}

The Riak CS storage API is compatible with the Amazon S3 REST API, which means that any of the operations listed can be executed using any of the commonly available S3 libraries or tools.

## API Feature Comparison

The following table describes the support status for current Amazon S3 functional features.

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
PUT Object (Copy) {{1.3.0+}} | <abbr title="Supported" class="supported">✓</abbr> | Support is limited to a 0 byte copy from an object to itself for the purpose of updating metadata. |
PUT Object (Copy) {{1.3.0-}} | Coming Soon | Planned for future release |
DELETE Object | <abbr title="Supported" class="supported">✓</abbr> | |
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

* [[GET Service|RiakCS GET Service]] &mdash; Returns a list of all buckets owned by the user who sent the request

## Bucket-level Operations

* [[GET Bucket|RiakCS GET Bucket]] &mdash; Returns a list of the objects within a bucket
* [[GET Bucket ACL|RiakCS GET Bucket ACL]] &mdash; Returns the Access Control List (ACL) associated with a bucket
* {{1.3.0+}} [[GET Bucket policy|RiakCS GET Bucket policy]] &mdash; Gets the policy of a bucket
* [[PUT Bucket|RiakCS PUT Bucket]] &mdash; Creates a new bucket
* [[PUT Bucket ACL|RiakCS PUT Bucket ACL]] &mdash; Sets the ACL permissions for a bucket
* {{1.3.0+}} [[PUT Bucket policy|RiakCS PUT Bucket policy]] &mdash; Sets the policy for a bucket
* [[DELETE Bucket|RiakCS DELETE Bucket]] &mdash; Deletes a bucket
* {{1.3.0+}} [[DELETE Bucket policy|RiakCS DELETE Bucket policy]] &mdash; Deletes the policy of a bucket

## Object-level Operations

* [[GET Object|RiakCS GET Object]] &mdash; Retrieves an object
* [[GET Object ACL|RiakCS GET Object ACL]] &mdash; Returns the ACLs associated with an object
* [[PUT Object|RiakCS PUT Object]] &mdash; Stores an object to a bucket
* {{1.3.0+}} [[PUT Object (Copy)|RiakCS PUT Object (Copy)]] &mdash; Creates a copy of an object
* [[PUT Object ACL|RiakCS PUT Object ACL]] &mdash; Sets the ACLs associated with an object
* [[HEAD Object|RiakCS HEAD Object]] &mdash; Retrieves object metadata (not the full content of the object)
* [[DELETE Object|RiakCS DELETE Object]] &mdash; Deletes an object

{{#1.3.0+}}
## Multipart Upload

Multipart upload allows you to upload a single object as a set of parts. Object parts can be uploaded independently and in any order. After all parts are uploaded, Riak CS assembles an object out of the parts. When your object size reaches 100MB, you should consider using multipart uploads instead of uploading the object in a single operation. Read more about multipart uploads on the [[overview|Multipart Upload Overview]] page.

* [[Initiate Multipart Upload|RiakCS Initiate Multipart Upload]] &mdash; Initiates a multipart upload and returns an upload ID
* [[Upload Part|RiakCS Upload Part]] &mdash; Uploads a part in a multipart upload
* [[Complete Multipart Upload|RiakCS Complete Multipart Upload]] &mdash; Completes a multipart upload and assembles previously uploaded parts
* [[Abort Multipart Upload|RiakCS Abort Multipart Upload]] &mdash; Aborts a multipart upload and eventually frees storage consumed by previously uploaded parts.
* [[List Parts|RiakCS List Parts]] &mdash; Lists the parts that have been uploaded for a specific multipart upload.
* [[List Multipart Uploads|RiakCS List Multipart Uploads]] &mdash; Lists multipart uploads that have not yet been completed or aborted.
{{/1.3.0+}}

## Common Headers

* [[Common RiakCS Request Headers]]
* [[Common RiakCS Response Headers]]

{{/1.3.2-}}
{{#1.4.0+}}

There are two storage API options for Riak CS. The first and most fully featured is the S3 API. There is also limited but improving support for the OpenStack Object Storage API.

Riak CS can present different APIs by using the URL-rewriting capabilities of [Webmachine](https://github.com/basho/webmachine). Configuring what API Riak CS uses is done by specifying the proper *rewrite* module in the configuration file. A rewrite module contains a set of rules for translating requests made using a particular API to requests in the native Riak CS API. The native API was designed to facilitate the organization and maintenance of the Riak CS Webmachine resource modules.

#### S3 API
* Module: `riak_cs_s3_rewrite`
* [Documentation](http://docs.aws.amazon.com/AmazonS3/latest/API/APIRest.html)
* [[Mapping|Mapping-From-S3-API-to-Riak-CS-internal-API]]

#### Openstack Object Storage API (v1)
* Module: `riak_cs_oos_rewrite`
* [Documentation](http://docs.openstack.org/api/openstack-object-storage/1.0/content/index.html)
* [[Mapping|Mapping-From-OOS-API-to-Riak-CS-internal-API]]

Selecting an API is done by adding or changing the `rewrite_module` key in the Riak CS `app.config` file. For example, to instruct Riak CS to present the S3 API, ensure the following is contained in the `app.config` in the `riak_cs` section:

```erlang
{rewrite_module, riak_cs_s3_rewrite}
```

The S3 API is the default that is set in the configuration that is included when installing a Riak CS package or building from source.

More details for each option can be found by following one of the following links:

* [[S3 API|RiakCS S3 Storage API]]
* [[OpenStack API|RiakCS OpenStack Storage API]]
{{/1.4.0+}}
