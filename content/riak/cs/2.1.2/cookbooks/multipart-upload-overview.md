---
title: "Multipart Upload Overview"
description: ""
menu:
  riak_cs-2.1.2:
    name: "Multipart Upload Overview"
    identifier: "theory_multipart_upload"
    weight: 101
    parent: "theory"
project: "riak_cs"
project_version: "2.1.2"
aliases:
  - /riakcs/2.1.2/cookbooks/Multipart-Upload-Overview/
  - /riak/cs/latest/cookbooks/multipart-upload-overview/
---

Multipart upload allows users of Riak CS to do the following:

* upload large objects, potentially multiple terabytes, as a set of
  smaller parts
* pause and resume the upload of a large object
* begin an upload without prior knowledge of the total size of the whole
  object

In general, multipart uploads tend to be more efficient because parts
may be uploaded in parallel. In Riak CS they are designed to both behave
like Amazon S3 multipart uploads and to utilize the same user-facing
API.

{{% note title="Note on file size limit" %}}
The size limit on individual parts of a multipart upload is 5 gigabytes.
{{% /note %}}

There are three phases to a multipart upload: **initiation**, **parts
upload**, and **completion**. Each phase is described in more detail
below.

## Multipart Upload Phases

### Initiation

Initiation is done by sending a properly formatted multipart upload
initiation request to Riak CS. If the upload initiation is successful,
the response from Riak CS includes an upload ID.

This ID is a unique identifier for a particular multipart upload and
*must* be included with all subsequent requests to Riak CS pertaining to
this upload. This includes any of the upload operations described in the
remainder of this document except for listing all active multipart
uploads.

Metadata may be attached to an object uploaded using multipart upload
just like any other object stored in Riak CS. To do so, the metadata
should be included with the multipart upload initiation request.

### Parts Upload

A part upload must include both the upload ID received in response to an
initiation request and a part number. Part numbers should be integers
between 1 and 10,000. These numbers identify a part within the context
of the multipart upload and also specify positioning within the final
object. Uploading a part with a part number that has previously been
uploaded results in the previous part associated with that part number
being overwritten.

For each part that is uploaded, Riak CS returns an `ETag` header as part
of the response. Each `ETag` value and the part number it corresponds to
should be reserved for use in the multipart upload completion request.

The size of each part may be in the range of 5MB to 5GB.

### Completion

Once a complete multipart upload request is received, Riak CS assembles
the object from the uploaded parts. Subsequently, the object is
presented as a single entity to the user with no difference from any
other object stored in Riak CS. The uploaded parts are no longer
individually accessible.

A complete multipart upload request *must* include the upload ID and a
list of all part numbers and their corresponding `ETag` values. Riak CS
returns another `ETag` that identifies the completed object. It should
be noted that this `ETag` value is not necessarily an MD5 hash of the
object data and that this fact may cause warnings to be issued by some
client libraries or tools.

## Terminating an upload

#### Abort Multipart Upload

A multipart upload request may be aborted prior to sending an upload
completion request. The storage for all parts that have been fully
uploaded will be released.

Part uploads that are in-progress *may not* have their storage released,
so the abort request should only be sent after all parts already in
progress have uploaded to ensure that all storage is reclaimed.

Once a multipart upload is aborted, the upload ID is no longer valid.

## Listing uploads

#### Active Multipart Uploads

Riak CS can list all of the active multipart uploads for each user
account. The number of multipart uploads included in the response is
capped at 1000. If there are more than 1000 active multipart uploads for
a particular user account, they can be listed by using multiple
requests.

#### Completed Parts From An Active Upload

Riak CS can list the parts that have been successfully uploaded for a
specific multipart upload. If a multipart upload is comprised of more
than 1000 parts then the parts must be retrieved using multiple parts
requests.

The results of this request are not intended to be used when sending a
complete multipart upload request. The proper procedure is to record the
part numbers and the associated `ETag` values returned with part upload
responses and use that information when completing a multipart upload.

## Storage Calculation

As with [Amazon
S3](http://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html),
once you initiate a multipart upload, Riak CS retains all of the parts
of the upload until it is either completed or aborted. If the upload is
aborted, Riak CS deletes all upload artifacts and they will no longer be

For example, if a user has uploaded a 10 GB object via multipart upload without
completing the request, the object won't appear in the list objects
result but its object size _will_ be included in the user's usage
statistics.
