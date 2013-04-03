---
title: Multipart Upload Overview
project: riakcs
version: 1.3.0+
document: cookbook
toc: true
index: true
audience: intermediate
keywords: [operator, developer]
---

Multipart upload allows users of Riak CS to upload large objects as a
set of smaller parts. The size of each part may be in the range of 5MB
to 5GB.

There are three phases to a multipart upload: initiation, parts
upload, and completion. Each phase is described in more detail
below. Multipart uploads with Riak CS are designed to behave
in the same manner as multipart uploads to Amazon S3 and the
API is the same.

## Multipart Upload Phases

- **Multipart Upload Initiation**

    Initiation is done by sending a properly formatted multipart upload
    initiation request to Riak CS. If the upload initiation is successful
    the response from Riak CS includes an upload ID.

    This ID is a unique identifier for a particular multipart upload
    and *must* be included with all subsequent requests to Riak CS
    pertaining to this upload. This includes any of the upload
    operations described in the remainder of this document except for
    listing all active multipart uploads.

    Metadata may be attached to an object uploaded using multipart upload
    just like any other object stored in Riak CS. To do so the metadata
    should be included with the multipart upload initiation request.

- **Parts Upload**

    A part upload must include both the upload ID received in response
    to an initiation request and a part number. Part numbers should be
    integers between 1 and 10,000. These numbers identify a part
    within the context of the multipart upload and also specify
    positioning within the final object. Uploading a part with a part
    number that has previously been uploaded results in the previous
    part associated with that part number being overwritten.

    For each part that is uploaded, Riak CS returns an `ETag` header as
    part of the response. Each `ETag` value and the part number it
    corresponds to should be reserved for use in the multipart upload
    completion request.

- **Multipart Upload Completion**

    Once a complete multipart upload request is received, Riak CS
    assembles the object from the uploaded parts. Subsequently, the object
    is presented as a single entity to the user with no difference from
    any other object stored in Riak CS. The uploaded parts are no longer
    individually accessible.

    A complete multipart upload request *must* include the upload ID and a
    list of all part numbers and their corresponding `ETag` values. Riak
    CS returns another `ETag` that identifies the completed object. It
    should be noted that this `ETag` value is not necessarily an MD5 hash
    of the object data and this fact may cause warnings to be issued by
    some client libraries or tools.

## Terminating an upload

- **Abort Multipart Upload**

    A multipart upload request may be aborted prior to sending an
    upload completion request. The storage for all parts that have
    been fully uploaded will be released.

    Part uploads that are in-progress *may not* have their storage
    released, so to ensure all storage is reclaimed, the abort request
    should only be sent after all parts already in progress have
    uploaded.

    Once a multipart upload is aborted, the upload ID is no longer
    valid.

## Listing ongoing uploads

- **Active Multipart Uploads**

    Riak CS can list all of the active multipart uploads for each user
    account. The number of multipart uploads included in the response is
    capped at 1000. If there are more than 1000 active multipart uploads
    for a particular user account, they can be listed by using multiple
    requests.

- **Completed Parts From An Active Upload**

    Riak CS can list the parts that have been successfully uploaded
    for a specific multipart upload. If a multipart upload is
    comprised of more than 1000 parts then the parts must be retrieved
    using multiple parts requests.

    The results of this request are not intended to be used when sending a
    complete multipart upload request. The proper procedure is to record
    the part numbers and the associated `ETag` values returned with part
    upload responses and use that information when completing a multipart
    upload.
