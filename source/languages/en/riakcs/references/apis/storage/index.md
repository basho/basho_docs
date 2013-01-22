---
title: RiakCS Storage API
project: riakcs
version: 1.2.0+
document: api
toc: true
index: true
audience: advanced
keywords: [api, http]
---


The storage API is compatabile with the Amazon S3 REST API which means that any of the operations listed can be executed using any of the commonly available S3 libraries or tools.

## API Feature Comparison

The following table describes the support status for current Amazon S3 functional features.

<table>
    <tr>
        <th WIDTH="50%">Feature</th>
        <th WIDTH="20%">Status</th>
        <th WIDTH="30%">Remark</th>
    </tr>
    <tr>
        <td>GET Service (lists all buckets for authenticated user)</td>
        <td>Supported</td>
        <td></td>
    </tr>
    <tr>
        <td>DELETE Bucket</td>
        <td>Supported</td>
        <td></td>
    </tr>
    <tr>
        <td>PUT Bucket</td>
        <td>Supported</td>
        <td></td>
    </tr>



    <tr>
        <td>Bucket Lifecycle</td>
        <td>Not Supported</td>
        <td></td>
    </tr>
    <tr>
        <td>Policy (Buckets, Objects)</td>
        <td>Coming Soon</td>
        <td>Planned for future release</td>
    </tr>
    <tr>
        <td>Bucket Website</td>
        <td>Not Supported</td>
        <td></td>
    </tr>
    <tr>
        <td>Bucket ACLs (GET, PUT)</td>
        <td>Supported</td>
        <td></td>
    </tr>
    <tr>
        <td>Bucket Location</td>
        <td>Not Supported</td>
        <td></td>
    </tr>
    <tr>
        <td>Bucket Notification</td>
        <td>Not Supported</td>
        <td></td>
    </tr>
    <tr>
        <td>Bucket Object Versions</td>
        <td>Not Supported</td>
        <td></td>
    </tr>
    <tr>
        <td>GET Bucket Info (HEAD)</td>
        <td>Supported</td>
        <td></td>
    </tr>
    <tr>
        <td>Bucket Request Payment</td>
        <td>Not Supported</td>
        <td></td>
    </tr>
    <tr>
        <td>PUT Object</td>
        <td>Supported</td>
        <td></td>
    </tr>
    <tr>
        <td>PUT Object (Copy)</td>
        <td>Coming Soon</td>
        <td>Planned for future release</td>
    </tr>
    <tr>
        <td>DELETE Object</td>
        <td>Supported</td>
        <td></td>
    </tr>
    <tr>
        <td>GET Object</td>
        <td>Supported</td>
        <td>Range query unimplemented</td>
    </tr>
    <tr>
        <td>Object ACLs (GET, PUT)</td>
        <td>Supported</td>
        <td></td>
    </tr>
    <tr>
        <td>HEAD Object</td>
        <td>Supported</td>
        <td></td>
    </tr>
    <tr>
        <td>POST Object</td>
        <td>Not Supported</td>
        <td></td>
    </tr>
    <tr>
        <td>Copy Object</td>
        <td>Coming Soon</td>
        <td>Planned for future release </td>
    </tr>
    <tr>
        <td>Multipart Uploads</td>
        <td>Coming Soon</td>
        <td>Planned for future release </td>
    </tr>
</table>

## Service-level Operations

* [[GET Service|RiakCS GET Service]] - Returns a list of all buckets owned by the user who sent the request

## Bucket-level Operations

* [[GET Bucket|RiakCS GET Bucket]] - Returns a list of the objects within a bucket
* [[GET Bucket ACL|RiakCS GET Bucket ACL]] - Returns the ACL associated with a bucket
* [[PUT Bucket|RiakCS PUT Bucket]] - Creates a new bucket
* [[PUT Bucket ACL|RiakCS PUT Bucket ACL]] - Sets the ACL permissions for a bucket
* [[DELETE Bucket|RiakCS DELETE Bucket]] - Deletes a bucket

## Object-level Operations

* [[GET Object|RiakCS GET Object]]- Retrieves an object
* [[GET Object ACL|RiakCS GET Object ACL]] - Returns the ACLs associated with an object
* [[PUT Object|RiakCS PUT Object]] - Stores an object to a bucket
* [[PUT Object ACL|RiakCS PUT Object ACL]] - Sets the ACLs associated with an object
* [[HEAD Object|RiakCS HEAD Object]] - Retrieves object metadata (not the full content of the object)
* [[DELETE Object|RiakCS DELETE Object]]- Deletes an object

## Common Headers

* [[Common RiakCS Request Headers]]
* [[Common RiakCS Response Headers]]
