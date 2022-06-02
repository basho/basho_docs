---
title: "Mapping From S3 API to Riak CS internal API"
description: ""
project: "riak_cs"
project_version: "2.1.2"
toc: true
aliases:
  - /riakcs/2.1.2/references/apis/storage/s3/Mapping-From-S3-API-to-Riak-CS-internal-API
  - /riak/cs/2.1.2/references/apis/storage/s3/Mapping-From-S3-API-to-Riak-CS-internal-API
  - /riak/cs/latest/references/apis/storage/s3/mapping-from-s3-api-to-riak-cs-internal-api/
---

## Overview

This document is to outline a mapping of S3 API URLs to their
rewritten format that is processed by Webmachine.

## URL Mapping

### Service Operations

* `GET Service`
    * `GET /` -> `GET /buckets`

### Bucket Operations

*Note* Common method to specify bucket is to prefix bucket name to `Host` header value

* `GET Bucket`
    * `GET /` or `GET /<bucket>` -> `GET /buckets/<bucket>/objects`
* `HEAD Bucket`
    * `HEAD /` or `HEAD /<bucket>` -> `HEAD /buckets/<bucket>`
* `PUT Bucket`
    * `PUT /` or `PUT /<bucket>` -> `PUT /buckets/<bucket>`
* `DELETE Bucket`
    * `DELETE /` or `DELETE /<bucket>` -> `DELETE /buckets/<bucket>`
* `GET Bucket acl`
    * `GET /?acl` -> `GET /buckets/<bucket>/acl`
* `PUT Bucket acl`
    * `PUT /?acl` -> `PUT /buckets/<bucket>/acl`
* `GET Bucket location`
    * `GET /?location`-> `GET /buckets/<bucket>/location`
* `PUT Bucket location`
    * `PUT /?location`-> `PUT /buckets/<bucket>/location`
* `GET Bucket versioning`
    * `GET /?versioning`-> `GET /buckets/<bucket>/versioning`
* `PUT Bucket versioning`
    * `PUT /?versioning`-> `PUT /buckets/<bucket>/versioning`
* `GET Bucket policy`
    * `GET /?policy`-> `GET /buckets/<bucket>/policy`
* `PUT Bucket policy`
    * `PUT /?policy`-> `PUT /buckets/<bucket>/policy`
* `DELETE Bucket policy`
    * `DELETE /?policy`-> `DELETE /buckets/<bucket>/policy`
* `List Multipart Uploads`
    * `GET /?uploads` -> `GET /buckets/<bucket>/uploads`
* `Delete Multiple Objects` (This is listed in the S3 docs as an object operation, but it fits better here)
    * `POST /?delete` -> `POST /buckets/<bucket>/delete`

### Object Operations

*Note* Common method to specify bucket is to prefix bucket name to `Host` header value

* `GET Object`
    * `GET /<object>` -> `GET /buckets/<bucket>/objects/<object>`
* `HEAD Object`
    * `HEAD /<object>` -> `HEAD /buckets/<bucket>/objects/<object>`
* `PUT Object`
    * `PUT /<object>` -> `PUT /buckets/<bucket>/objects/<object>`
* `DELETE Object`
    * `DELETE /<object>` -> `DELETE /buckets/<bucket>/objects/<object>`
* `GET Object acl`
    * `GET /<object>?acl` -> `GET /buckets/<bucket>/objects/<object>/acl`
* `PUT Object acl`
    * `PUT /<object>` -> `PUT /buckets/<bucket>/objects/<object>/acl`
* `Initiate Multipart Upload`
    * `POST /<object>?uploads` -> `POST /buckets/<bucket>/objects/<object>/uploads`
* `Upload Part`
    * `PUT /<object>?partNumber=<part_num>&uploadId=<upload_id>` -> `PUT /buckets/<bucket>/objects/<object>/uploads/<upload_id>?partNumber=<part_num>`
* `Complete Multipart Upload`
    * `POST /<object>?uploads` -> `POST /buckets/<bucket>/objects/<object>/uploads`
* `Upload Part`
    * `DELETE /<object>&uploadId=<upload_id>` -> `DELETE /buckets/<bucket>/objects/<object>/uploads/<upload_id>`
* `List Parts`
    * `GET /<object>?uploadId=<upload_id>` -> `GET /buckets/<bucket>/objects/<object>/uploads/<upload_id>`
