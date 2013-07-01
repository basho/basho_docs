---
title: RiakCS OpenStack Storage API
project: riakcs
version: 1.4.0+
document: api
toc: true
index: true
audience: advanced
keywords: [api, http, openstack]
---


The OpenStack storage API (*v1*) provides a convenient way to integrate RiakCS for use as object storage in an OpenStack deployment.

## API Feature Comparison

The following table describes the support status for current OpenStack Object Storage API features.

Feature | Status | Remark
--------|--------|--------
List Containers (lists all buckets for authenticated user) | <abbr title="Supported" class="supported">✓</abbr> | |
Get Account Metadata | Coming Soon | Planned for future release |
Create or Update Account Metadata | Coming Soon | Planned for future release |
Delete Account Metadata | Coming Soon | Planned for future release |
List Objects | <abbr title="Supported" class="supported">✓</abbr> | |
Create Container | <abbr title="Supported" class="supported">✓</abbr> | |
Delete Container | <abbr title="Supported" class="supported">✓</abbr> | |
Create or Update Container Metadata | <abbr title="Unsupported" class="unsupported">✗</abbr> | |
Delete Container Metadata | <abbr title="Unsupported" class="unsupported">✗</abbr> | |
Create Static Website | <abbr title="Unsupported" class="unsupported">✗</abbr> | |
Get Object | <abbr title="Supported" class="supported">✓</abbr> | |
Create or Update Object | <abbr title="Supported" class="supported">✓</abbr> | |
Create Large Objects | Coming Soon | Planned for future release |
Assigning CORS Headers to Requests | Coming Soon | Planned for future release |
Enabling File Compression with the Content-Encoding Header | <abbr title="Unsupported" class="unsupported">✗</abbr> | |
Enabling Browser Bypass with the Content-Disposition Header | <abbr title="Unsupported" class="unsupported">✗</abbr> | |
Expiring Objects with the X-Delete-After and X-Delete-At Headers | <abbr title="Unsupported" class="unsupported">✗</abbr> | |
Object Versioning | Coming Soon | Planned for future release |
Copy Object | Coming Soon | Planned for future release |
Delete Object | <abbr title="Supported" class="supported">✓</abbr> | |
Get Object Metadata | Coming Soon | Planned for future release |
Update Object Metadata | Coming Soon | Planned for future release |

## Storage Account Services

## Storage Container Services

## Storage Object Services

## Multipart Upload
