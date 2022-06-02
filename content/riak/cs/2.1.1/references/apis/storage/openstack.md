---
title: "Riak CS OpenStack Storage API"
description: ""
menu:
  riak_cs-2.1.1:
    name: "OpenStack API"
    identifier: "api_openstack"
    weight: 102
    parent: "api"
project: "riak_cs"
project_version: "2.1.1"
aliases:
  - /riakcs/2.1.1/references/apis/storage/openstack/
  - /riak/cs/2.1.1/references/apis/storage/openstack/
---

The OpenStack storage API (*v1*) provides a convenient way to integrate Riak CS for use as an object storage system in conjunction with an OpenStack deployment.

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

* [List Containers]({{<baseurl>}}riak/cs/2.1.1/references/apis/storage/openstack/list-containers) --- Lists the containers owned by an account

## Storage Container Services

* [List Objects]({{<baseurl>}}riak/cs/2.1.1/references/apis/storage/openstack/list-objects) --- Lists the objects in a container
* [Create Container]({{<baseurl>}}riak/cs/2.1.1/references/apis/storage/openstack/create-container) --- Creates a new container
* [Delete Container]({{<baseurl>}}riak/cs/2.1.1/references/apis/storage/openstack/delete-container) --- Deletes a container

## Storage Object Services

* [Get Object]({{<baseurl>}}riak/cs/2.1.1/references/apis/storage/openstack/get-object) --- Retrieves an object
* [Create or Update Object]({{<baseurl>}}riak/cs/2.1.1/references/apis/storage/openstack/create-object) --- Write an object in a container
* [Delete Object]({{<baseurl>}}riak/cs/2.1.1/references/apis/storage/openstack/delete-object) --- Delete an object from a container
