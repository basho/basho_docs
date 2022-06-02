---
title: "Mapping From OOS API to Riak CS internal API"
description: ""
project: "riak_cs"
project_version: "2.1.2"
aliases:
  - /riakcs/2.1.2/references/apis/storage/openstack/Mapping-From-OOS-API-to-Riak-CS-internal-API
  - /riak/cs/2.1.2/references/apis/storage/openstack/Mapping-From-OOS-API-to-Riak-CS-internal-API
  - /riak/cs/latest/references/apis/storage/openstack/mapping-from-oos-api-to-riak-cs-internal-api/
---

## Overview

This document outlines a mapping of the OpenStack Object Storage (OOS) API (version 1.0) URLs to their rewritten format that is processed by Webmachine and Riak CS.

## URL Mapping

### Storage Account Services

* List Containers
    * `GET /<api_version>/<account>` -> `GET /buckets`
* Retrieve account metadata
    * **TBD**
* Create/Update account metadata
    * **TBD**
* Delete account metadata
    * **TBD**

### Storage Container Services

* List Objects
    * `GET /<api_version>/<account>/<container>` -> `GET /buckets/<bucket>/objects`
* Create Container
    * `PUT /<api_version>/<account>/<container>` -> `PUT /buckets/<bucket>`
* Delete Container
    * `DELETE /<api_version>/<account>/<container>` -> `DELETE /buckets/<bucket>`
* Retrieve Container Metadata
    * **TBD**
* Create/Update Container Metadata
    * **TBD**
* Delete Container Metadata
    * **TBD**

### Storage Object Services

* Retrieve Object
    * `GET /<api_version>/<account>/<container>/<object>` -> `GET /buckets/<bucket>/objects/<object>`
* Create/Update Object
    * `PUT /<api_version>/<account>/<container>/<object>` -> `PUT /buckets/<bucket>/objects/<object>`
* Delete Object
    * `DELETE /<api_version>/<account>/<container>/<object>` -> `DELETE /buckets/<bucket>/objects/<object>`
* Retrieve Object Metadata
    * **TBD**
* Update Object Metadata
    * **TBD**
