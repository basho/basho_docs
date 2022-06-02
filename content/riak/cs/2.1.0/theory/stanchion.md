---
title: "Stanchion"
description: ""
menu:
  riak_cs-2.1.0:
    name: "Stanchion"
    identifier: "theory_stanchion"
    weight: 100
    parent: "theory"
project: "riak_cs"
project_version: "2.1.0"
aliases:
  - /riakcs/2.1.0/theory/stanchion/
  - /riak/cs/2.1.0/theory/stanchion/
---

Stanchion is an application used by Riak CS to manage the serialization
of requests, which enables Riak CS to manage [globally unique entities](#globally-unique-entities) like users and bucket names. Serialization in this context means that the entire cluster agrees upon a single value for any globally unique entity at any given time; when that value is changed, the new value must be recognized throughout the entire cluster.

## The Role of Stanchion in a Riak CS Cluster

Unlike Riak and Riak CS, which both run on multiple nodes in your
cluster, there should be only _one_ running Stanchion instance in your
Riak CS cluster at any time. Correspondingly, your Stanchion
installation must be managed and configured separately. For more
information, see the following documents:

* [Configuring Stanchion]({{<baseurl>}}riak/cs/2.1.0/cookbooks/configuration/stanchion)
* [Installing Stanchion]({{<baseurl>}}riak/cs/2.1.0/cookbooks/installing#installing-stanchion-on-a-node)
* [The Stantion Command-line Interface]({{<baseurl>}}riak/cs/2.1.0/cookbooks/command-line-tools#stanchion)

For a more in-depth discussion of implementation details, see the
project's
[README](https://github.com/basho/stanchion/blob/master/README.org) on
GitHub.

## Globally Unique Entities

There are two types of entities that must be globally unique within a
Riak CS system:

1. **User identifiers** --- Riak CS mandates that each user create an
account using an email address as an identifier. Stanchion takes steps
to ensure that an email address has not already been used before
accepting a user creation request.
2. **Bucket names** --- Bucket names must be unique within a Riak CS
system (just as they must be unique in S3 and other systems) and any
attempt to create a bucket with a name that is already in use are
rejected.

The uniqueness of these entities is enforced by serializing any creation
or modification requests that involve them. This process is handled by
Stanchion. What happens under the hood is essentially that Stanchion
mandates that all [vnodes]({{<baseurl>}}riak/kv/2.1.3/learn/glossary#vnode) in the underlying Riak cluster that are responsible for the user or bucket being created must be available at creation time.

One result of this enforcement is that user creation requests and bucket
creation or modification, i.e. deletion, requests are not highly
available like other Riak CS system operations. If the Stanchion
application is unavailable or cannot be reached for whatever reason,
you will not be able to carry out user- and bucket-related operations.
In addition, instability in the Riak cluster may lead to user and bucket
requests being disallowed. If this happens, you will see something like
this in the Stanchion console or error logs:

```log
2013-01-03 05:24:24.028 [warning] <0.110.0>@stanchion_utils:bucket_available:501 Error occurred trying to check if the bucket <<"mybucket">> exists. Reason: <<"{pr_val_unsatisfied,3,2}">>
```

Because of this, user- and bucket-related operations should be used
_only_ as preparation for a workflow and not included as part of a
highly available workflow.
