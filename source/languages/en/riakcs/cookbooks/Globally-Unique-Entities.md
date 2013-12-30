---
title: Globally Unique Entities
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
index: true
audience: intermediate
keywords: [operator, developer]
---

There are two entities that must be globally unique within a Riak CS system.

1. **User identifiers**. Riak CS mandates that each user create an account using an email address as an identifier and takes steps to ensure that an email address has not already been used before accepting a user creation request.

2. **Bucket names**. Bucket names must be unique within
a Riak CS system and any attempts to create a bucket with a name that is already in use are rejected.

The uniqueness of these entities is enforced by serializing any creation or modification requests that involve them. The request serialization application for the Riak CS system is called [Stanchion](https://github.com/basho/stanchion). More specific details on its implementation can be found in its [README](https://github.com/basho/stanchion/blob/master/README.org) file.

Uniqueness is further enforced by mandating that all of the primary vnodes of the underlying Riak cluster that are responsible for the user or bucket being created be available at creation time. One result of this enforcement is that user creation requests and bucket creation or modification---i.e. deletion---requests are not highly available like other Riak CS system operations. 

If the Stanchion application is unavailable or cannot be reached for any reason, the aforementioned user and bucket operations will not be allowed to complete. Additionally, instability in the Riak cluster may lead to user and bucket requests being disallowed. This would be indicated by error messages similar to this in the Stanchion console or error log files:

```log
2013-01-03 05:24:24.028 [warning] <0.110.0>@stanchion_utils:bucket_available:501 Error occurred trying to check if the bucket <<"mybucket">> exists. Reason: <<"{pr_val_unsatisfied,3,2}">>
```

As such, these operations should *only* be used as preparation for
a workflow and *not* actually included as part of a highly available
workflow.
