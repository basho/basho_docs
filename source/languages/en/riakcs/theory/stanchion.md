---
title: Stanchion
project: riakcs
version: 1.5.0+
document: cookbook
audience: intermediate
keywords: [operator, stanchion]
---

Stanchion is an application used by Riak to manage the serialization of
requests, which enables Riak CS to manage [[globally unique
entities|Stanchion#Globally-Unique-Entities]] like users and bucket
names.

Unlike Riak and Riak CS, which both run on multiple nodes in your
cluster, there should be only one running Stanchion instance in your
Riak CS cluster at any time. This makes Stanchion less fault tolerant
than Riak and Riak CS.

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
or modification requests that involve them.
