---
title: "Riak CS Compared To Atmos"
description: ""
menu:
  riak_cs-2.1.2:
    name: "Riak CS Compared To Atmos"
    identifier: "compare_atmos"
    weight: 101
    parent: "theory_comparisons"
project: "riak_cs"
project_version: "2.1.2"
aliases:
  - /riakcs/2.1.2/references/appendices/comparisons/Riak-Compared-to-Atmos/
  - /riak/cs/2.1.2/references/appendices/comparisons/Riak-Compared-to-Atmos/
  - /riak/cs/latest/references/appendices/comparisons/atmos/
---

The purpose of this comparison is not to serve as an introduction to Riak CS and EMC Atmos, or their commonalities, but rather to enumerate interesting differences between the two systems. This document is intended for those who already have a basic understanding of both systems.

If you feel that this comparison is unfair to either of these technologies, please [submit an issue on GitHub](https://github.com/basho/basho_docs/issues/new) or send an email to **docs@basho.com**.

## Feature/Capability Comparison

The table below gives a high-level comparison of Riak CS and Atmos features and capabilities. For low-level details, please refer to the Riak CS and [Atmos docs](https://community.emc.com/community/edn/atmos).

<table>
    <tr>
        <th WIDTH="15%">Feature/Capability</th>
        <th WIDTH="42%">Riak</th>
        <th WIDTH="43%">Atmos</th>
    </tr>
    <tr>
        <td>Interfaces</td>
        <td>Riak CS offers an S3-compatible interface that can be used with existing S3 clients and libraries. 
    </td>
        <td>Atmos offers a REST and SOAP API, an S3-compatible API, and an Atmos SDK as well as interfaces to traditional storage solutions, including NFS/CIF and CAS.
    </td>
    </tr>
    <tr>
        <td>Availability and Architecture for Reads/Writes</td>
        <td>On write, Riak CS breaks large objects into blocks. Riak CS distributes data across physical machines using consistent hashing and replicates objects a default of 3 times in the underlying Riak storage system. A manifest is maintained for each object that points to which blocks comprise the object. The manifest is used to retrieve all blocks and present them to a client on read. <br />
        Riak CS is a masterless system in that any node can receive and route client requests, making it highly resilient to failure conditions like network partition and hardware failure. Riak uses a request serializer for globally unique entities like users and buckets. This request serializer runs on a single node and in the event of failure, a portion of write operations (specifically, creating new buckets and users) will be unavailable until service is restored.<br />
        In Riak, by default, objects (including their manifests) are replicated 3 times in the underlying system. Riak can also be configured to store more replicas in a given site.
      </td>
        <td>EMC Atmos stores objects and their metadata separately. The
        Metadata Service is responsible for storing all of an object's metadata, including policy and user-defined data, and for providing the object layout which is required for both writes and reads to the underlying storage service. On read, the client will connect with a Resource Management Service to talk to a Metadata Location Service, which then locates the correct Metadata Service for the object.<br />  
        The Metadata Location Service, responsible for finding a local Metadata Service on read, is deployed on two nodes of the first rack in an EMC Atmos implementation. The Metadata Service itself is a master/slave system with a primary and secondary node. The use of a master/slave architecture for metadata services that are required for reads and writes may compromise availability in the event of hardware failure or network partition. Additionally, Atmos stores only two copies of the metadata for an object at a site, which may also cause availability problems in certain failure conditions.
        </td>
    </tr>
    <tr>
        <td>Users and Multitenancy</td>
        <td>Riak exposes multitenancy using S3 conventions for user provisioning and management. Administrators can create users which are then able to authenticate, create buckets, upload and download data, retrieve account information and other user privileges.
        </td>
        <td>EMC Atmos implements a more complex tenant scheme. Atmos recommends implementing 1-2 tenants in a system and using multiple sub-tenants underneath each tenant. The number of tenants is limited to the number of physical nodes, as front-end nodes are assigned to a specific tenant for client access. Configuring tenants and subtenants may be operationally complex, while assigning specific tenants to specific front-end nodes may cause end-user availability issues in the event of node failure.
        </td>
    </tr>
    <tr>
        <td>Hardware</td>
        <td>Riak CS ships as software and can be installed from source or with packages, including Ubuntu and CentOS. There is no hardware lock-in to specific vendors, and Riak CS is designed to be run on commodity hardware so that enterprises can achieve economies of scale.
        </td>
        <td>EMC Atmos can be deployed as a software/hardware bundle on Atmos Hardware or as a virtual edition deployed on a VMware-certified third-party storage system.
        </td>
    </tr>
  <tr>
        <td>Multi-Datacenter Replication</td>
        <td>For multi-site replication in Riak CS, global information for users, bucket information, and manifests are streamed in real time from a primary implementation to a secondary site, so that global state is maintained across locations. Objects can then be replicated in either fullsync or realtime sync mode. The secondary site will replicate the object as in normal operations. Additional datacenters can be added in order to create availability zones or additional data redundancy and locality. Riak CS can also be configured for bi-directional replication. 
    </td>
        <td>In EMC Atmos, object replication to secondary sites is done via synchronous or asynchronous replication configured by policies. These policies are implemented as part of the Metadata Service. A read-only copy of the metadata is maintained at secondary sites.    
   </td>
    </tr>
</table>
