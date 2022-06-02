---
title: "Basho Software Release and Maintenance Policy"
description: ""
menu:
  community:
    name: "Release & Maintenance"
    identifier: "community_release_maint"
    weight: 700
    parent: "community_overview"
toc: true
aliases:
  - /riak/2.1.4/community/releaseandmaintenance/
  - /riak/2.1.3/community/releaseandmaintenance/
  - /riak/2.1.1/community/releaseandmaintenance/
  - /riak/kv/2.1.4/community/releaseandmaintenance/
  - /riak/kv/2.1.3/community/releaseandmaintenance/
  - /riak/kv/2.1.1/community/releaseandmaintenance/
---

Basho may change or supplement this Policy in its sole discretion.  Basho will exercise commercially reasonable efforts to notify then-current customers of any such changes provided that, in all cases, the revised Policy is effective upon posting. 

## Definitions

The following words and/or phrases having the meanings ascribed to them below: 

“Feature Release” may include patches and/or the introduction of changes to features or functionality in the software.  Feature Releases are indicated by an increment to the number located at either the “X” or the “Y” location in the pattern X.Y.z. For example, a move from version 1.4.3 to 2.0.0, and a move from 2.0.0 to 2.1.0 indicate Feature Releases. 

“Long Term Support” (or “LTS”) is an extended level of support provided for a limited number of specifically designated Feature Releases, and their associated Patch Releases, as explained below. 

“Patch Release” means a periodic release to address errors or bugs. Patch Releases are indicated by an increment to the number at location “Z” in the pattern x.y.Z. For example, a move from version 2.0.4 to 2.0.5 indicates a Patch Release.

“Software Patch” means a software file, or files, provided to address errors or bugs.  A Software Patch is different from a Patch Release in that a Software Patch is applied to a specific pre-existing installed release, whereas a Patch Release would replace any pre-existing installed release.

“Standard Support and Maintenance” is the level of maintenance made available for all Feature Releases and associated Patch Releases, as explained below.  

## Standard Support and Maintenance Window

Basho’s Standard Support and Maintenance includes the resolution of defects via Patch Releases for a period of time starting at the initial release date of a Feature Release and ending with the next Feature Release.  During that time, Basho will release as many Patch Releases as it determines to be appropriate to support the Feature Release.  Once a new Feature Release has been made, no further Patch Releases will be made available for any prior Feature Release. 

## Long Term Support Window

Basho will promote at least one (1) Feature Release per calendar year to LTS status. From the date a Feature Release is promoted to LTS status, Basho will, in its discretion, provide defect resolution either via Patch Releases or Software Patches for the longer of : i)  2 years, or ii) until the next Feature Release is made available (if longer than 2 years).  After the LTS Window for a specific release has expired, no further Patch Releases or Software Patches will be made available for that Feature Release. 

## Software Support Services

Customers under current licenses and/or current Support and Maintenance Agreements are expected to use the most current release of the Software.    For a period of ninety (90) days following the expiration of a Support Window, as set out above, Basho will continue to respond to Customer trouble tickets for the previous release.  However, Basho will no longer provide Patch Releases or Software Patches (including critical bug fixes) for that release.    

Software Support Services, including submission of trouble tickets and Response Time SLAs, if any, may be further defined in the applicable license/support agreement. 
