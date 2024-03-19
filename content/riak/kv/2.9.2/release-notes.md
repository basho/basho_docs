---
title: "Riak KV 2.9.2 Release Notes"
description: ""
project: "riak_kv"
project_version: "2.9.2"
lastmod: 2020-04-08T00:00:00-00:00
sitemap:
  priority: 0.2
menu:
  riak_kv-2.9.2:
    name: "Release Notes"
    identifier: "index_release_notes"
    weight: 101
    parent: index
toc: false
aliases:
  - /riak/2.9.2/community/release-notes
  - /riak/kv/2.9.2/intro-v20
  - /riak/2.9.2/intro-v20
  - /riak/kv/2.9.2/introduction
---

Released Apr 08, 2020.

## Overview

This release includes:

- An extension to the node_confirms feature so that node_confirms can be tracked on GETs as well as PUTs. This is provided so that if an attempt to PUT with a node_confirms value failed, a read can be made which will only succeed if repsonses from sufficient nodes are received. This does not confirm absolutely that the actual returned response is on sufficient nodes, but does confirm nodes are now up so that anti-entropy mechanisms will soon resolve any missing data.

- Support for building leveldb on 32bit platforms.

- Improvements to reduce the cost of journal compaction in leveled when there are large numbers of files containing mainly skeleton key-changes objects. The cost of scoring all of these files could have a notable impact on read loads when spinning HDDs are used (although this could be mitigated by running the journal compaction less frequently, or out of hours). Now an attempt is made to reduce this scoring cost by reading the keys to be scored in order, and scoring keys relatively close together. This will reduce the size of the disk head movements required to complete the scoring process.

- The abilty to switch the configuration of leveled journal compaction to using recalc mode, and hence avoid using skeleton key changes objects altogether. The default remains retain mode, the switch from retain mode to enabling recalc is supported without any data modification (just a restart required). There is though, no safe way other than leaving the node from the cluster (and rejoining) to revert from recalc back to retain. The use of the recalc strategy can be enabled via configuration. The use of recalc mode has outperformed retain in tests, when both running journal compaction jobs, and recovering empty ledgers via journal reloads.

- An improvement to the efficiency of compaction in the leveled LSM-tree based ledger with large numbers of tombstones (or modified index entries), by using a grooming selection strategy 50% of the time when selecting files to merge rather than selecting files at random each time. The grooming selection, will take a sample of files and merge the one with the most tombstones. The use of the grooming strategy is not configurable, and will have no impact until the vast majority of SST files have been re-written under this release.

[Previous Release Notes](#previous-release-notes)

## Previous Release Notes

Please see the KV 2.9.1 release notes [here]({{<baseurl>}}riak/kv/2.9.1/release-notes/), and the KV 2.9.0p5 release notes [here]({{<baseurl>}}riak/kv/2.9.2/release-notes/).
