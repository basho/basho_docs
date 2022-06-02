---
title_supertext: "Developing with Riak KV"
title: "Frequently Asked Questions"
description: ""
project: "riak_kv"
project_version: "2.2.6"
menu:
  riak_kv-2.2.6:
    name: "Developing FAQ"
    identifier: "developing_faq"
    weight: 108
    parent: "developing"
toc: true
aliases:
  - /riak/2.2.6/community/faqs/developing
  - /riak/kv/2.2.6/community/faqs/developing
---

[[Basho Bench]: {{<baseurl>}}riak/kv/2.2.6/using/performance/benchmarking
[Bitcask]: {{<baseurl>}}riak/kv/2.2.6/setup/planning/backend/bitcask
[Bucket Properties]: {{<baseurl>}}riak/kv/2.2.6/developing/usage
[built-in functions list]: https://github.com/basho/riak_kv/blob/master/priv/mapred_builtins.js
[commit hooks]: {{<baseurl>}}riak/kv/2.2.6/developing/usage/commit-hooks
[Configuration Files]: {{<baseurl>}}riak/kv/2.2.6/configuring/reference
[contrib.basho.com]: https://github.com/basho/riak_function_contrib
[Erlang Riak Client]: {{<baseurl>}}riak/kv/2.2.6/developing/client-libraries
[MapReduce]: {{<baseurl>}}riak/kv/2.2.6/developing/usage/mapreduce
[Memory]: {{<baseurl>}}riak/kv/2.2.6/setup/planning/backend/memory
[Riak CS]: {{<baseurl>}}riak/cs/2.1.1
[System Planning]: {{<baseurl>}}riak/kv/2.2.6/setup/planning/start/#network-configuration-load-balancing
[vector clocks]: {{<baseurl>}}riak/kv/2.2.6/learn/concepts/causal-context#vector-clocks


## General


**Q: How can I automatically expire a key from Riak? I want to regularly purge items from Riak that are older than a certain timestamp, but MapReduce times out on large numbers of items. Can I expire data automatically?**

**A:**
  If you're using [Bitcask], the default storage backend, and you want items to expire at a consistent interval (assuming that they are not updated), set the `expiry_secs` option in `app.config`. Items that persist past this threshold will not be returned on get/fetch operations and will eventually be removed from disk by Bitcask's merging process. For example:

  ```erlang
  {bitcask, [
      {data_root, "data/bitcask"},
      {expiry_secs, 86400} %% Expire after a day
  ]},
  ```

  There is no limit on how large or small the `expiry_secs` setting can be as long as it is greater than 0.

  You can also set auto-expire using the [Memory] storage backend, but it will be limited by RAM.


---

**Q: Is there better performance for a few objects in many buckets, or many objects in a few buckets?**


**A:**
  Generally speaking, it does not matter if you have many buckets with a small number of objects or a small number of buckets with a large number of objects. Buckets that use the cluster's default bucket properties (which can be set in your `app.config` file) are essentially free.

  If the buckets require different bucket properties, however, those custom properties incur some cost because changes in bucket properties must be gossiped around the cluster. If you create many, many buckets with custom properties, the cost can indeed have an impact.


---

**Q: Can I list buckets or keys in production?**


**A:**
  It is *not* recommended that you list the buckets in production because it is a costly operation irrespective of the bucket's size.

  Buckets are not like directories on a file system or tables in a database; rather, they are logical properties applied to objects, i.e. there is no *actual* separation of objects by bucket.

  A filter must be applied to all of the objects in the system in order to find those residing in a particular bucket. Buckets are intended for configuration purposes (e.g. replication properties) rather than for general queries.

  To keep track of groups of objects there are several options with various trade-offs: secondary indexes, search, or a list using links.


---

**Q: Why do secondary indexes (2i) return inconsistent results after using `force-remove` to drop a node from the cluster?**


**A:**
  The Riak key/value store distributes values across all of the partitions in the ring. In order to minimize synchronization issues with secondary indexes, Riak stores index information in the same partition as the data values. 

   When a node fails or is taken out of the cluster without using riak-admin leave, all of the data held by that node is lost to the cluster. This leaves N - 1 consistent replicas of the data. If `riak-admin force-remove` is used to remove the downed node, the remaining clusters will claim the partitions the failed node previously held. The data in the newly claimed vnodes will be made consistent one key at a time through the read-repair mechanism as each key is accessed, or through Active Anti-entropy (AAE) if enabled.

  As a simplistic example, consider this hypothetical cluster:  

  * 5 nodes (nodes A-E)  
  * ring size = 16  
  * `n_val` = 3.

  For this example, I am using simple small integers instead of the actual 160-bit partition index values for the sake of simplicity. The partitions are assigned to the nodes as follows:  

```    
A: 0-5-10-15
B: 1-6-11
C: 2-7-12
D: 3-8-12
E: 4-9-14
```
  When a value is stored in Riak, the `{bucket, key}` is hashed to determine its first primary partition, and the value is stored in that partition and the next `n_val` - 1 partitions in the ring.
  A preflist consists of the vnode which owns the key, and the next `n_val` vnodes in the ring, in order. In this scenario there are 16 preflists:

  <table border="1">
  <tr><td>0-1-2</td><td>1-2-3</td><td>2-3-4</td><td>3-4-5</td></tr>
  <tr><td>4-5-6</td><td>5-6-7</td><td>6-7-8</td><td>7-8-9</td></tr>
  <tr><td>8-9-10</td><td>9-10-11</td><td>10-11-12</td><td>11-12-13</td></tr>
  <tr><td>12-13-14</td><td>13-14-15</td><td>14-15-0</td><td>15-0-1</td></tr>
  </table>

  Index information for each partition is co-located with the value data.  In order to get a full result set for a secondary index query, Riak will need to consult a "covering set" of vnodes that includes at least one member of each preflist. This will require a minimum of 1/`n_val` of the vnodes, rounded up, in this case 6. There are 56 possible covering sets consisting of 6 vnodes:

  <table border="1">
  <tr><td>0-1-4-7-10-13</td><td>0-2-4-7-10-13</td><td>0-2-5-7-10-13</td><td>0-2-5-8-10-13</td></tr>
  <tr><td>0-2-5-8-11-13</td><td>0-2-5-8-11-14</td><td>0-3-4-7-10-13</td><td>0-3-5-7-10-13</td></tr>
  <tr><td>0-3-5-8-10-13</td><td>0-3-5-8-11-13</td><td>0-3-5-8-11-14</td><td>0-3-6-7-10-13</td></tr>
  <tr><td>0-3-6-8-10-13</td><td>0-3-6-8-11-13</td><td>0-3-6-8-11-14</td><td>0-3-6-9-10-13</td></tr>
  <tr><td>0-3-6-9-11-13</td><td>0-3-6-9-11-14</td><td>0-3-6-9-12-13</td><td>0-3-6-9-12-14</td></tr>
  <tr><td>0-3-6-9-12-15</td><td>1-2-5-8-11-14</td><td>1-3-5-8-11-14</td><td>1-3-6-8-11-14</td></tr>
  <tr><td>1-3-6-9-11-14</td><td>1-3-6-9-12-14</td><td>1-3-6-9-12-15</td><td>1-4-5-8-11-14</td></tr>
  <tr><td>1-4-6-8-11-14</td><td>1-4-6-9-11-14</td><td>1-4-6-9-12-14</td><td>1-4-6-9-12-15</td></tr>
  <tr><td>1-4-7-8-11-14</td><td>1-4-7-9-11-14</td><td>1-4-7-9-12-14</td><td>1-4-7-9-12-15</td></tr>
  <tr><td>1-4-7-10-11-14</td><td>1-4-7-10-12-14</td><td>1-4-7-10-12-15</td><td>1-4-7-10-13-14</td></tr>
  <tr><td>1-4-7-10-13-15</td><td>2-3-6-9-12-15</td><td>2-4-6-9-12-15</td><td>2-4-7-9-12-15</td></tr>
  <tr><td>2-4-7-10-12-15</td><td>2-4-7-10-13-15</td><td>2-5-6-9-12-15</td><td>2-5-7-9-12-15</td></tr>
  <tr><td>2-5-7-10-12-15</td><td>2-5-7-10-13-15</td><td>2-5-8-9-12-15</td><td>2-5-8-10-12-15</td></tr>
  <tr><td>2-5-8-10-13-15</td><td>2-5-8-11-12-15</td><td>2-5-8-11-13-15</td><td>2-5-8-11-14-15</td></tr>
  </table>

  When a node fails or is marked down, its vnodes will not be considered for coverage queries. Fallback vnodes will be created on other nodes so that PUT and GET operations can be handled, but only primary vnodes are considered for secondary index coverage queries. If a covering set cannot be found, `{error, insufficient_vnodes}` will be returned. Thus, the reply will either be complete or an error.
  
  When a node is `force-remove`d, it is dropped from the cluster without transferring its data to other nodes, and the remaining nodes then claim the unowned partitions, designating new primary replicas to comply with `n_val`, but they do not immediately populate the data or indexes. 
  
  Read repair, triggered by GETs or PUTs on the individual keys, and/or Active Anti-Entropy, will eventually repopulate the data, restoring consistency.  
  A GET operation for a key will request the data from all of the vnodes in its preflist, by default waiting for over half of them to respond. This results in consistent responses to get even when one of the vnodes in the preflist has been compromised.  
  
  Secondary index queries, however, consult a covering set which may include only 1 member of the preflist.  If that vnode is empty due to the `force-remove` operation, none of the keys from that preflist will be returned.

  Continuing with the above example, consider if node C is force removed. 
  This is one possible configuration after rebalancing:
  
```
A: 0-5-10-15
B: 1-6-11-2*
D: 3-8-12-7*
E: 4-9-14-12*
```

  Vnodes 2,7, and 12 (marked with `*`) are newly created primary partitions that do not contain any values or index information.

  In this new 4-node configuration any coverage set that includes vnodes 2,7, or 12 will return incomplete results until consistency is restored via read-repair or AAE, because not all vnodes will contain the data that would otherwise be present.


  So making a couple of assumptions for demonstration purposes:  
  
  1. The keys `a`, `b`, and `c` are stored in the following preflists:
  
      ```
  a - 0-1-2
  b - 6-7-8
  c - 10-11-12
      ```

  2. The cluster is not loaded, so no GET/PUT or other coverage queries are being performed
  
  3. AAE is not enabled

  The coordinating node (the one that receives the request from the client) will attempt to spread the load by not using the same partitions for successive coverage queries.  

  The results from secondary index queries that should return all 3 keys will vary depending on the nodes chosen for the coverage set. Of the 56 possible covering sets ...

  * 20 sets (35.7% of sets) will return all 3 keys `{a,b,c}`:
    <table border="1">
    <tr><td>0-2-5-8-10-13</td><td>0-2-5-8-11-13</td><td>0-2-5-8-11-14</td><td>0-3-5-8-10-13</td></tr>
    <tr><td>0-3-5-8-11-13</td><td>0-3-5-8-11-14</td><td>0-3-6-8-10-13</td><td>0-3-6-8-11-13</td></tr>
    <tr><td>0-3-6-8-11-14</td><td>0-3-6-9-10-13</td><td>0-3-6-9-11-13</td><td>0-3-6-9-11-14</td></tr>
    <tr><td>1-2-5-8-11-14</td><td>1-3-5-8-11-14</td><td>1-3-6-8-11-14</td><td>1-3-6-9-11-14</td></tr>
    <tr><td>1-4-5-8-11-14</td><td>1-4-6-8-11-14</td><td>1-4-6-9-11-14</td><td>1-4-7-8-11-14</td></tr>
    </table>

  * 24 sets (42.9%) will return 2 of the 3 keys:
    <table border="1">
    <tr><td colspan=4>`{a,b}` (7 sets)</td></tr>
    <tr><td>0-3-6-9-12-13</td><td>0-3-6-9-12-14</td><td>0-3-6-9-12-15</td><td>1-3-6-9-12-14</td></tr>
    <tr><td>1-3-6-9-12-15</td><td>1-4-6-9-12-14</td><td>1-4-6-9-12-15</td><td>&nbsp;</td></tr>
    <tr><td colspan=4>`{a,c}` (12 sets)</td></tr>
    <tr><td>0-1-4-7-10-13</td><td>0-2-4-7-10-13</td><td>0-2-5-7-10-13</td><td>0-3-4-7-10-13</td></tr>
    <tr><td>0-3-5-7-10-13</td><td>0-3-6-7-10-13</td><td>1-4-7-10-11-14</td><td>1-4-7-10-12-14</td></tr>
    <tr><td>1-4-7-10-12-15</td><td>1-4-7-10-13-14</td><td>1-4-7-10-13-15</td><td>1-4-7-9-11-14</td></tr>
    <tr><td colspan=4>`{b,c}` (5 sets)</td></tr>
    <tr><td>2-5-8-10-12-15</td><td>2-5-8-10-13-15</td><td>2-5-8-11-12-15</td><td>2-5-8-11-14-15</td></tr>
    <tr><td>2-5-8-11-13-15</td><td colspan="3">&nbsp;</td></tr>
    </table>

  * 10 sets (17.8%) will return only one of the 3 keys:
    <table border="1">
    <tr><td colspan=4>`{a}` (2 sets)</td></tr>
    <tr><td>1-4-7-9-12-14</td><td>1-4-7-9-12-15</td><td colspan="2">&nbsp;</td></tr>
    <tr><td colspan=4>`{b}` (4 sets)</td></tr>
    <tr><td>2-3-6-9-12-15</td><td>2-4-6-9-12-15</td><td>2-5-6-9-12-15</td><td>2-5-8-9-12-15</td></tr>
    <tr><td colspan=4>`{c}` (4 sets)</td></tr>
    <tr><td>2-4-7-10-12-15</td><td>2-4-7-10-13-15</td><td>2-5-7-10-12-15</td><td>2-5-7-10-13-15</td></tr>
    </table>

  * 2 sets (3.6%) will not return any of the 3 keys
    <table border="1">
    <tr><td>2-4-7-9-12-15</td><td>2-5-7-9-12-15</td></tr>
    </table>

---

**Q: How do I load 3rd-party Javascript libraries for use in MapReduce functions?**
  Is it possible to load third-party javascript libraries (like Underscore.js) to be available in MapReduce functions?


**A:**
  Yes. For JavaScript, this can be done in `app.config` in `js_source_dir` in the `riak_kv` settings:

  ```erlang
  {js_source_dir, "/etc/riak/javascript"},
  ```

  For Erlang code (please note that you need compiled modules in this dir), set `add_paths` in the `riak_kv` section:

  ```erlang
  {add_paths, "/etc/riak/erlang"},
  ```

  You can find more details in the [Configuration Files] document.

---

**Q: Is it possible to use key filtering to just return a list of keys that match a particular pattern without performing a MapReduce on it?**
  When running a MapReduce query, a map phase results in Riak pulling an object off of disk. Some queries are only interested in the keys of an object and not the value. Is it possible to run a MapReduce query that does not have to pull objects off of disk?


**A:**
  Yes. Specifying a MapReduce query with just a reduce phase will avoid any need to pull data off of disk. To return the results of a key filtering query you can do the following:

  ```json
  {
    "inputs": {
      "bucket": "test",
      "key_filters": [
        ["ends_with","1"]
      ]
    },
    "query": [
      {
        "reduce": {
          "language": "erlang",
          "module": "riak_kv_mapreduce",
          "function": "reduce_identity"
        }
      }
    ]
  }
  ```

  There is also a reduce function for counting inputs. This function can be used to count keys in a bucket without reading objects from disk:

  ```json
  {
    "inputs": {
      "bucket": "test",
      "key_filters": [
        [
          "ends_with","1"
        ]
      ]
    },
    "query": [
      {
        "reduce": { 
          "language": "erlang",
          "module": "riak_kv_mapreduce",
          "function": "reduce_count_inputs"
        }
      }
    ]
  }
  ```


---

**Q: How can I observe object sizes and sibling counts?**


**A:**
  `riak-admin status` will return the following stats, which give the mean and median along with the 95th, 99th, and 100th percentile object size and sibling counts.

  ```
  node_get_fsm_siblings_mean : 0
  node_get_fsm_siblings_median : 0
  node_get_fsm_siblings_95 : 0
  node_get_fsm_siblings_99 : 0
  node_get_fsm_siblings_100 : 0
  node_get_fsm_objsize_mean : 0
  node_get_fsm_objsize_median : 0
  node_get_fsm_objsize_95 : 0
  node_get_fsm_objsize_99 : 0
  node_get_fsm_objsize_100 : 0
  ```


---

**Q: A node left the cluster before handing off all data. How can I resolve this?**


**A:**
  In versions of Riak earlier than Riak 1.0, there are cases in which a node that is leaving the cluster will shut down before handing off all of its data. This has been resolved in Riak 1.0.

  If you encounter this issue, you can rely upon the `read-repair` command to restore your lost replicas. Simply send a `HEAD` request for each key in your data set and Riak will restore replicas as needed.

  Alternatively, if the node that left prematurely is still installed/available, you can manually re-initiate handoff using the following sequence. This approach requires entering code directly into the Erlang console of a running Riak node, and is therefore most appropriate for users with a support contract with Basho that can ask for help if anything goes wrong.

  **Manual approach**: Restart the node that prematurely left by using `riak console`. Then copy/paste the following sequence, changing the first line to point to a node still in your cluster. Handoff should then restart, but there may be no visual indicator. Simply leave the node running for awhile. It should eventually hand off all data and then shut down. Verify handoff by once again checking the size of your data directories.

  ```erlang
  ClusterNode = 'riak@127.0.0.1'.

  application:set_env(riak_core, wants_claim_fun, {riak_core_claim, never_wants_claim}).
  {ok, Ring} = rpc:call(ClusterNode, riak_core_ring_manager, get_my_ring, []).
  Ring2 = setelement(2, Ring, node()).
  riak_core_ring_manager:set_my_ring(Ring2).
  riak_core_ring_manager:write_ringfile().
  [gen_server:cast(riak_core_node_watcher, {up, Node, [riak_kv]}) || Node
  ```


---

**Q: Is there a limit on the size of files that can be stored on Riak?**


**A:**
  There isn't a limit on object size, but we suggest you keep it to no more than 1-2MB for performance reasons. Variables such as network speed can directly affect the maximum usable object size for a given cluster. You should use a tool like [Basho Bench] to determine the performance of your cluster with a given object size before moving to production use. Or if your use case demands storing many large objects, you may want to consider the [Riak CS] object storage system, which is designed for precisely that purpose.
  

---

**Q: Does the bucket name impact key storage size?**


**A:**
  The storage per key is 40 bytes plus the key size and bucket name size.

  Example:

  Key size: 15 bytes.
  Bucket Name size: 10 bytes.

  Total size = 40 + 15 + 10 = **65 bytes**.



---

**Q: Are Riak-generated keys unique within a bucket?**


**A:**
  It's not guaranteed, but you are extremely unlikely to get collisions. Riak generates keys using an Erlang-generated unique ID and a timestamp hashed with SHA-1 and base-62 encoded for URL safety.


---

**Q: Where are bucket properties stored?**


**A:**
  The bucket properties for the default bucket type are stored in the *ring* (metadata stored in each node about the cluster).  Rings are gossipped as a single unit, so if possible you should limit your creation of custom buckets under the default bucket type.
  Bucket properties for non-default bucket types are stored in the cluster metadata system.  The cluster metadata system is a more efficient way of replicating this information around a Riak cluster.

  The bucket properties stay in the ring and cluster metadata even if the bucket is empty.

---

**Q: Are Riak keys / buckets case sensitive?**


**A:**
  Yes, they are case sensitive and treated as binaries (byte buffers). Thus, `mykey` is not equal to `MyKey`.


---

**Q: Can I run my own Erlang applications in the same VM as Riak?**


**A:**
  We do not recommend running your application inside the same virtual machine as Riak for several reasons. If they are kept separate, the following will hold:

  1. Your application and Riak will not compete for the same resources and are thus less likely to affect each other's performance and availability.
  2. You will be able to upgrade Riak and your application independently of one another.
  3. When your application or Riak need more capacity, you can scale them separately to meet your production needs.


---

**Q: Is there a simple way to reload an Erlang module for MapReduce across a cluster?**


**A:**
  Assuming that the module is in your code path, you can run `c:nl(ModName)` from the Erlang console .



---

**Q: How do I spread requests across---i.e. load balance---a Riak cluster?**


**A:**
  There are at least two acceptable strategies for load balancing requests across your Riak cluster: **virtual IPs** and **reverse-proxy**.

  For further information see [System Planning].


---

<a name="restart-merges"></a>
**Q: Why does it seem that Bitcask merging is only triggered when a Riak node is restarted?**
  There have been situations where the data directory for a Riak node (e.g. `data/bitcask`) grows continually and does not seem to merge. After restarting the node a series of merges are kicked off and the total size of the data directory shrinks. Why does this happen?


**A:**
  Riak and Bitcask are operating normally. Bitcask's merge behavior is as follows:

  1. List all of the data files in the Bitcask directory; it should be noted that a Bitcask directory exists for every vnode (e.g. `data/bitcask/0`)
  2. Remove the currently active file from the list; the active file is the one being actively written
  3. Lookup file stats for each data file; this includes percent fragmentation and number of dead bytes
  4. If any of the stats exceed the defined triggers, the Bitcask directory is merged

  The default triggers for a Bitcask directory:

  * `{frag_merge_trigger, 60}, % >= 60% fragmentation`
  * `{dead_bytes_merge_trigger, 536870912}, % Dead bytes > 512 MB`

  In the described scenario, merging has not occurred because none of the data files have triggered the merge. After restarting the node, however, the previously active file is now included in the merge trigger analysis and triggers a merge on the Bitcask directory.

  If Riak was never restarted, the merge would eventually happen when writes roll over to a new data file. Bitcask rolls writes over to a new data file once the currently active file has exceeded a certain size (2 GB by default).


---

**Q: When retrieving a list of siblings I am getting the same vtag multiple times.**
  When retrieving a list of siblings via the REST interface, I am seeing the same vtag appear multiple times. Is this normal? I thought vtags were unique. Are they referring to the same sibling?


**A:**
  The vtag is calculated on a `PUT` based on the vclock and is stored as part of the object's metadata.

  It is possible to get siblings with the same vtag during vector clock pruning and read/repair.

  See [vector clocks] for more information.



---

**Q: How should I structure larger data objects?**
  I have a data object that is denormalized, with multiple child data objects, and stored as a nested JSON hash. However, retrieving and storing this object becomes increasingly costly as my application modifies and adds pieces to the object. Would breaking the object into smaller pieces improve performance? What are the tradeoffs?


**A:**
  The factors involved in deciding whether or not to break this large object into multiple pieces are more concerned with conceptual structure than performance, although performance will be affected. Those factors include:

  1. How tightly coupled are the child objects to the parent? That is, are they frequently updated at the same time?
  2. How likely are the objects to be updated at the same time by multiple processes?

  If the parent and child objects are not too tightly coupled (or the children are updated much more frequently), then splitting them along conceptual boundaries will improve performance in your application by decreasing payload size and reducing update conflicts. Generally, you will want to add links to connect the objects for easy fetching and traversal.


---

**Q: Is there any way in Riak to limit access to a user or a group of users?**


**A:**
  Allowing multiple users, also known as multitenancy, is not built into Riak (though it is built into [Riak CS]). Riak has no built-in authentication.

  If you need to restrict access, consider putting an authenticating reverse-proxy server in front of it.


---

**Q: Is there a way to enforce a schema on data in a given bucket?**
  Suppose I'd like to set up a bucket to store data adhering to a particular schema. Is there any way to set this up with Riak? This way, when my application attempts to store data in a particular bucket, it will check with this schema first before storing it. Otherwise, it will produce an error.


**A:**
  Riak does not implement any form of schema validation. A pre-commit hook can be used in this scenario but would need to be written by your development team. You can read more about [commit hooks] in the docs. This document provides two pre-commit hook examples, one in Erlang that restricts objects that are too large and one in Javascript that restricts non-JSON content.


---

**Q: How does the Erlang Riak Client manage node failures?**
  Does the Erlang Riak Client manage its own reconnect logic? What should a client do to maintain the connection or reconnect in case of nodes going down?


**A:**
  The [Erlang Riak Client] gives you several options for how to manage connections. You can set these when starting a `riakc_pb_socket` process or by using the `set_options` function.

  * `queue_if_disconnected` (default: `false`) --- requests will be queued when the connection to the server is lost.
  * `auto_reconnect` (default: `false`) --- if the connection is lost, `riakc_pb_socket` will attempt to reconnect automatically. This is set to `true` if `queue_if_disconnected` is set to `true`.

  If these options are both false, connection errors will be returned to the process-making requests as `{error, Reason}` tuples.


---

**Q: Is there a limiting factor for the number of buckets in a cluster?**


**A:**
  As long as you use the default bucket properties, buckets consume no resources. Each bucket with non-default bucket properties is stored in the gossiped ring state, so the more buckets with custom properties, the more ring data must be handed off to every node.

  More on [Bucket Properties].


---

**Q: Is it possible to configure a single bucket's properties in `app.config`?**


**A:**
  Not a specific bucket, only the defaults. However, you should only need to change them once, since after that the settings will be reflected in the ring state.

  You can read more on `app.config` in [Configuration Files].


---

**Q: Is there a simple command to delete a bucket?**


**A:**
  There is no straightforward command to delete an entire bucket. You must delete all of the key/value objects individually. Thus, the following will not work:

  ```curl
  curl -X DELETE http://your-host:8098/riak/your-bucket
  ```


---

**Q: Can Riak be configured to fail an update instead of generating a conflict?**


**A:**
  No. The closest thing would be to use the `If-None-Match` header, but that is only supported in the HTTP interface and probably won't accomplish what you're trying to do.


---

**Q: How can I limit the number of keys retrieved?**


**A:**
  You'll need to use a [MapReduce] job for this.

  You could also run `keys=stream` and close the connection when you have the designated number. This will not, however, reduce load on the Riak cluster. It will only reduce load on your client.


---

**Q: How is the real hash value for replicas calculated based on the preflist?**


**A:**
  The hash is calculated first and then the next subsequent *N* partitions are chosen for the preflist.


---

**Q: Do client libraries support load balancing/round robin?**


**A:**

  * The Riak Ruby client has failure-aware load balancing. It will round-robin unless there are network errors, in which case other nodes will be preferred.
  * The Java client is strictly round robin, but with retries built in.
  * The Python client also follows round robin without retries.
  * The Erlang client does not support any load balancing.

## MapReduce


**Q: Does the number of keys in a bucket affect the performance of MapReduce?**


**A:**
  Yes. In general, the smaller the number of keys a bucket holds, the faster MapReduce operations will run.


---

**Q: How do I filter out `not_found` from MapReduce results?**
  If I want to filter out the `not_found` in my MapReduce, should I do it in the reduce phase? I have a MapReduce job that returns what I'm looking for, but I want to filter out the `not_found` entries so that I only get a list back with the keys.


**A:**
  There is a built-in function for this that ships with Riak. Check out `Riak.filterNotFound` from the [built-in functions list].


---

**Q: Is it possible to call a reduce function at specific intervals during a map function?**
  When doing the map step on a whole bucket, can I choose how many keys to map before calling the reduce? I am generating a lot of data in memory and it could be reduced if I could call the following reduce step more often.


**A:**
  Not currently. The reduce function is run occasionally as the bucket is processed and MapReduce doesn't wait for the whole map process to finish before running the reduce.


---

**Q: When searching over a bucket using MapReduce, is it recommended to perform the search during the map phase or the reduce phase?**


**A:**
  Aside from the performance considerations of doing a full-bucket [MapReduce], searching is a form of filtering, which should be done in the map phase.


---

**Q: Is it possible to delete data from Riak with a JavaScript MapReduce job?**


**A:**
  This is not currently possible. If you want to delete objects from MapReduce, use an Erlang reduce phase like the one on [contrib.basho.com].


---

**Q: Why does MapReduce return a JSON object on occasion instead of an array?**


**A:**
  `mochijson2` assumes that anything that looks like a proplist---a list of 2-tuples---is turned into a hash:

  ```erlang
  list_to_binary(mochijson2:encode([{a , b}, {foo, bar}])).
  <<"{\"a\":\"b\",\"foo\":\"bar\"}">>
  ```

  JSON has no "tuple" notion. For the time being, a recommended workaround would be to use a list of length-2 lists.
