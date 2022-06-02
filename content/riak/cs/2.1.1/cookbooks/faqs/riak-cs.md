---
title: "Riak CS FAQs"
description: ""
menu:
  riak_cs-2.1.1:
    name: "FAQs"
    identifier: "reference_faq"
    weight: 100
    parent: "reference"
project: "riak_cs"
project_version: "2.1.1"
aliases:
  - /riakcs/2.1.1/cookbooks/faqs/riak-cs/
---

Q: What is Riak CS?
A:
  Riak CS is [multi-tenant](http://en.wikipedia.org/wiki/Multitenancy) cloud storage software for public and private clouds. Built on Basho's distributed database [Riak KV]({{<baseurl>}}riak/kv/2.1.3), Riak CS is commercial software designed to provide simple, available, distributed cloud storage at any scale. Riak CS is S3 API compatible and supports per-tenant reporting for billing and metering use cases.

Q: Can users share data?
A:
  Data is private by default. Users can manipulate Access Control Lists (ACLs) to grant access to their buckets or objects to other users---or even to unauthenticated requesters.

  For implementation details, see the [Access Control Lists](http://docs.aws.amazon.com/AmazonS3/latest/dev/ACLOverview.html) documentation.

Q: Is it possible to specify a filesystem where my Riak CS buckets will live?
A: You can specify the location of **all** Riak CS bucket data by changing the settings for Riak's backends to a path on a particular filesystem. If this is your goal, you can configure Riak to suit your environment.

  If you look at our example Riak `advanced.config`/`app.config` backend
  definition from the [Configuring Riak for CS]({{<baseurl>}}riak/cs/2.1.1/cookbooks/configuration/riak-for-cs) section, it looks like this:

  ```advanced.config
  {riak_kv, [
      {add_paths, ["/usr/lib/riak-cs/lib/riak_cs-2.1.1/ebin"]},
      {storage_backend, riak_cs_kv_multi_backend},
      {multi_backend_prefix_list, [{<<"0b:">>, be_blocks}]},
      {multi_backend_default, be_default},
      {multi_backend, [
        {be_default, riak_kv_eleveldb_backend, [
          {total_leveldb_mem_percent, 30},
            {data_root, "/var/lib/riak/leveldb"}
        ]},
          {be_blocks, riak_kv_bitcask_backend, [
            {data_root, "/var/lib/riak/bitcask"}
        ]}
      ]},
      %% Other configs
  ]},
  %% Other sections
  ```

  You need to change the two `data_root` values, substituting `/var/lib/riak` as shown in the above example with the path to the filesystem you prefer. Please note that you should do this before starting Riak for the
  first time; if you do make these changes *after* you've started Riak, the previous data will not be available unless you manually move it to to the new location.

  This will allow you to change the location of all bucket data. Riak CS does not yet currently support specifying per-bucket filesystem granularity.

Q: Does Riak CS encrypt data at rest?
A: No, Riak CS does not currently support encryption of data at rest.

Q: Does Riak CS support compression of objects at rest?
A: By default, the LevelDB backend used by Riak CS relies on [gzip](http://www.gzip.org/) compression for data at higher levels in LevelDB's storage system. Any additional object compression needs to be performed by clients connecting to Riak CS.

Q: Does Riak CS support object search?
A: There is currently no search functionality in Riak CS. Search functionality can be provided using an external application to read and index items stored in Riak CS (provided that they are not encrypted).
