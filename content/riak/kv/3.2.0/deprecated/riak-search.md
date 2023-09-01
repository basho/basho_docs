---
title: "Riak Search"
title_supertext: "Deprecated:"
project: "riak_kv"
project_version: 3.2.0
keywords: "search schema solr yokozuna"
---

[riak 2.9.10]: {{<baseurl>}}riak/kv/2.9.10/
[config 2.9.10-search]: {{<baseurl>}}riak/kv/2.9.10/configuring/search/

Riak Search (aka Yokozuna) using Solr is deprecated in this version. The last version with support was Riak KV 2.9.10. Check out [Riak KV 2.9.10][riak 2.9.10], and it's [Riak Search config page][config 2.9.10-search].

The recommended method of performing searches in Riak is to use secondary indexes (2i) and map-reduce with regex.
