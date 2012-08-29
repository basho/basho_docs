A key feature of Riak KV is the pluggable storage backends. These allow the
ability to choose a low-level storage engine that suits specific operational
needs. For example, if one needs maximum throughput coupled with data
persistence and has a bounded keyspace, Bitcask is a good choice. On the other hand, if one needs to store a large number of keys, then LevelDB would be a better backend recommendation.

As of Riak version 1.2, four backends are supported:

- [[Bitcask]]
- [[LevelDB]]
- [[Memory]]
- [[Multi]]

Riak supports the use of custom storage backends as well. See the storage [[Backend API|Backend-API]] for more details.