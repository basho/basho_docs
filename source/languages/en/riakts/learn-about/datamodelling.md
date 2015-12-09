---
title: Modeling Data in Riak TS
project: riakts
version: 1.0.0+
document: guide
toc: true
index: true
audience: intermediate
---

The values of the partition key determines which Vnodes handle its writes and queries. This means that if the *family* and *series* fields are the same for large numbers of writes and the quanta that is specified has a large duration, only n_val Vnodes will process the incoming writes and Riak will not be able to parallelise the workload across CPUs.

Riak queries work best when the data being queried is in the same family, series and quanta because all keys and values are written contiguously on disk. For each family, series and quanta a sub-query is created and run against the Vnodes that data is hashed to. Currently there is a limit of 5 sub-queries for a single query to prevent overload.

This means that there is a trade off to be made when deciding on the quanta size, small quantas are best for writes and large quantas are best for queries.

FIXME we cannot make recommendations on quanta size because we do not yet have numbers on how the system behaves with different quantas and family/series settings.
FIXME the subquery limit should not remain at 5 for Riak TS 1.0