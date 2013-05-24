---
title: Log Data
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [use-cases]
---

## Simple Case

A common use case for Riak is storing large amounts of log data for analysis with Map/Reduce; or as the primary storage for log data with a secondary analytics cluster used to perform more advanced analytics tasks. For this, you can create a bucket called "logs" (or anything else) and use a unique value - such as a date - for the key. Log files would be the values associated with the unique keys. For storing log data from different systems, you could create a unique bucket for each system and write associated logs to that bucket. In terms of analyzing log data, you could then use Riak's MapReduce for aggregation tasks such as summing the counts of records for a date, or Riak Search for more robust, text-based queries. 

## Complex Case

For storing a large amount of log data that is frequently writing data to Riak, some users might consider doing primary storage of logs in a Riak cluster and then replicating data to a secondary cluster to run heavy analytics jobs, either over another Riak cluster or another solution such as Hadoop. Because the access patterns of reading and writing data to Riak is very different from the access pattern of something like a MapReduce job which is iterating over many keys, separating the write workload from the analytics workload will let you maintain higher performance and yield more predictable latency.

## Community Examples

<table class="links">
  <tr>
    <td><a href="http://www.simonbuckle.com/2011/08/27/analyzing-apache-logs-with-riak/" target="_blank" title="Riak at OpenX"><img src="/images/simon-analyzing-logs.png"/></a>
    </td>
    <td>Simon Buckle on <a href="http://www.simonbuckle.com/2011/08/27/analyzing-apache-logs-with-riak/" target="_blank">analyzing Apache logs with Riak.</a>
    </td>
  </tr>
</table>
