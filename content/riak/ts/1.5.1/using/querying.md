---
title: "Querying Data in Riak TS"
description: "Querying Data in Riak TS"
menu:
  riak_ts-1.5.1:
    name: "Query Data"
    identifier: "querying_data_riakts"
    weight: 304
    parent: "using"
project: "riak_ts"
project_version: "1.5.1"
toc: true
aliases:
    - /riakts/1.5.1/using/querying/
---

[activating]: ../creating-activating/
[DESCRIBE]: describe/
[guidelines]: guidelines/
[planning]: ../planning/
[SELECT]: select/
[SHOW CREATE TABLE]: show-create-table/
[SHOW TABLES]: show-tables/
[writing]: ../writingdata/

You've [planned][planning] and [created][activating] your Riak TS table, and you've [written][writing] data to it. You are ready to query! But what does that mean?

Riak TS offers you several ways to define, manipulate, and query the data within your TS table. You can:

* Use [SELECT] to run various queries on your TS dataset. 
* Use [DESCRIBE] to see a full definition of your TS table.
* Use [SHOW TABLES] to list all the TS tables you have.
* Use [SHOW CREATE TABLE] to generate the SQL required to recreate a TS table.

You can also take a look at the [guidelines] to get an idea of the rules and best practices for running queries. 


{{% note title="WARNING" %}}
When querying, you must ensure the node issuing the query has adequate memory to receive the response. Queries will return rows based on the timespan (quanta) specified, if the returning rows do not fit into the memory of the requesting node, the node is likely to fail. 

Any given query consists of subqueries. If a single subquery loads a result that does not fit into memory, an out of memory error will occur on the subquery node and the requesting node will return a timeout error as it waits for the subquery to return.
{{% /note %}}