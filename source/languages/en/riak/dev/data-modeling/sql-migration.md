---
title: Migrating from an SQL Database to Riak
project: riak
version: 2.0.0+
document: cookbook
audience: advanced
keywords: [migration, sql]
---

1. Issue caveats about use cases and specificity
2. Convert SQL table data to a set of objects (on a per-row basis)
3. Fetch columns names
4. Convert each object to JSON
5. Store objects in Riak with keys stored in a set
6. Functions for fetching all objects, fetching objects based on primary key, etc.