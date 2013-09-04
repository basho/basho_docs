---
title: "Taste of Riak: Querying with Java"
project: riak
version: 1.3.1+
document: tutorials
toc: true
audience: beginner
keywords: [developers, client, 2i, search, linkwalking, java]
---

Now that we've had a taste of the CRUD interface for Riak, let's look into three other ways of querying for data - Link Walking, Secondary Indices, and Riak Search.  

###Links

###Secondary Indexes

If you're coming from a SQL world, Secondary Indexes (2i) are a lot like SQL indexes.  They are a way to lookup objects based on a secondary key, or tag.  Ordinary Key/Value data is opaque to 2i, so we have to add entries to the indices at the application level.

### Search
