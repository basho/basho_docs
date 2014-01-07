---
title: Strong Consistency
project: riak
version: 2.0.0+
document: guide
toc: true
audience: intermediate
keywords: []
---

http://vimeo.com/51973001

The core emphasis of Riak has always been on data availability rather than consistency. To that end, Riak has always favored the "A" and the "P"---availability and partition (i.e. fault) tolerance---to the "C"---consistency---in the CAP theorem. Now that doesn't mean that consistency was ever simply discarded as a goal; rather, Riak was built to provide *eventual* as opposed to *strong* consistency.

Favoring availability and partioning over strong consistency remains the default behavior in Riak. However, in post-2.0 releases, we have sought to make strong---or at the very least strong*er*---consistency an option for Riak users on a bucket-by-bucket basis. If your use case demands that some---or all---buckets provide strong consistency, then you may wish to pay attention to this doc.

## What Strong Consistency Means

If you get or put a key, the next successful read is guaranteed to show that write (or the result of a future write that saw the write); Riak ensures that the object didn't change since you last accessed it; the request will fail if a concurrent write occurred and changed the object; the old value tends to win

## Strongly Consistent Buckets

`consistent = true`

```curl
curl -XPUT \
-H "Content-Type: application/json" \
-d '{"props": {"consistent": true}' \
http://localhost:8098/buckets/my_bucket
```

`riak_ensemble` => `put_once`, `modify`, `overwrite`

