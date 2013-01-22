---
title: Sensor Data
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [use-cases]
---

## Simple Case

 Riak's scalable design makes it useful for data sets, like sensor data, that scale rapidly and are subject to heavy read/write loads. Many sensors collect and send data at a given interval. One way to model this in Riak is to create a bucket for each sensor device, and use the interval as a unique key (i.e., a date or combination of date/time), then store update data as the value. You could then query on the interval; or alternatively store a timestamp as a secondary index (piece of queryable metadata attached to the key/value pair) that would allow you to perform queries on specific ranges or perform MapReduce queries against the indexes.

## Complex Case

If you are dealing with thousands or millions of sensors, yet very small data sets, storing all of a single device's updates as unique keys may be overly cumbersome when it comes to reading that device's data. Retrieving it all would mean calling a number of keys. Instead, you could store all of a device's updates in a document with a unique key to identify the device. Stored as a JSON document, you can read and parse all of those updates on the client side. Riak doesn't allow you to append data to a document without reading the object and writing it back to the key, however. This strategy would mean more simplicity and performance on the read side in tradeoff for slightly more work at write time and on the client side. You also have to keep an eye out for the total size of the document as it grows - we tend to recommend a sensible object limit of about 3-4MB in order to avoid performance problems in the cluster.
