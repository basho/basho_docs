---
title: User Subscriptions/Events/Timelines
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [use-cases]
---

*Typically one-to-many and many-to-many relationships*

## Simple Case

Sometimes you may want to do more complex or specific kinds of modeling user data. A common example would be storing data for assembling a social network timeline. To create a user timeline, you could make "timeline" a bucket in Riak, and make keys a unique user ID. You would store timeline information as a value - a list of status update IDs which could then be used to retrive the full information from another bucket, or perhaps containing the full status update. If you want to store additional data - such as a timestamp, category or list properties - turn the list into an array of hashes containing this additional information. Note that in Riak, you cannot append information to an object, so to add events in the timeline, you would have to read the full object, add the new values to the hash, and write it back. 

## Complex Case
