---
title: User Settings/Preferences
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [use-cases]
---

*Typically one-to-one relationships*

## Simple Case

For user account-related data that is simple, frequently read but rarely changed (such as a privacy setting or theme preference), consider storing it in the user object itself. Another common pattern is to create a companion User Settings type of object, also keyed off of the user id for easy one-read retrieval.


## Complex Case

 If you find your application frequently writing to the user account, or have dynamically growing user related data such as bookmarks, subscriptions or multiple notifications, then a more advanced data model is called for (see the section on social events/subscriptions)
 