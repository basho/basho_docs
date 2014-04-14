---
title: Key/Value Best Practices
project: riak

---

More in [this video](http://www.youtube.com/watch?v=-_3Us7Ystyg#aid=P-4heI_bFwo), with the presentation slides available [on Speaker Deck](https://speakerdeck.com/hectcastro/throw-some-keys-on-it-data-modeling-for-key-value-data-stores-by-example)

RDBMSes => relationships, transactions, schemas, ability to extend pre-existing structure easily, ad-hoc queries

NoSQL => no joins, no grouping across shards

Interface is basically a hash => { key: value }

Everything is binary data; all of it is opaque (with the exception of Riak Data Types, discussed below)

## Example

Possible sources for "natural" keys:

#### Timestamps

Timestamps, e.g. `2013-11-05T08:15:30-05:00`

#### UUIDs



#### Combination

For example, the name of the data type in your application, plus an identifier: `user_17711`. If you're using Riak as a CMS and you want to manage multiple site domains, you could create a separate bucket for each, e.g. `main-site`, `login-sites`, and `user-homepages`, and store `user_<USER_ID>` keys in each.

## Riak Data Types

[[Riak Data Types|Using Data Types]] are special in Riak 

You can use [[sets|Data Types#sets]] to keep track of which user keys are in the bucket


