---
title: Ring Resizing
project: riak
toc: true
audience: advanced
---

Ring resizing enables `riak_core` applications to change the number of partitions (running vnodes) during normal operations, even when under considerable load. Previously, a cluster was limited to always having a number of partitions dictated by `ring_creation_size` and set once and for all (or at least for the life cycle of the cluster). Changing the number of partitions previously involved standing up a brand new cluster and migrating extant data to and from that cluster.

Now, this can be done with far less hassle and without a "dummy" cluster. This is _not_ intended as a scaling feature; Riak has plenty of these already (like adding and removing nodes...the ring size is a much different sort of thing). 

Only permitted cluster operation during a ring resize is `force-remove`; all other operations will be delayed

https://gist.github.com/jrwest/3ddfc521d33a23644b9f

Two components:

1. Determine what the future ring will look like
2. Make the transition

Determining future ownership => `riak_core_claimant` => ring resizing, staging, planning, committing, etc.; determines ownership in the future ring and what transfers will be necessary to achieve the transition

Expanding the ring => all existing partitions will be in both the old and new ring; initially assigned to a dummy node before being assigned through `claim`; if the ring is downsized, some partitions will be removed; some partitions may change owners as part of this process

Transfers are scheduled using the "next list;" future transfer info is carried in the gossip structure; future ring cannot be calculated using only the "next list" => gossip structure determines this as well; stored in metadata dictionary

If the plan is committed, the claimant will not install the new ring until all of the scheduled transfers have completed, in order to ensure a safe transition; because of this, all cluster operations with the exception of `force-replace` will be delayed until resizing completes; the claimant also provides the ability to cancel while resizing in flight

## Transfer Scheduling

Partitions must transfer their data not only to new _owners_ of the same partitions but also to new _partitions_, because every preflist 

#### For a few key pairs:

![](https://a248.e.akamai.net/camo.github.com/ae4de1ceb72cac63a0f71bd43d11dc337609a6fb/687474703a2f2f646174612e7269616b63732e6e65743a383038302f6a72772d7075626c69632f64796e616d69632d72696e672f657870616e642d707265666c6973742e706e67)

In the image on the left a key in the old ring is owned by N (3 in this case) partitions, as expected. In the middle image, each partition is divided in half to show, relatively, where the keys fall within the keyspace owned by each partition. The image on the right shows which partitions should own the key in the new ring.

![](https://a248.e.akamai.net/camo.github.com/d33619a217feda1b1992dbc86c53b69a1d6bf640/68747470733a2f2f613234382e652e616b616d61692e6e65742f63616d6f2e6769746875622e636f6d2f333435363062353663653133396135386330393437393762323432656632373466376166323934632f363837343734373033613266326636343631373436313265373236393631366236333733326536653635373433613338333033383330326636613732373732643730373536323663363936333266363437393665363136643639363332643732363936653637326636353738373036313665363432643730373236353636366336393733373432643734373236313665373336363635373237333265373036653637)

To safely transfer this key between the old and new ring there are five necessary transfers (there would be a 6th, the top arrow in the image above, but the owner of that portion of the keyspace is the same node). The number of transfers is dependent on the growth factor (new ring size / old ring size) and the largest N value in the cluster. A similar process takes place when the ring is shrinking.

Claimant schedules a single **resize operation** for each vnode in the existing ring, which is an entry in the next list with a special value, `$resize`, in the next owner field; intended to indicate that the existing vnode has several actual transfers to complete before it has safely handed off all data

The claimant will also schedule a **resize transfer** for each index; represents a single handoff of a portion of the keyspace between two partitions that may or may not have different owners (most likely, they will). Resize transfer depends on several factors:

1. If the ring is shrinking and the partition will no longer exist in the new ring, the transfer is scheduled to the owner of the first successor.
2. If the ring is expanding and the existing partition is not being moved to a new owner, the transfer is scheduled to the owner for the first predecessor.
3. If the ring is expanding and the existing partition is being moved to a new owner, the transfer is scheduled to the new owner.

A resize transfer is like a repair because not all keys are transferred; there is a filter function that determines which keys will be sent; the list of partitions is used to schedule future transfers

## Afterward

Data is removed after the resized ring is installed. The claimant performs this by scheduling a `$delete` in the next list; scheduled for any partition that no longer exists in the new ring or was moved during resizing; each partition that meets those criteria deletes its data and shuts down

## Scratchpad

Two major changes that must be made to any `riak_core` application that wants to support dynamic ring sizing:

1. The following function must be defined:

```erlang
object_info(term()) -> {undefined | binary(),binary()}`
```

2. And then the following function:

```erlang
request_hash(term()) -> undefined | binary()
```
