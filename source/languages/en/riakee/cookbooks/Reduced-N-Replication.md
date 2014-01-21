---
title: Reduced-N Replication
project: riakee
version: 2.1.0+
document: cookbook
toc: true
audience: advanced
---

Riak Enterprise version >= 2.1.0 comes equipped with the object size reduction mutator [`riak_repl_reduced`](https://github.com/basho/riak_repl/blob/develop/src/riak_repl_reduced.erl). When this mutator is employed, objects are reduced in size upon put and then expanded when retrieved via get requests.

The benefit of this sort of mutator is reduction in overall storage cost, but please note that reduced-N replication often incurs additional costs in the form of higher CPU usage and in some cases increased bandwidth. Reduced-N replication behavior can be switched on and off and modified on both a per-bucket and cluster-wide basis.

<div class="note">
<div class="title">Note</div>
Reduced-N replication is enabled by default in Riak Enterprise. If you do not wish to use it, you will need to explicitly disable it using one of the methods shown below.
</div>

Reduced-N replication is managed via the `full_objects` property. This property can have three values:

Value | Description
:-----|:-----------
`always` | Objects are *never* reduced. This is the default.
`never` | Objects are *always* reduced.
`N` | If a positive integer value is entered instead of `always` or `never`, keep that many or N full objects, whichever is smaller; N is the usual `n_val` on a put; if a full value cannot be found, a proxy get is attempted. (**this explanation needs work**).

## Managing Reduced-N Replication for Buckets

The `full_objects` parameter can be set either via bucket properties or bucket types.

Bucket properties:

```curl
curl -XPUT \
  -H "Content-Type: application/json" \
  -d '{"props":{"full_objects": ... }}' \
  http://localhost:8098/buckets/my_bucket/props
```

Via bucket types:

```bash
riak-admin bucket-type create reduced_n '{"props":{"full_objects":"never"}}'
riak-admin bucket-type status reduced_n
riak-admin bucket-type activate reduced_n
```

## Managing Reduced-N Replication on a Cluster-wide Basis

Cluster metadata:

The value of `{'riak_repl', 'reduced_n'}, 'full_objects'}` must be set to one of the values in the table above. More on managing cluster metadata [[here|Cluster Metadata]].

Command line:

```bash
riak-repl full_object [always | never | <positive_integer> ]
```

`full_object` can be set as a normal bucket property

## Removing the Mutator Entirely

Even if the `full_objects` parameter is set to `always` (meaning that no reduction is taking place), the `riak_repl_reduced` mutator is still registered in the cluster metadata. This can be undone in the Erlang shell. Opening the shell:

```bash
riak attach
```

```erlang
(dev1@127.0.0.1)> riak_kv_mutator:unregister(riak_repl_reduced).
```

The above removes the ability to use reduced objects completely.
