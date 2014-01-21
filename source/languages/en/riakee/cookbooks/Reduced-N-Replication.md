---
title: Reduced-N Replication
project: riakee
version: 2.0.0+
document: cookbook
toc: true
audience: advanced
---



Shrink down objects on a put, then expand them on get (analogy to zipping); shrinking is based on either cluster or bucket settings (get usage examples)

Targeted to sink clusters

Trade CPU and perhaps bandwidth for reduced storage cost

Look up `riak_kv_mutator`

General:

* A put is performed; the object is checked for `cluster_of_record` metadata; if the data is missing or has the same name as the cluster, the object is not reduced; if the `cluster_of_record` is different from the current cluster, however, it will be reduced

The mutator never reduces an object (by default); to enable cluster-wide reduction, `riak_core_metadata` =>

`{'riak_repl', 'reduced_n'}, 'full_objects'}` to `always`, `never`, or a positive integer

The `full_objects` bucket property overrides cluster settings and uses the same option values
* `always` --- Always use `full_objects` => _never_ reduce objects (default)
* `never` --- Never use `full_objects` => _always_ reduce objects; all requests for the object will use proxy gets to the cluster of record; if the object cannot be retrieved from there, a `not found` value is returned; in order for `proxy_get` to work, the source cluster must have `proxy_get` enabled
* **positive integer** --- Keep that many or N full objects, whichever is smaller; N is the usual `n_val` on a put; if a full value cannot be found, a proxy get is attempted

To set the `full_objects` property on a cluster-wide basis, use the `riak-repl` command:

```bash
riak-repl full_objects [always | never | ]
```

Replication can be enabled or disabled on a per-bucket basis (or via bucket types)

`full_object` can be set as a normal bucket property

to remove the mutator: `riak_kv_mutator:unregister(riak_repl_reduced)`

The above removes the abiility to do reduced objects completely. setting full_objects to never means any object that isn't put directly on the cluster is reduced.

## Important

`full_objects` can be set per bucket

The mutator is enabled by default (in fact, the default is `always`)
