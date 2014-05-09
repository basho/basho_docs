---
title: Dotted Version Vectors
project: riak
version: 2.0.0+
document: appendix
audience: advanced
---

Dotted version vectors (DVVs)

## Resources

* [Evaluating Dotted Version Vectors in Riak](http://asc.di.fct.unl.pt/~nmp/pubs/inforum-2011-2.pdf)
* [Improving Logical Clocks in Riak with Dotted Version Vectors: A Case Study](http://paginas.fe.up.pt/~prodei/dsie12/papers/paper_19.pdf)
* [Dotted Version Vector Sets](https://github.com/ricardobcl/Dotted-Version-Vectors)




Notes
-----

DVVs prevent false conflicts --> preventing sibling explosion
Means of describing causality between values, so that decisions can be made in case of conflict
Concurrent updates problem (as with vector clocks)
More lightweight
Causally concurrent values are all keps (provided that `allow_mult` is set to `true`)
DVV sets => storing all values
Vector clocks != version vectors

VVS => scalability problems, loss of accuracy (when pruning is used to prevent vector growth)
DVVs => size is limited to replication degree