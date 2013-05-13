---
title: Upgrading from Riak Search
project: riak
version: 0.14.2+
document: cookbook
toc: true
audience: advanced
keywords: [search, upgrading]
---

As of Riak 1.0, Riak Search has been integrated into Riak. A few
considerations must be kept in mind when transitioning.

<div class="note">
Rolling upgrade is guaranteed to work with Search only if upgrading
from the 0.14.2 version.  Even in this case there are caveats which
are explained below.  You may try a rolling upgrade from previous
versions but the behavior is undefined.  Rather, it is suggested to
stop the entire cluster and upgrade all nodes offline.
</div>

## Upgrading from Riak Search 0.14.2

<div class="note">
There are some extra steps that must be taking while installing the
new package.
</div>

<div class="note">
While in a mixed cluster state some queries will return incorrect
results or timeout.  Once all nodes are upgraded to the 1.0.0 version
queries will return correct results.
</div>

### Migration of Old Data

Since Riak Search was integrated into Riak proper in 1.0.0 the package
names no longer match (i.e. from Riak Search to riak) therefore it's
not possible to simply install the new package.  Instead, uninstall
the old package, move the data directory, and then install the new
package.  Finally, don't forget to migrate any changes you made to
`vm.args` or `app.config` from the original Riak Search install.
Below is an example of what this might look like on CentOS.


    riaksearch stop
    rpm -e riak-search-0.14.2-1
    mv /var/lib/riaksearch /var/lib/riak
    rpm -Uvh riak-ee-1.0.0-1.el5.x86_64.rpm
    vim /etc/riak/vm.args /etc/riak/app.config

Along with your legacy modifications to `app.config` you must also
explicitly tell Riak to enable the Search functionality.  Below is
what the `riak_search` of your `app.config` should look like.  Don't
forget to do this for all nodes.


```erlang
  {riak_search, [
                 {enabled, true}
                ]},
```

Afterwards, you can start the node and continue with the [[Rolling Upgrades]]
instructions.

### Why Do Some Queries Fail During Rolling Upgrade

There was an oversight in the code that prevents 0.14.2 and 1.0.0
nodes from coordinating Search queries properly.  These failures will
be non-deterministic because it depends on which node handles the
query request and which one coordinates it.  This, ultimately depends
on which node holds the data (which is determined by a hash function),
which node handles the request, and some code in Search which performs
random selection in certain cases.

## Upgrading from Riak Search <0.14.2

As previously mentioned, when moving from older versions of Riak
Search to Riak 1.0, rolling upgrades are not recommended. The entire
cluster must be shut down, and each node upgraded to Riak 1.0.  As
above, each `app.config` must be modified to enabled Search.
