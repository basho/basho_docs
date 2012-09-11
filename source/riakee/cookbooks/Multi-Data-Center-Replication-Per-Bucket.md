<div class="info"><div class="title">Riak Enterprise Only</div>This documentation applies only to Riak Enterprise, Basho's commercial extension to <a href="http://wiki.basho.com/Riak.html">Riak</a>. To learn more about the differences between Riak and Riak Enterprise, <a href="http://basho.com/products/riak-overview/">read here</a>.  To talk to us about using Riak Enterprise,  <a href="http://info.basho.com/Wiki_Contact.html" target="_blank">let us know</a>.</div>

<div class="note"><div class="title">This feature is available in Riak Enterprise 1.1+</div></div>

To enable or disable replication per bucket, you can use the `repl` bucket property.

Some changes have ocurred between 1.1 and 1.2.

These `repl` values are available in Riak EE version 1.1 and above:

  * `true`: enable default replication (realtime + fullsync)
  * `false`: disable default replication (realtime + fullsync)

These option values are only available in Riak EE version 1.2 and above:

  * `realtime`: replication only occurs in realtime for this bucket
  * `fullsync`: replication only occurs during a full-synchronization
  * `both`: replication occurs in realtime and during full-synchronization

### Example of Disabling

```
curl -v -X PUT -H "Content-Type: application/json" \
-d '{"props":{"repl":false}}' \
http://127.0.0.1:8091/riak/my_bucket
```

### Example of Enabling

```
curl -v -X PUT -H "Content-Type: application/json" \
-d '{"props":{"repl":true}}' \
http://127.0.0.1:8091/riak/my_bucket
```
