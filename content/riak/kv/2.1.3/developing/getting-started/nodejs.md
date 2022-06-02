---
title: "Getting Started with NodeJS"
description: ""
project: "riak_kv"
project_version: "2.1.3"
menu:
  riak_kv-2.1.3:
    name: "NodeJS"
    identifier: "getting_started_nodejs"
    weight: 104
    parent: "developing_getting_started"
toc: true
aliases:
  - /riak/2.1.3/dev/taste-of-riak/nodejs
  - /riak/kv/2.1.3/dev/taste-of-riak/nodejs
---

[introduction.js]: https://github.com/basho/riak-nodejs-client-examples/blob/master/dev/taste-of-riak/introduction.js
[npm]: https://www.npmjs.com/package/basho-riak-client
[node_js_installation]: https://github.com/basho/riak-nodejs-client/wiki/Installation
[nodejs_wiki]: https://github.com/basho/riak-nodejs-client/wiki

If you haven't set up a Riak Node and started it, please visit [Running A Cluster]({{<baseurl>}}riak/kv/2.1.3/using/running-a-cluster) first.

To try this flavor of Riak, a working installation of Node.js 0.12 or later is
required.

Code for these examples is available [here][introduction.js]. To run, follow
these directions:

```bash
git clone git://github.com/basho/riak-nodejs-client-examples
cd riak-nodejs-client-examples
npm install
node ./app.js
```

### Client Setup

Install [the Riak Node.js Client][node_js_installation] through [NPM][npm].

### Connecting to Riak

Connecting to Riak with the Riak Node.js Client requires creating a new client
object and using the callback argument to know when the client is fully
initialized:

```javascript
var Riak = require('basho-riak-client');
var nodes = [
    'riak-test:10017',
    'riak-test:10027',
    'riak-test:10037',
    'riak-test:10047'
];
var client = new Riak.Client(nodes, function (err, c) {
    // NB: at this point the client is fully initialized, and
    // 'client' and 'c' are the same object
});
```

This creates a new `Riak.Client` object which handles all the details of
tracking active nodes and also provides load balancing. The `Riak.Client` object
is used to send commands to Riak. When your application is completely done with
Riak communications, the following method can be used to gracefully shut the
client down and exit Node.js:

```javascript
client.stop(function (err, rslt) {
    // NB: you may wish to check err
    process.exit();
});
```

Let's make sure the cluster is online with a `Ping` request:

```javascript
var assert = require('assert');

client.ping(function (err, rslt) {
    if (err) {
        throw new Error(err);
    } else {
        // On success, ping returns true
        assert(rslt === true);
    }
});
```

This is some simple code to test that a node in a Riak cluster is online - we
send a simple ping message. Even if the cluster isn't present, the Riak Node.js
Client will return a response message. In the callback it is important to check
that your activity was successful by checking the `err` variable.

We are now ready to start interacting with Riak.

## Next Steps

[CRUD Operations]({{<baseurl>}}riak/kv/2.1.3/developing/getting-started/nodejs/crud-operations)
