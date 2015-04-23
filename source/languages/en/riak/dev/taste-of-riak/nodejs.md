---
title: "Taste of Riak: NodeJS"
project: riak
version: 1.4.0+
document: guide
toc: true
audience: beginner
keywords: [developers, client, javascript, nodejs]
---

If you haven't set up a Riak Node and started it, please visit the
[[Prerequisites|Taste of Riak: Prerequisites]] first.

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

Connecting to Riak with the Riak Node.js Client requires creating a new client object.

```javascript
var Riak = require('basho-riak-client');
var client = new Riak.Client([
    'riak-test:10017',
    'riak-test:10027',
    'riak-test:10037',
    'riak-test:10047'
]);
```

This creates a new `Riak.Client` object which handles all the details of
tracking active nodes and also provides load balancing. The `Riak.Client` object
is used to send commands to Riak. When your application is completely done with
Riak communications, the following method can be used to gracefully shut the
client down and exit Node.js:

```javascript
function client_shutdown() {
    client.shutdown(function (state) {
        if (state === Riak.Cluster.State.SHUTDOWN) {
            process.exit();
        }
    });
}
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

### Saving Objects to Riak

Pinging a Riak cluster sounds like a lot of fun, but eventually someone is going
to want us to do productive work. Let's create some data to save in Riak.

The Riak Node.js Client makes use of a `RiakObject` class to encapsulate Riak
key/value objects. At the most basic, a `RiakObject` is responsible for
identifying your object and for translating it into a format that can be easily
saved to Riak.

```javascript
var async = require('async');

var people = [
    {
        emailAddress: "bashoman@basho.com",
        firstName: "Basho",
        lastName: "Man"
    },
    {
        emailAddress: "johndoe@gmail.com",
        firstName: "John",
        lastName: "Doe"
    }
];

var storeFuncs = [];
people.forEach(function (person) {
    // Create functions to execute in parallel to store people
    storeFuncs.push(function (async_cb) {
        client.storeValue({
                bucket: 'contributors',
                key: person.emailAddress,
                value: person
            },
            function(err, rslt) {
                async_cb(err, rslt);
            }
        );
    });
};

async.parallel(storeFuncs, function (err, rslts) {
    if (err) {
        throw new Error(err);
    }
});
```

In this sample, we create a collection of `Person` objects and then save each
`Person` to Riak. Once again, we check the response from Riak.

### Reading from Riak

Let's find a person!

```javascript
var logger = require('winston');

client.fetchValue({ bucket: 'contributors', key: 'bashoman@basho.com', convertToJs: true },
    function (err, rslt) {
        if (err) {
            throw new Error(err);
        } else {
            var riakObj = rslt.values.shift();
            var bashoman = riakObj.value;
            logger.info("I found %s in 'contributors'", bashoman.emailAddress);
        }
    }
);
```

We use `client.fetchValue` to retrieve an object from Riak. This returns an
array of `RiakObject` objects which helpfully encapsulates the communication
with Riak.

After verifying that we've been able to communicate with Riak *and* that we have
a successful result, we use the `value` property to get the object, which has
already been converted to a javascript object due to the use of `convertToJs:
true` in the options.

### Modifying Existing Data

Let's say that Basho Man has decided to be known as Riak Man:

```javascript
bashoman.FirstName = "Riak";
riakObj.setValue(bashoman);

client.storeValue({ value: riakObj }, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
});
```

Updating an object involves modifying a `RiakObject` then using
`client.storeValue` to save the existing object.

### Deleting Data

```javascript
client.deleteValue({ bucket: 'contributors', key: 'johndoe@gmail.com' }, function (err, rslt) {
    if (err) {
        throw new Error(err);
    }
};
```

Just like other operations, we check the results that have come back from Riak
to make sure the object was successfully deleted. Of course, if you don't care
about that, you can just ignore the result.

The Riak Node.js Client has a lot of additional functionality that makes it easy
to build rich, complex applications with Riak. Check out the
[documentation][dotnet_wiki] to learn more about working with the Riak Node.js
Client and Riak.

## Next Steps

More complex use cases can be composed from these initial create, read, update,
and delete (CRUD) operations. [[In the next chapter|Taste of Riak: Querying]],
we will look at how to store and query more complicated and interconnected data,
such as documents.


[introduction.js]: https://github.com/basho/riak-nodejs-client-examples/blob/master/dev/taste-of-riak/introduction.js
[node_js_installation]: https://github.com/basho/riak-nodejs-client/wiki/Installation
[npm]: https://www.npmjs.com/package/basho-riak-client
[dotnet_wiki]: https://github.com/basho/riak-dotnet-client/wiki
