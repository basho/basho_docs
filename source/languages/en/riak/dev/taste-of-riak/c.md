---
title: "Taste of Riak: C"
project: riak
version: 1.4.0+
document: guide
audience: beginner
keywords: [developers, client, c]
---

This Taste of Riak will show you some simple synchronous operations against Riak. The Riak C client also has an asynchronous mode, but that won't be covered here.

<div class="note">
<div class="title">Riak C Client status</div>

The Riak C Client is currently under development and is NOT ready to use in a production environment. Expect significant changes to the API.
</div>


If you haven't set up a Riak node and started it, please visit the [[Prerequisites|Taste of Riak: Prerequisites]] first.

###Client Setup

Download and install the [riak-c-client](https://github.com/basho/riak-c-client) by following [these](https://github.com/basho/riak-c-client#building) directions.

Next, download [taste_of_riak.c](https://raw.github.com/basho/taste-of-riak/master/c/taste_of_riak.c) and [Makefile](https://raw.github.com/basho/taste-of-riak/master/c/Makefile) for this tutorial, and save it to your working directory.

<div class="note">
<div class="title">Configuring for a local cluster</div>

If you set up a local Riak cluster using the [[five minute install]] method, open up the `taste_of_riak.c` file in an editor, and change the value of riak_port to 10017.  This code section should now look like:


```c
const char* riak_host = "localhost";
const char* riak_port = "10017";
```

</div>

You can now compile and run this via the command line.

```bash
make
./taste_of_riak
```

Running it should return:

```
-------------------------------
Test Riak PUT
-------------------------------
V-Clock: 6bce61606060cc60ca05521c4783367207f39ce3cf604a64cc636590f16a3cc3970500
Objects: 1
Bucket:
Value:
Last Mod: 2014-02-25 12:08:44
Last Mod uSecs: 592348
VTag: 7RZlcD77ja2udGZnKqcTbE

Ok
-------------------------------
Test Riak GET
-------------------------------
Bucket: TestBucket
Key: TestKey
Value: TestData
Last Mod: 2014-02-25 12:08:44
Last Mod uSecs: 592348
VTag: 7RZlcD77ja2udGZnKqcTbE

Ok
-------------------------------
Test Riak UPDATE
-------------------------------
Ok
-------------------------------
Test Riak GET
-------------------------------
V-Clock: 6bce61606060cc60ca05521c4783367207f39ce3cf604a64ca636590f16a3cc3970500
Unmodified: false
Deleted: false
Objects: 1
Bucket: TestBucket
Key: TestKey
Value: MyValue
Last Mod: 2014-02-25 12:08:44
Last Mod uSecs: 636704
VTag: 1bYXYmbn2zfs3YXt2Yd36T

Ok
-------------------------------
Test Riak DELETE
-------------------------------
Ok
```

Since C doesn’t have a REPL environment, we shall now walk through the code to see what it actually did at each step.  

###Connecting to Riak

The first thing we do in our code is initialize a Riak configuration and connection:

```c
riak_config *cfg;
riak_connection *cxn = NULL;
riak_error err;

err = riak_config_new_default(&cfg);
if(err)
  …
  
const char* riak_host = "localhost";
const char* riak_port = "8087";
err = riak_connection_new(cfg, &cxn, riak_host, riak_port, NULL);
if(err) 
  …
 
```

In many cases, the default `riak_config` should work fine, as it allows you to configure logging, custom memory allocators, and custom protocol buffers allocators. A `riak_config` is passed to most calls in the C client. 

A `riak_connection` establishes a TCP connection to the specified host and port. 

Next, we store some data in Riak via the `put` function. 

###Storing Objects In Riak

```c
// this allocates a riak_object in memory, but it's not stored in Riak
// until we call riak_put()
riak_object *obj = riak_object_new(cfg);

// setting bucket, key, and value on a riak_object is done via
// riak_binary structs. A riak_binary is a charset-agnostic byte array.
// You must free any riak_binary structs that you allocate
riak_binary *bucket_bin   = riak_binary_copy_from_string(cfg, "TestBucket");
riak_binary *key_bin      = riak_binary_copy_from_string(cfg, "TestKey");
riak_binary *value_bin    = riak_binary_copy_from_string(cfg, "TestData");

// set bucket, key, and value on the in-memory object
riak_object_set_bucket(cfg, obj, bucket_bin);
riak_object_set_key(cfg, obj, key_bin);
riak_object_set_value(cfg, obj, value_bin);

riak_put_response *put_response = NULL;
riak_put_options *put_options = riak_put_options_new(cfg);
// do the PUT against Riak.
// at this point, your object will be stored in Riak
err = riak_put(cxn, obj, put_options, &put_response);
if(err == ERIAK_OK) {
  …
```

In this first example we have stored the string `TestData` with the lookup key of `TestKey` in bucket `TestBucket`.  

###Reading Objects From Riak
Now that we have a few objects stored, let’s retrieve them and make sure they contain the values we expect.

```c
riak_binary *bucket_bin   = riak_binary_copy_from_string(cfg, "TestBucket");
riak_binary *key_bin      = riak_binary_copy_from_string(cfg, "TestKey");

// a buffer to write results into
char output[10240];
// check this for errors after performing an operation
riak_error err;

// allocate a struct to set GET options, specifically
// to set the basic_quorum & r options
riak_get_options *get_options = riak_get_options_new(cfg);
riak_get_options_set_basic_quorum(get_options, RIAK_TRUE);
riak_get_options_set_r(get_options, 2);

riak_get_response *get_response = NULL;
err = riak_get(cxn, bucket_bin, key_bin, get_options, &get_response);
if(err == ERIAK_OK) {
    riak_print_get_response(get_response, output, sizeof(output));
    printf("%s\n", output);
}

// see the taste_of_riak.c file for memory cleanup, etc
```

###Updating Objects In Riak

To perform an update, we store the new value with the same key using the `riak_object` instance that's returned from `riak_get`.


```c
riak_binary *new_value_bin = riak_binary_copy_from_string(cfg, "MyValue");
  …
riak_get_response *get_response = NULL;
err = riak_get(cxn, bucket_bin, key_bin, get_options, &get_response);
if(err) {
  …
}

// access the raw object
riak_object *obj = riak_get_get_content(get_response)[0];
// change it's value to "MyValue"
riak_object_set_value(cfg, obj, new_value_bin);

riak_put_response *put_response = NULL;
riak_put_options *put_options = riak_put_options_new(cfg);
// store the object with new value
err = riak_put(cxn, obj, put_options, &put_response);
if(err) {
  …
}

```

###Deleting Objects From Riak

To delete, the bucket and key of an object are specified along with any delete options.

```c
riak_delete_options *delete_options = riak_delete_options_new(cfg);
riak_delete_options_set_w(delete_options, 1);
riak_delete_options_set_dw(delete_options, 1);
err = riak_delete(cxn, bucket_bin, key_bin, delete_options);
if(err) {
  …
}
```




