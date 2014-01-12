---
title: "Taste of Riak: OCaml"
project: riak
version: 1.4.0+
document: guide
toc: true
audience: intermediate
keywords: [developers, client, ocaml]
---

If you haven't set up a Riak Node and started it, please visit the
[[Prerequisites|Taste of Riak: Prerequisites]] first.

To try this flavor of Riak, a working installation of [OCaml](http://ocaml.org/) with [OPAM](http://opam.ocamlpro.com/doc/Quick_Install.html) is required. 

### Client Setup

The [riak-ocaml-client](http://metadave.github.io/riak-ocaml-client/) is a community-maintained Riak client library for OCaml.

First, download the *riak-ocaml-client* via OPAM.

```bash
$ opam install oasis
$ opam install riak
```

Agree to download additional dependencies when prompted by OPAM.


Next, download the `taste-of-ocaml` sample project from Github:

```bash
$ git clone git@github.com:basho-labs/taste-of-ocaml.git
$ cd taste-of-ocaml
```

The `src` directory contains a single file titled `taste_of_riak.ml`.

The sample code tries to connect to 127.0.0.1, port 8098 by default. If you set up a local Riak cluster using the [[five minute install]] method,
change the `pbip` let binding to **10017**:

```
let pbip = 10017 in
...
```

Let's compile `src/taste_of_riak.ml` using the following commands:

```bash
$ ./configure
$ make
```

Running the `./taste_of_riak.byte` command should return the following output:

```bash
$ ./taste_of_riak.byte
Ping
	Pong
Put: bucket=MyBucket, key = MyKey, value = MyValue
Get: bucket=MyBucket, key = MyKey
	Value = MyValue
Delete: bucket=MyBucket, key = MyKey
Get: bucket=MyBucket, key = MyKey
	Not found
```

### Connecting

To connect to a Riak node via Protocol Buffers, you need to specify the IP address and port number. This value can be found in Riak's `app.config` file, under the `riak_api` section's `pb` property.

For example:

```erlang
	{pb, [ {"127.0.0.1", 10017 } ]}
```

The `riak_connect_with_defaults` function takes the IP and port number as parameters.
For example:

```ocaml
let pbhost = "127.0.0.1" in
let pbip = 10017 in
try_lwt
   lwt conn = riak_connect_with_defaults pbhost pbip in
   ...
```

The Riak OCaml Client uses [Lwt](http://ocsigen.org/lwt/manual/) and the Lwt Syntax extension. The table below can give you an idea of how the syntax preprocesses OCaml to easily support Lwt:


Without Lwt           | With Lwt
----------------------|---------------------
let pattern1 = expr1  |	lwt pattern1 = expr1
try                   | try_lwt
match expr with       | match_lwt expr with
while expr do         | while_lwt expr do
raise exn             | raise_lwt exn
assert expr	           | assert_lwt expr


### Storing Data in Riak

Next, we will store some simple data in Riak. Buckets, keys, and values are stored as strings:

```ocaml
let my_bucket = "MyBucket" in
let my_key = "Foo" in
let my_value = "Bar" in
lwt _result = riak_put conn bucket (Some key) value [] in
```

The last parameter, an empty list in this case, specifies the Riak *put options*. 

For example, to specify the `Put_return_body` options, use the following:

```ocaml
let put_options = [Put_return_body true] in
lwt _result = riak_put conn bucket (Some key) value put_options in
```


### Fetching Data from Riak

Next, we can fetch data from Riak using a bucket and key. Since the the
`riak_get` function might not find a value at the specified key, you'll have
to pattern match against the `Maybe` value returned. If a value exists at the
specified key, you'll have to pattern match against `Maybe` as well to
retrieve the actual content at that key.

```ocaml
 lwt obj = riak_get conn bucket key [] in
  match obj with
      | Some o ->
          (match o.obj_value with
              | Some v -> print_endline ("Value = " ^ v);
                          return ()
              | None -> print_endline "No value";
                        return ())
      | None -> print_endline "Not found";
                return ()
```

Also, note the `return ()` from Lwt at the end of each clause. 

To specify options for the get operation:

```ocaml
let get_options = [Get_basic_quorum false; Get_head true] in
lwt obj = riak_get conn bucket key get_options in
...
```


### Deleting Objects from Riak

If you want to delete data from Riak, simply call the `riak_del` function:

```ocaml
let key = "MyKey" in
let del_options = [] in
lwt _ = riak_del conn bucket key del_options in
    return ()
```

### Next steps

For documentation on all available functions in the Riak OCaml Client, check out the [project page](http://metadave.github.io/riak-ocaml-client/) and the [bundled test](https://github.com/metadave/riak-ocaml-client/blob/master/test/test.ml).
