---
title: "Taste of Riak: Haskell"
project: riak
version: 1.4.0+
document: guide
toc: true
audience: intermediate
keywords: [developers, client, haskell]
---

If you haven't set up a Riak Node and started it, please visit the
[[Prerequisites|Taste of Riak: Prerequisites]] first.

To try this flavor of Riak, a working installation of [Haskell](http://www.haskell.org/platform/) (ideally the Haskell Platform) with [Cabal](http://www.haskell.org/haskellwiki/Cabal/How_to_install_a_Cabal_package) is required.

### Client Setup

The [riak-haskell-client](https://github.com/markhibberd/riak-haskell-client) is a
community-maintained Riak client for Haskell.

We're assuming you have a working install of ghc and cabal, if not consult your
OS documentation on how to make that happen.

First, clone the `taste-of-haskell` sample project from Github:

```
git clone git@github.com/tmcgilchrist/taste-of-haskell.git
cd taste-of-ocaml
make
```

Running make will setup a cabal sandbox and install all the necessary
dependencies to get `riak-haskell-client` running.

The `src` directory contains a single file titled `Main.hs`.

The sample code tries connecting to 127.0.0.1 port 8087 by default.

Let's compile `src/Main.hs` using the following command:
```
make
```

Running the `make run` command should return the following output:

```
$ cabal run
Preprocessing executable 'taste-of-haskell' for taste-of-haskell-0.1.0.0...
Running taste-of-haskell...
fromList ["__riak_client_test__"]
Get: bucket="testBucket", key = "testKey", value = Content {value = "{something: 'blah'}", content_type = Just "application/octet-stream", charset = Nothing, content_encoding = Nothing, vtag = Nothing, links = fromList [], last_mod = Nothing, last_mod_usecs = Nothing, usermeta = fromList [], indexes = fromList [], deleted = Nothing}
==========================
Get: bucket="testBucket", key = "testKey"
BucketProps {n_val = Just 3, allow_mult = Just False, last_write_wins = Just False, precommit = fromList [], has_precommit = Just True, postcommit = fromList [], has_postcommit = Just True, chash_keyfun = Just (ModFun {module' = "riak_core_util", function = "chash_std_keyfun"}), linkfun = Just (ModFun {module' = "riak_kv_wm_link_walker", function = "mapreduce_linkfun"}), old_vclock = Just 86400, young_vclock = Just 20, big_vclock = Just 50, small_vclock = Just 50, pr = Just 0, r = Just 4294967293, w = Just 4294967293, pw = Just 0, dw = Just 4294967293, rw = Just 4294967293, basic_quorum = Just False, notfound_ok = Just True, backend = Nothing, search = Nothing, repl = Nothing}
==========================
Delete: bucket="testBucket", key = "testKey"
==========================
Get: bucket="testBucket", key = "testKey"
BucketProps {n_val = Just 3, allow_mult = Just False, last_write_wins = Just False, precommit = fromList [], has_precommit = Just True, postcommit = fromList [], has_postcommit = Just True, chash_keyfun = Just (ModFun {module' = "riak_core_util", function = "chash_std_keyfun"}), linkfun = Just (ModFun {module' = "riak_kv_wm_link_walker", function = "mapreduce_linkfun"}), old_vclock = Just 86400, young_vclock = Just 20, big_vclock = Just 50, small_vclock = Just 50, pr = Just 0, r = Just 4294967293, w = Just 4294967293, pw = Just 0, dw = Just 4294967293, rw = Just 4294967293, basic_quorum = Just False, notfound_ok = Just True, backend = Nothing, search = Nothing, repl = Nothing}
==========================
```

### Connecting

The haskell riak client uses the protocol buffers interface so to connect you
need to specify the IP address and the protocol buffers port. These values can
be found in Riaks's `app.config` file, in the section `riak_kv`with keys `bp_ip`
and `pb_port`.

For example:

```
	{pb, [ {"127.0.0.1", 10017 } ]}
```

The `Client` data type in `Network.Riak.Basic` takes the IP and port numbers as
parameters. For example:

```
client = RB.Client "127.0.0.1" "8087" "test"
```

The Riak Haskell client runs within the IO monad, so should be similar to
typical IO Haskell programs. For example:

```
main = do
  conn <- Network.Riak.Basic.connect Network.Riak.Basic.Client "127.0.0.1" "8087" "test"
  Network.Riak.Basic.ping conn
```

### Storing Data in Riak

Next, we will store some simple data in Riak. Buckets, keys and values are each
wrapped in types, using Bucket, ByteString and ByteString respecitvely.

```
testBucket :: B.ByteString
testBucket = "testBucket"

testKey :: B.ByteString
testKey = "testKey"

testValue :: B.ByteString
testValue = "{something: 'blah'}"
```

Other modules within Riak Haskell client provide more conveniences around
storing JSON and doing automatic conflict resolution. Consult the
`riak-haskell-client` haddocks on hackage for further information.

To store this data in Riak we use `put` with the following
options:

```
main = do
    conn <- Network.Riak.Basic.connect Network.Riak.Basic.Client "127.0.0.1" "8087" "test"
    Network.Riak.Basic.put conn testBucket testKey Nothing (binary testValue) Default Default
```

Looking at the signature for `put` we can see there are some extra options
around; vector clock, write quorum and the read-write quorum, which we're
supplying with default values. For a real application you'll want to modify
those depending on requirements, checkout the `riak-haskell-client` haddocks for
more information on what those types mean.

### Fetching Data from Riak

Next, we can fetch data from Riak using a bucket and a key. Since there may not
be a value for that key `get` returns us a `Maybe` which needs to be unpacked to
get the value out, here we use `maybe`.

```
main = do
    conn <- Network.Riak.Basic.connect Network.Riak.Basic.Client "127.0.0.1" "8087" "test"
    ...
    b <- Network.Riak.Basic.get conn testBucket testKey Default
    maybe (print "No value") (print) b
```

To specify options for the get operation supply an alternate value in the last
position, either `Default`, `Quorum` or `One` depending on how reliable the read
should be.


### Deleting Objects From Riak

For deleting data from Riak, simple call the `delete` function:

```
main = do
    conn <- Network.Riak.Basic.connect Network.Riak.Basic.Client "127.0.0.1" "8087" "test"
    ...
    Network.Riak.Basic.delete conn testBucket testKey Default
```

Again we're using a default value for the quorum and you'll notice the function
doesn't return anything.

### Next steps

For documentation on all the available functions in the Riak Haskell Client,
checkout the [project page](https://github.com/markhibberd/riak-haskell-client)
and the [hackage page](https://hackage.haskell.org/package/riak).
