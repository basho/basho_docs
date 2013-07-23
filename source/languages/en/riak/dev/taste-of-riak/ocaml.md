---
title: "Taste of Riak: OCaml"
project: riak
version: 1.4.0+
document: guide
toc: true
audience: beginner
keywords: [developers, client, ocaml]
---

If you haven't set up a Riak Node and started it, please visit the
[[Prerequisites|Taste of Riak: Prerequisites]] first.

To try this flavor of Riak, a working installation of [OCaml](http://ocaml.org/) with [OPAM](http://opam.ocamlpro.com/doc/Quick_Install.html) is required. 

### Client Setup

The [riak-ocaml-client](http://metadave.github.io/riak-ocaml-client/) is a community-maintained Riak client library for OCaml.

First, download the riak-ocaml-client via OPAM.

```
opam install oasis
opam install riak
```

This will download the required dependencies:

```
The following actions will be performed:
 - install ocamlfind.1.3.3 [required by riak]
 - install easy-format.1.0.1 [required by riak]
 - install lwt.2.4.3 [required by riak]
 - install ounit.1.1.2 [required by riak]
 - install ulex.1.1 [required by riak]
 - install xmlm.1.1.1 [required by riak]
 - install piqi.0.6.0 [required by riak]
 - install riak-pb.1.0.0 [required by riak]
 - install riak.1.0.0
9 to install | 0 to reinstall | 0 to upgrade | 0 to downgrade | 0 to remove
Do you want to continue ? [Y/n]
```
Press `Y` to continue.

Next, download the `taste-of-ocaml` sample project from Github:

```
git clone git@github.com:metadave/taste-of-ocaml.git
cd taste-of-ocaml
```

The `src` directory contains a single file titled `taste_of_riak.ml`.

// Connects to a Riak node at 127.0.0.1:8098

If you set up a local Riak cluster using the [[five minute install]] method,
use this code snippet instead:

```ocaml
 let pbip = 10018 in
```

We are now ready to start interacting with Riak.

### Creating Objects in Riak


### Reading Objects from Riak


### Deleting Objects from Riak

