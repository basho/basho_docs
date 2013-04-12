---
title: Installing on SUSE
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, suse]
prev: "[[Installing on Solaris]]"
up:   "[[Installing and Upgrading]]"
next: "[[Installing on Windows Azure]]"
---

The following steps should get you up and running with Riak on SuSE.

Riak may be unofficially installed on the following x86/x86_64 flavors of SuSE via community provided support:

* SLES11-SP1
* OpenSUSE 11.2
* OpenSUSE 11.3
* OpenSUSE 11.4

The Riak package and all of its dependencies (including the base Erlang) can be found in an OpenSUSE Build Service (http://build.opensuse.org) Zypper repository

(The following commands are assumed to be run as root)

## Add the Riak zypper repository

```bash
$ zypper ar http://download.opensuse.org/repositories/server:/database/$distro Riak
```
Where $distro is one of:
* SLE_11_SP1
* openSUSE_11.2
* openSUSE_11.3
* openSUSE_11.4

_NOTE: The first time you try to use the repository after adding it to your system, you may be asked to accept the GPG key for the repo._

## Install the Riak package

```bash
$ zypper in riak
```

This should automatically pull in its dependencies, including erlang if you do
not already have it installed.

## (Optional) Enable "refresh" on the riak repository to receive updates

```bash
$ zypper mr -r Riak
```

## Next Steps?

From here you might want to check out:

* [[Post Installation Notes|Post Installation]]: for checking Riak health after installation
* [[Riak Fast Track|The-Riak-Fast-Track]]: a guide for setting up a 4 node cluster and exploring Riak's main features.
* [[Basic Cluster Setup|Basic Cluster Setup]]: a guide that will show you how to go from one node to bigger than Google!
