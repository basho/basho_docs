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
moved: {
    '1.4.0-': '/tutorials/installation/Installing-on-SUSE'
}
---

<div class="note">
<div class="title">Note: 2.0.4 not currently available</div>
Riak version 2.0.4 is not currently available for SUSE due to a known
issue. If you'd like to upgrade Riak, we'd recommend waiting for the
2.0.5 release.
</div>

The following steps should get you up and running with Riak on SuSE.

Riak may be installed on the following x86/x86_64 flavors of SuSE:

* SLES11-SP1
* OpenSUSE 11.2
* OpenSUSE 11.3
* OpenSUSE 11.4

The Riak package and all of its dependencies (including the base
Erlang) can be found in an OpenSUSE Build Service
(http://build.opensuse.org) Zypper repository.

The following commands assume that you are running as root.

## Add the Riak Zypper Repository

```bash
zypper ar http://download.opensuse.org/repositories/server:/database/$distro Riak
```

Where `$distro` is one of:

* `SLE_11_SP1`
* `openSUSE_11.2`
* `openSUSE_11.3`
* `openSUSE_11.4`

<div class="note">
<div class="title">Note on GPG keys</div>
The first time you try to use the repository after adding it to your
system, you may be asked to accept the GPG key for the repo.
</div>

## Install the Riak Package

```bash
zypper in riak
```

This should automatically pull in the `riak` package's dependencies,
including Erlang if you do not already have it installed.

## Enabling Refresh

You have the option of enabling refresh on the `riak` repository to
receive updates by running the following command:

```bash
zypper mr -r Riak
```

## Next Steps?

From here you might want to check out:

* [[Post-Installation Notes|Post Installation]]: for checking Riak
  health after installation
* [[Five-Minute Install]]: a guide that will show you how to go from one
  node to bigger than Google!
