---
title: "Installing Riak CS"
description: ""
menu:
  riak_cs-3.0.1:
    name: "Installing Riak CS"
    identifier: "installing"
    weight: 200
    parent: "index"
project: "riak_cs"
project_version: "3.0.1"
aliases:
  - /riakcs/3.0.1/cookbooks/installing/Installing-Riak-CS/
  - /riak/cs/3.0.1/cookbooks/installing/Installing-Riak-CS/
  - /riak/cs/latest/cookbooks/installing/
---

Riak CS is supported on a variety of operating systems, including
Alpine, Amazon Linux, Ubuntu, Debian, CentOS, Oracle, SUSE, FreeBSD, and OS X. Riak CS is
*not* supported on Microsoft Windows.

You can install Riak CS on a single node (for development purposes) or
using an automated deployment tool. Any Riak CS installation involves
three components, all of which must be installed separately:

* [Riak KV]({{<baseurl>}}riak/kv/3.0.9/) --- The distributed database on top of which Riak CS 
is built
* Riak CS itself
* [Stanchion]({{<baseurl>}}riak/cs/3.0.1/theory/stanchion) --- An application used to manage [globally unique entities]({{<baseurl>}}riak/cs/3.0.1/theory/stanchion/#globally-unique-entities) such as users and buckets.

[Riak KV](#installing-riak-kv) and [Riak CS](#installing-riak-cs-on-a-node) must be installed on each node in your cluster. [Stanchion](#installing-stanchion-on-a-node), however, needs to be installed on only one node.

## Version Compatibility

We strongly recommend using one of the documented [version combinations]({{<baseurl>}}riak/cs/3.0.1/cookbooks/version-compatibility/)
when installing and running Riak CS.

## Installing Riak KV

Before installing Riak CS, Riak KV must be installed on each node in
your cluster. You can install Riak KV either as part of an OS-specific package
or from source.

  * [Debian and Ubuntu]({{<baseurl>}}riak/kv/3.0.9/setup/installing/debian-ubuntu)
  * [RHEL and CentOS]({{<baseurl>}}riak/kv/3.0.9/setup/installing/rhel-centos)
  * [Mac OS X]({{<baseurl>}}riak/kv/3.0.9/setup/installing/mac-osx)
  * [FreeBSD]({{<baseurl>}}riak/kv/3.0.9/setup/installing/freebsd)
  * [SUSE]({{<baseurl>}}riak/kv/3.0.9/setup/installing/sles)
  * [Raspbian]({{<baseurl>}}riak/kv/3.0.9/setup/installing/raspbian)
  * [Alpine]({{<baseurl>}}riak/kv/3.0.9/setup/installing/alpine)
  * [From Source]({{<baseurl>}}riak/kv/3.0.9/setup/installing/source)

Riak is also officially supported on the following public cloud
infrastructures:

  * [Windows Azure]({{<baseurl>}}riak/kv/3.0.9/setup/installing/windows-azure)
  * [AWS Marketplace]({{<baseurl>}}riak/kv/3.0.9/setup/installing/amazon-web-services)

Remember that you must repeat this installation process on each node in
your cluster. For future reference, you should make note of the Riak KV
installation directory.

If you want to fully configure Riak KV prior to installing Riak CS, see our
documentation on [configuring Riak KV for CS]({{<baseurl>}}riak/cs/3.0.1/cookbooks/configuration/riak-for-cs/).

## Installing Riak CS on a Node

Riak CS and Stanchion packages are available on the [Download Riak CS]({{<baseurl>}}riak/cs/3.0.1/downloads/)
page. Similarly, Riak packages are available on the [Download Riak KV]({{<baseurl>}}riak/kv/3.0.9/downloads/) page.

After downloading Riak CS, Stanchion, and Riak, install them using your
operating system's package management commands.

> **Note on Riak CS and public ports**
>
> **Riak CS is not designed to function directly on TCP port 80, and
it should not be operated in a manner that exposes it directly to the
public internet**. Instead, consider a load-balancing solution
such as a dedicated device [HAProxy](http://haproxy.1wt.eu) or [Nginx](http://wiki.nginx.org/Main) between Riak CS and the outside world.

### Installing Riak CS on Mac OS X

To install Riak CS on OS X, first download the appropriate package from
the [downloads]({{<baseurl>}}riak/cs/3.0.1/downloads) page:

```bash
curl -O https://files.tiot.jp/riak/cs/3.0/3.0.1/osx/10.14/riak_cs-3.0.1-OTP22.tar.gz
```

Then, unpack the downloaded tarball:

```bash
tar -xvzf riak_cs-3.0.1-OTP22.tar.gz
```

At this point, you can move on to [configuring Riak CS]({{<baseurl>}}riak/cs/3.0.1/cookbooks/configuration/riak-cs/).

### Installing Riak CS on Debian, Ubuntu or Raspbian

On Debian and Ubuntu, Riak CS packages are hosted on
[files.tiot.jp](https://files.tiot.jp/riak/cs/3.0/3.0.1/). 

Platform-specific pages are linked below:

* [Focal](https://files.tiot.jp/riak/cs/3.0/3.0.1/ubuntu/focal64/riak-cs_3.0.1-OTP22_amd64.deb)
* [Jammy](https://files.tiot.jp/riak/cs/3.0/3.0.1/ubuntu/jammy64/riak-cs_3.0.1-OTP22_amd64.deb)
* [Stretch](https://files.tiot.jp/riak/cs/3.0/3.0.1/debian/9/riak-cs_3.0.1-OTP22_amd64.deb)
* [Buster](https://files.tiot.jp/riak/cs/3.0/3.0.1/debian/10/riak-cs_3.0.1-OTP22_amd64.deb)
* [Bullseye](https://files.tiot.jp/riak/cs/3.0/3.0.1/debian/11/riak-cs_3.0.1-OTP22_amd64.deb)
* [Raspbian](https://files.tiot.jp/riak/cs/3.0/3.0.1/raspbian/buster/riak-cs_3.0.1-OTP22_armhf.deb)

#### dpkg Installation

For the simplest installation process on LTS (Long-Term Support)
releases, use `dpkg -i` after downloading the appropriate package for your OS.

Now install the `riak-cs` package:

```bash
sudo dpkg -i riak-cs_3.0.1-OTP22_*.deb
```

### Installing Riak CS on Amazon Linux, Oracle Linux, RHEL, CentOS or openSUSE


On Amazon Linux, Riak CS packages are hosted on
[files.tiot.jp](https://files.tiot.jp/riak/cs/3.0/3.0.1/amazon/). 

Platform-specific pages are linked below:

* [amzn2](https://files.tiot.jp/riak/cs/3.0/3.0.1/amazon/2/riak-cs-3.0.1.OTP22-1.amzn2.x86_64.rpm)

On Oracle Linux, Riak CS packages are hosted on
[files.tiot.jp](https://files.tiot.jp/riak/cs/3.0/3.0.1/oracle/). 

Platform-specific pages are linked below:

* [el8](https://files.tiot.jp/riak/cs/3.0/3.0.1/oracle/8/riak-cs-3.0.1.OTP22-1.el8.x86_64.rpm)

On RHEL or CentOS, Riak CS packages are hosted on
[files.tiot.jp](https://files.tiot.jp/riak/cs/3.0/3.0.1/rhel). 

Platform-specific pages are linked below:

* [el7](https://files.tiot.jp/riak/cs/3.0/3.0.1/rhel/7/riak-cs-3.0.1.OTP22-1.el7.x86_64.rpm)
* [el8](https://files.tiot.jp/riak/cs/3.0/3.0.1/rhel/8/riak-cs-3.0.1.OTP22-1.el8.x86_64.rpm)

On openSUSE, Riak CS packages are hosted on
[files.tiot.jp](https://files.tiot.jp/riak/cs/3.0/3.0.1/sles). 

Platform-specific pages are linked below:

* [SLES15](https://files.tiot.jp/riak/cs/3.0/3.0.1/sles/15/riak-cs-3.0.1.OTP22-1.SLES15.x86_64.rpm)

#### yum Installation (Amazon Linux, Oracle Linux, RHEL or CentOS)

For the simplest installation process on LTS (Long-Term Support)
releases, use `yum` after downloading the appropriate package for your OS.

<!--If you wish to install using a `.repo` file, packagecloud can generate

one for you on the basis of a name that you specify, e.g. a hostname,
and the desired operating system and distribution. The following example
script would store your hostname in the variable `HOSTNAME`, send that
information to packagecloud to generate a `.repo` file, and then store
the return value in a file called `basho.repo`, which is stored in the
`/etc/yum.repos.d` directory:

```bash
#!/bin/bash

HOSTNAME=`hostname -f`
FILENAME=/etc/yum.repos.d/basho.repo
OS=el
DIST=5
PACKAGE_CLOUD_RIAK_CS_DIR=https://packagecloud.io/install/repositories/basho/riak-cs
curl "${PACKAGE_CLOUD_RIAK_CS_DIR}/config_file.repo?os=${OS}&dist=${DIST}&name=${HOSTNAME}" > $FILENAME
```

The `name` that you submit to packagecloud can be anything you like. The
`HOSTNAME` used above was for example purposes. The resulting file
should hold contents like the following:

```
[basho_riak-cs]
name=basho_riak-cs
baseurl=https://files.tiot.jp/riak/cs//el/5/$basesearch
repo_gpgcheck=1
gpgcheck=0
enabled=1
gpgkey=https://packagecloud.io/gpg.key
sslverify=1
sslcacert=/etc/pki/tls/certs/ca-bundle.crt
```

With your `basho.repo` file populated, you can update your rpm sources
list. -->

```bash
sudo yum localinstall -y riak-cs-3.0.1.OTP22-1*
```
#### zypper Installation (openSUSE)

For the simplest installation process on LTS (Long-Term Support)
releases, use `zypper` after downloading the appropriate package for your OS.

```bash
sudo zypper localinstall -y riak-cs-3.0.1.OTP22-1*
```

### Installing Riak CS on FreeBSD or Alpine

On FreeBSD and Alpine, Riak CS packages are hosted on
[files.tiot.jp](https://files.tiot.jp/riak/cs/3.0/3.0.1/).

Platform-specific pages are linked below:

* [FreeBSD 13](https://files.tiot.jp/riak/cs/3.0/3.0.1/freebsd/13/riak-cs_3.0.1-OTP22.pkg)
* [Alpine 3.14](https://files.tiot.jp/riak/cs/3.0/3.0.1/alpine/3.14/riak-cs_3.0.1-r0.apk)

#### pkg Installation (FreeBSD)

For the simplest installation process on LTS (Long-Term Support)
releases, use `pkg -i` after downloading the appropriate package for your OS..

Now install the `riak-cs` package:

```bash
sudo pkg -i riak-cs_3.0.1-OTP22.pkg
```

#### apk Installation (Alpine)

For the simplest installation process on LTS (Long-Term Support)
releases, use `apk add` after downloading the appropriate package for your OS..

Now install the `riak-cs` package:

```bash
sudo apk add riak-cs_3.0.1-r0.apk
```

## Installing Stanchion on a Node

Stanchion is an application that manages globally unique entities within
a Riak CS cluster. It performs actions such as ensuring unique user
accounts and bucket names across the whole system. **Riak CS cannot be
used without Stanchion**.

All Riak CS nodes must be configured to communicate with a single
Stanchion node. Although multiple Stanchion instances may be installed
and running within a cluster, even one on each node, only one may be
actively used by the cluster. Running multiple instances of Stanchion
simultaneously can produce a variety of problems such as the inability
to create user accounts and buckets or the inability to enforce their
uniqueness.

Because only one Stanchion instance can be used at any given time, it's
not uncommon for a load balancer to be used to handle Stanchion failover
in the event that the primary Stanchion node becomes unavailable. You
can achieve this by specifying a load balancer IP as the Stanchion IP
in each Riak CS node's `riak-cs.conf`. This load balancer must be
configured to send all requests to a single Stanchion node, failing over
to a secondary Stanchion node if the primary is unavailable. More
details can be found in [Specifying the Stanchion Node]({{<baseurl>}}riak/cs/3.0.1/cookbooks/configuration/stanchion).

### Installing Stanchion on Mac OS X

First, download the appropriate package from the [downloads]({{<baseurl>}}riak/cs/3.0.1/downloads/#stanchion-3-0-0) page.

```bash
curl -O https://files.tiot.jp/riak/stanchion/3.0/3.0.1/osx/10.14/stanchion-3.0.1-OTP22.tar.gz
```

Then, unpack the downloaded tarball:

```bash
tar -xvf stanchion-3.0.1-OTP22.tar.gz
```

At this point, you can move on to [configuring Riak CS]({{<baseurl>}}riak/cs/3.0.1/cookbooks/configuration/riak-cs).

### Installing Stanchion on Debian or Ubuntu

<!-- On Debian or Ubuntu, you can either use `apt` or install the `.deb`
package manually.

#### Installing Using `apt` (recommended)

First, install the signing key:

```curl
curl http://apt.basho.com/gpg/basho.apt.key | sudo apt-key add -
```

If the signing key and `apt` repository have already been added, add
the Basho repository to your `apt` sources list (and update them):

```bash
sudo bash -c "echo deb http://apt.basho.com $(lsb_release -sc) main > /etc/apt/sources.list.d/basho.list"
sudo apt-get update
```

Now, install the `stanchion` package:

```bash
sudo apt-get install stanchion
```

At this point, you can move on to [configuring Riak CS]({{<baseurl>}}riak/cs/3.0.1/cookbooks/configuration/riak-cs).

#### Installing the `.deb` Package Manually (not recommended)
-->
On Debian and Ubuntu, Riak CS packages are hosted on
[files.tiot.jp](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/).

Platform-specific pages are linked below:

* [Focal](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/ubuntu/focal64/riak-cs_3.0.1-OTP22_amd64.deb)
* [Jammy](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/ubuntu/jammy64/riak-cs_3.0.1-OTP22_amd64.deb)
* [Stretch](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/debian/9/riak-cs_3.0.1-OTP22_amd64.deb)
* [Buster](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/debian/10/riak-cs_3.0.1-OTP22_amd64.deb)
* [Bullseye](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/debian/11/stanchion_3.0.1-OTP22_amd64.deb)
* [Raspbian](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/raspbian/buster/stanchion_3.0.1-OTP22_armhf.deb)

#### dpkg Installation

For the simplest installation process on LTS (Long-Term Support)
releases, use `dpkg -i` after downloading the appropriate package for your OS..

Now install the `stanchion` package:

-->
On Debian and Ubuntu, Riak CS packages are hosted on
[files.tiot.jp](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/).

Platform-specific pages are linked below:

* [Focal](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/ubuntu/focal64/stanchion_3.0.1-OTP22_amd64.deb)
* [Jammy](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/ubuntu/jammy64/stanchion_3.0.1-OTP22_amd64.deb)
* [Stretch](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/debian/9/stanchion_3.0.1-OTP22_amd64.deb)
* [Buster](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/debian/10/stanchion_3.0.1-OTP22_amd64.deb)
* [Bullseye](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/debian/11/stanchion_3.0.1-OTP22_amd64.deb)
* [Raspbian](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/raspbian/buster/stanchion_3.0.1-OTP22_armhf.deb)

#### dpkg Installation

For the simplest installation process on LTS (Long-Term Support)
releases, use `dpkg -i` after downloading the appropriate package for your OS..

Now install the `stanchion` package:

```bash
sudo dpkg -i stanchion_3.0.1-OTP22_*.deb
```

At this point, you can move on to [configuring Riak CS]({{<baseurl>}}riak/cs/3.0.1/cookbooks/configuration/riak-cs).

### Installing Stanchion on Amazon Linux, Oracle Linux, RHEL, CentOS or openSUSE

On Amazon Linux, Stanchion packages are hosted on
[files.tiot.jp](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/amazon/).

Platform-specific pages are linked below:

* [amzn2](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/amazon/2/stanchion-3.0.1.OTP22-1.amzn2.x86_64.rpm)

On Oracle Linux, Stanchion packages are hosted on
[files.tiot.jp](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/oracle/).

Platform-specific pages are linked below:

* [el8](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/oracle/8/stanchion-3.0.1.OTP22-1.el8.x86_64.rpm)

On RHEL or CentOS, Stanchion packages are hosted on
[files.tiot.jp](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/rhel).

Platform-specific pages are linked below:

* [el7](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/rhel/7/stanchion-3.0.1.OTP22-1.el7.x86_64.rpm)
* [el8](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/rhel/8/stanchion-3.0.1.OTP22-1.el8.x86_64.rpm)

On openSUSE, Stanchion packages are hosted on
[files.tiot.jp](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/sles).

Platform-specific pages are linked below:

* [SLES15](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/sles/15/stanchion-3.0.1.OTP22-1.SLES15.x86_64.rpm)

#### yum Installation (Amazon Linux, Oracle Linux, RHEL or CentOS)

For the simplest installation process on LTS (Long-Term Support)
releases, use `yum` after downloading the appropriate package for your OS.

```bash
sudo yum -localinstall -y stanchion-3.0.1*
```

#### zypper Installation (openSUSE)

For the simplest installation process on LTS (Long-Term Support)
releases, use `zypper` after downloading the appropriate package for your OS.

```bash
sudo zypper localinstall -y stanchion-3.0.1.OTP22-1*
```

At this point, you can move on to [configuring Riak CS]({{<baseurl>}}riak/cs/3.0.1/cookbooks/configuration/riak-cs).

> **Note on SELinux**
>
> CentOS enables Security-Enhanced Linux (SELinux) by default. If you
encounter errors during installation, try disabling SELinux.

### Installing Stanchion on FreeBSD or Alpine

On FreeBSD and Alpine, Stanchion packages are hosted on
[files.tiot.jp](https://files.tiot.jp/riak/cs/3.0/3.0.1/).

Platform-specific pages are linked below:

* [FreeBSD 13](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/freebsd/13/stanchion_3.0.1-OTP22.pkg)
* [Alpine 3.14](https://files.tiot.jp/riak/stanchion/3.0/3.0.1/alpine/3.14/stanchion_3.0.1-r0.apk)

#### pkg Installation (FreeBSD)

For the simplest installation process on LTS (Long-Term Support)
releases, use `pkg -i` after downloading the appropriate package for your OS..

Now install the `stanchion` package:

```bash
sudo pkg -i stanchion_3.0.1-OTP22.pkg
```

#### apk Installation (Alpine)

For the simplest installation process on LTS (Long-Term Support)
releases, use `apk add` after downloading the appropriate package for your OS..

Now install the `stanchion` package:

```bash
sudo apk add stanchion_3.0.1-r0.apk
```

## What's Next?

Once you've completed installation of Riak CS and Riak, you're ready to
learn more about [configuring Riak CS]({{<baseurl>}}riak/cs/3.0.1/cookbooks/configuration/riak-cs).
