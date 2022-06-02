---
title: "Installing Riak CS"
description: ""
menu:
  riak_cs-2.0.0:
    name: "Installing Riak CS"
    identifier: "installing"
    weight: 200
    parent: "index"
project: "riak_cs"
project_version: "2.0.0"
aliases:
  - /riakcs/2.0.0/cookbooks/installing/Installing-Riak-CS/
  - /riak/cs/2.0.0/cookbooks/installing/Installing-Riak-CS/
---

Riak CS is supported on a variety of operating systems, including
Ubuntu, CentOS, Fedora, Solaris, SmartOS, FreeBSD, and OS X. Riak CS is
*not* supported on Microsoft Windows.

You can install Riak CS on a single node (for development purposes) or
using an automated deployment tool. Any Riak CS installation involves
three components, all of which must be installed separately:

* [Riak KV]({{<baseurl>}}riak/kv/2.0.7/) --- The distributed database on top of which Riak CS 
is built
* Riak CS itself
* [Stanchion]({{<baseurl>}}riak/cs/2.0.0/theory/stanchion) --- An application used to manage [globally unique entities]({{<baseurl>}}riak/cs/2.0.0/theory/stanchion/#globally-unique-entities) such as users and buckets.

[Riak KV](#installing-riak) and [Riak CS](#installing-riak-cs-on-a-node) must be installed on each node in your cluster. [Stanchion](#installing-stanchion-on-a-node), however, needs to be installed on only one node.

## Version Compatibility

We strongly recommend using one of the documented [version combinations]({{<baseurl>}}riak/cs/2.0.0/cookbooks/version-compatibility/) 
when installing and running Riak CS.

## Installing Riak KV

Before installing Riak CS, Riak KV must be installed on each node in
your cluster. You can install Riak KV either as part of an OS-specific package
or from source.

  * [Debian and Ubuntu]({{<baseurl>}}riak/kv/2.0.7/setup/installing/debian-ubuntu)
  * [RHEL and CentOS]({{<baseurl>}}riak/kv/2.0.7/setup/installing/rhel-centos)
  * [Mac OS X]({{<baseurl>}}riak/kv/2.0.7/setup/installing/mac-osx)
  * [FreeBSD]({{<baseurl>}}riak/kv/2.0.7/setup/installing/freebsd)
  * [SUSE]({{<baseurl>}}riak/kv/2.0.7/setup/installing/suse)
  * [From Source]({{<baseurl>}}riak/kv/2.0.7/setup/installing/source)

Riak KV is also officially supported on the following public cloud
infrastructures:

  * [Windows Azure]({{<baseurl>}}riak/kv/2.0.7/setup/installing/windows-azure)
  * [AWS Marketplace]({{<baseurl>}}riak/kv/2.0.7/setup/installing/amazon-web-services)

Remember that you must repeat this installation process on each node in
your cluster. For future reference, you should make note of the Riak KV
installation directory.

If you want to fully configure Riak KV prior to installing Riak CS, see our
documentation on [configuring Riak KV for CS]({{<baseurl>}}riak/cs/2.0.0/cookbooks/configuration/riak-for-cs/).

## Installing Riak CS on a Node

Riak CS and Stanchion packages are available on the [Download Riak CS]({{<baseurl>}}riak/cs/2.0.0/downloads/)
page. Similarly, Riak packages are available on the [Download Riak KV]({{<baseurl>}}riak/kv/2.0.7/downloads/) page.

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
the [downloads]({{<baseurl>}}riak/cs/2.0.0/downloads) page:

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak-cs/1.5/2.0.0/osx/10.8/riak-cs-2.0.0-OSX-x86_64.tar.gz
```

Then, unpack the downloaded tarball:

```bash
tar -xvzf riak-cs-2.0.0-OSX-x86_64.tar.gz
```

At this point, you can move on to [configuring Riak CS]({{<baseurl>}}riak/cs/2.0.0/cookbooks/configuration/riak-cs/).

### Installing Riak CS on Debian or Ubuntu

On Debian and Ubuntu, Riak CS packages are hosted on
[packagecloud.io](https://packagecloud.io/basho/riak-cs). Instructions
for installing via shell scripts, manual installation, Chef, and Puppet
can be found in packagecloud's [installation docs](https://packagecloud.io/basho/riak/install).

Platform-specific pages are linked below:

* [Lucid](https://packagecloud.io/basho/riak-cs/riak-cs_2.0.0-1_amd64.deb?distro=lucid)
* [Precise](https://packagecloud.io/basho/riak-cs/riak-cs_2.0.0-1_amd64.deb?distro=precise)
* [Squeeze](https://packagecloud.io/basho/riak-cs/riak-cs_2.0.0-1_amd64.deb?distro=squeeze)
* [Trusty](https://packagecloud.io/basho/riak-cs/riak-cs_2.0.0-1_amd64.deb?distro=trusty)
* [Wheezy](https://packagecloud.io/basho/riak-cs/riak-cs_2.0.0-1_amd64.deb?distro=wheezy)

#### Advanced apt Installation

For the simplest installation process on LTS (Long-Term Support)
releases, use `apt-get`. First, you must retrieve the signing key:

```curl
curl https://packagecloud.io/gpg.key | sudo apt-key add -
```

Second, you must install the `apt-transport-https` package in order to
be able to fetch packages over HTTPS:

```curl
sudo apt-get install -y apt-transport-https
```

With HTTPS enabled, we recommend adding the desired Riak CS package to
your `.list` file. packagecloud can autogenerate such a file on the
basis of a name that you specify, e.g. a hostname, and the desired
operating system and distribution. The following example script would
store your hostname in the variable `HOSTNAME`, send that information to
packagecloud to autogenerate a `.list` file, and then store the return
value in a file called `basho.list`, which is stored in the
`/etc/apt/sources.list.d` directory. This example script is specific to
the Precise Ubuntu distribution:

```bash
#!/bin/bash

HOSTNAME=`hostname -f`
FILENAME=/etc/apt/sources.list.d/basho.list
OS=ubuntu
DIST=precise
PACKAGE_CLOUD_RIAK_CS_DIR=https://packagecloud.io/install/repositories/basho/riak-cs
curl "${PACKAGE_CLOUD_RIAK_CS_DIR}/config_file.list?os=${OS}&dist=${DIST}&name=${HOSTNAME}" > $FILENAME
```

The `name` that you submit to packagecloud can be anything you like. The
`HOSTNAME` used above was for example purposes. The resulting file
should hold contents like the following:

```
# this file was generated by packagecloud.io for
# the repository at https://packagecloud.io/basho/riak

deb https://packagecloud.io/basho/riak-cs/ubuntu/ precise main
deb-src https://packagecloud.io/basho/riak-cs/ubuntu/ precise main
```

With your `basho.list` file populated, you can update your apt sources
list:

```bash
sudo apt-get update
```

Now install the `riak-cs` package:

```bash
sudo apt-get install riak-cs
```

### Installing Riak CS on RHEL or CentOS


On RHEL or CentOS, Riak CS packages are hosted on
[packagecloud.io](https://packagecloud.io/basho/riak-cs). Instructions
for installing via shell scripts, manual installation, Chef, and Puppet
can be found in packagecloud's [installation
docs](https://packagecloud.io/basho/riak-cs/install).

Platform-specific pages are linked below:

* [el5](https://packagecloud.io/basho/riak-cs/riak-cs-2.0.0-1.x86_64.rpm?distro=5)
* [el6](https://packagecloud.io/basho/riak-cs/packages/el/6/riak-cs-2.0.0-1.el6.x86_64.rpm)
<!-- * [el7](https://packagecloud.io/basho/riak-cs/riak-cs-2.0.0-1.x86_64.rpm?distro=7) -->
* [Fedora 19](https://packagecloud.io/basho/riak-cs/riak-cs-2.0.0-1.fc19.x86_64.rpm?distro=19)

#### Advanced rpm Installation

For the simplest installation process on LTS (Long-Term Support)
releases, use yum. First, you must install the `pygpgme` package, which
enables yum to handle [GPG](https://www.gnupg.org/) signatures:

```bash
sudo yum install pygpgme
```

If you wish to install using a `.repo` file, packagecloud can generate
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
baseurl=https://packagecloud.io/basho/riak-cs/el/5/$basesearch
repo_gpgcheck=1
gpgcheck=0
enabled=1
gpgkey=https://packagecloud.io/gpg.key
sslverify=1
sslcacert=/etc/pki/tls/certs/ca-bundle.crt
```

With your `basho.repo` file populated, you can update your rpm sources
list.

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
details can be found in [Specifying the Stanchion Node]({{<baseurl>}}riak/cs/2.0.0/cookbooks/configuration/#specifying-the-stanchion-node).

### Installing Stanchion on Mac OS X

First, download the appropriate package from the [downloads]({{<baseurl>}}riak/cs/2.0.0/downloads/#stanchion-1-4-3) page.

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/stanchion/1.4/1.4.3/osx/10.8/stanchion-2.0.0-OSX-x86_64.tar.gz
```

Then, unpack the downloaded tarball:

```bash
stanchion-2.0.0-OSX-x86_64.tar.gz
```

At this point, you can move on to [configuring Riak CS]({{<baseurl>}}riak/cs/2.0.0/cookbooks/configuration/riak-cs).

### Installing Stanchion on Debian or Ubuntu

On Debian or Ubuntu, you can either use `apt` or install the `.deb`
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

At this point, you can move on to [configuring Riak CS]({{<baseurl>}}riak/cs/2.0.0/cookbooks/configuration/riak-cs).

#### Installing the `.deb` Package Manually (not recommended)

```bash
sudo dpkg -i <stanchion-package.deb>
```

Replace `<riak-cs-package.deb>` with the actual filename for the package
you are installing.

At this point, you can move on to [configuring Riak CS]({{<baseurl>}}riak/cs/2.0.0/cookbooks/configuration/riak-cs).

### Installing Stanchion on RHEL or CentOS

On RHEL or CentOS, you can either use `yum` or install the `.rpm`
package manually.

#### Installing Using `yum` (recommended)

For CentOS/RHEL 6:

```bash
sudo yum install http://yum.basho.com/gpg/basho-release-6-1.noarch.rpm
```

For CentOS/RHEL 5:

```
sudo yum install http://yum.basho.com/gpg/basho-release-5-1.noarch.rpm
```

Once the `.rpm` package has been installed, install Stanchion:

```basho
sudo yum install stanchion
```

At this point, you can move on to [configuring Riak CS]({{<baseurl>}}riak/cs/2.0.0/cookbooks/configuration/riak-cs).

#### Installing the `.rpm` Package Manually (not recommended)

```bash
sudo rpm -Uvh <stanchion-package.rpm>
```

Replace `<stanchion-package.rpm>` with the actual filename for the
package you are installing.

At this point, you can move on to [configuring Riak CS]({{<baseurl>}}riak/cs/2.0.0/cookbooks/configuration/riak-cs).

> **Note on SELinux**
>
> CentOS enables Security-Enhanced Linux (SELinux) by default. If you
encounter errors during installation, try disabling SELinux.

## What's Next?

Once you've completed installation of Riak CS and Riak, you're ready to
learn more about [configuring Riak CS]({{<baseurl>}}riak/cs/2.0.0/cookbooks/configuration/riak-cs).
