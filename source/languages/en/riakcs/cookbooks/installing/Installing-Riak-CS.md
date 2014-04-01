---
title: Installing Riak CS
project: riakcs
version: 0.10.0+
document: tutorial
toc: true
index: true
audience: beginner
keywords: [installing]
---

A fully functional Riak CS system is comprised of Riak CS, Stanchion, and Riak. The supported operating systems include Ubuntu, CentOS, Fedora, Solaris, SmartOS, FreeBSD, and OS X. Riak CS is *not* supported on Microsoft Windows. You can install Riak CS on a single node or using an automated deployment tool.

## Installing Riak CS on a Node

Riak CS and Stanchion packages are available on the [[Download Riak CS]] page. Similarly, Riak packages are available on the [Download Riak](http://docs.basho.com/riak/latest/downloads/) page.

After downloading Riak CS, Stanchion, and Riak, install them using your operating system's package management commands.

<div class="note"><div class="title">Note</div><strong>Riak CS is not designed to function directly on TCP port 80, and it should not be operated in a manner that exposes it directly to the public internet</strong>. Instead, consider a load-balancing solution, such as a dedicated device <a href="http://haproxy.1wt.eu">HAProxy</a> or <a href="http://wiki.nginx.org/Main">Nginx</a> between Riak CS and the outside world.
</div>

### Installing Riak CS on Mac OS X

To install Riak CS on OS X, first download the appropriate package from the [[downloads|Download Riak CS]] page:

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/<riak-cs-os-x.tar.gz>
```

Then, unpack the downloaded tarball:

```bash
tar -xvzf <riak-cs-os-x.tar.gz>
```

Replace `<riak-cs-os-x.tar.gz>` with the actual filename for the package you are installing.

### Installing Riak CS on Debian or Ubuntu

On Debian or Ubuntu, you can either use `apt` or install the `.deb` package manually.

#### Installing Using `apt` (recommended)

First, install the signing key:

```curl
curl http://apt.basho.com/gpg/basho.apt.key | sudo apt-key add -
```

Then, add the Basho repository to your `apt` sources list (and update them):

```bash
sudo bash -c "echo deb http://apt.basho.com $(lsb_release -sc) main > /etc/apt/sources.list.d/basho.list"
sudo apt-get update
```

Now, install Riak CS:

```bash
sudo apt-get install riak-cs
```

#### Installing the `.deb` Package Manually (not recommended)

To install manually, use `dpkg`:

```bash
sudo dpkg -i <riak-cs-package.deb>
```

Replace `<riak-cs-package.deb>` with the actual filename for the package you are installing.

### Installing Riak CS on RHEL or CentOS

On RHEL or CentOS, you can either use `yum` or install the `.rpm` package manually.

#### Installing Using `yum` (recommended)

For CentOS/RHEL 6:

```bash
sudo yum install http://yum.basho.com/gpg/basho-release-6-1.noarch.rpm
```

For CentOS/RHEL 5:

```bash
sudo yum install http://yum.basho.com/gpg/basho-release-5-1.noarch.rpm
```

Once the `.rpm` package has been installed, install Riak CS:

```bash
sudo yum install riak-cs
```

#### Installing the `.rpm` Package Manually (not recommended)

```bash
rpm -Uvh <riak-cs-package.rpm>
```

Replace `<riak-cs-package.rpm>` with the actual filename for the package you are installing.

## Installing Stanchion

Stanchion is an application that ensures unique user accounts and bucket names across the whole system.

Problems can occur if all Riak CS nodes are not configured to communicate with a single Stanchion node. Although Stanchion instances may be installed and running on each node, only one may be actively used by the cluster.  This could mean the inability to create user accounts and buckets, or the inability to enforce their uniqueness.

As only a single instance of Stanchion can be used by the Riak CS cluster at any time, it's not uncommon for a load balancer to be used to handle Stanchion failover in the event the primary Stachion node becomes unavailable.  This can be accomplished by specifying a load balancer IP as the Stanchion IP in the Riak CS app.config.  This load balancer must be configured to send all requests to a single Stanchion node, failing over to a secondary Stanchion node if the primary is unavailable.  Details on specifying the Stanchion IP can be found in the [[Specifying the Stanchion Node|Configuring Riak CS#Specifying-the-Stanchion-Node]] section of [[Configuring Riak CS]].

Use the commands in the section for your operating system to install a pre-built Stanchion package on the node you choose for Stanchion.

### Installing Stanchion on Mac OS X

To install Stanchion on OS X, first download the appropriate package from the [[downloads|Download Riak CS]] page:

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/<stanchion-os-x.tar.gz>
```

Replace `<stanchion-os-x.tar.gz>` with the actual filename for the package you are installing.

Then, unpack the downloaded tarball:

```bash
tar -xvzf <stanchion-os-x.tar.gz>
```

### Installing Stanchion on Debian or Ubuntu

On Debian or Ubuntu, you can either use `apt` or install the `.deb` package manually.

#### Installing Using `apt` (recommended)

First, install the signing key:

```curl
curl http://apt.basho.com/gpg/basho.apt.key | sudo apt-key add -
```

If the signing key and `apt` repository have already been added, add the Basho repository to your `apt` sources list (and update them):

```bash
sudo bash -c "echo deb http://apt.basho.com $(lsb_release -sc) main > /etc/apt/sources.list.d/basho.list"
sudo apt-get update
```

Now, install Riak CS:

```bash
sudo apt-get install stanchion
```

#### Installing the `.deb` Package Manually (not recommended)

```bash
sudo dpkg -i <stanchion-package.deb>
```

Replace `<riak-cs-package.deb>` with the actual filename for the package you are installing.

### Installing Stanchion on RHEL or CentOS

On RHEL or CentOS, you can either use `yum` or install the `.rpm` package manually.

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

#### Installing the `.rpm` Package Manually (not recommended)

```bash
sudo rpm -Uvh <stanchion-package.rpm>
```

Replace `<stanchion-package.rpm>` with the actual filename for the package you are installing.

<div class="note"><div class="title">Note</div>CentOS enables Security-Enhanced Linux (SELinux) by default. If you encounter errors during installation, try disabling SELinux.</div>

## Installing Riak

If you have not yet installed Riak, follow the [[Riak Installation|Installing and Upgrading]] documentation to do so.

## What's Next?

Once you've completed installation of Riak CS and Riak, you're ready to learn more about [[Configuring Riak CS]].
