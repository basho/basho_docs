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
curl -O <riak-cs-os-x.tar.gz>
```

Then, unpack the downloaded tarball:

```bash
tar -xvf <riak-cs-os-x.tar.gz>
```

Replace `<riak-cs-os-x.tar.gz>` with the actual filename for the package you are installing.

### Installing Riak CS on Ubuntu

The following command installs Riak CS on a machine running either Debian or Ubuntu:

```bash
sudo dpkg -i <riak-cs-package.deb>
```

Replace `<riak-cs-package.deb>` with the actual filename for the package you are installing.

### Installing Riak CS on CentOS

The following command installs Riak CS on a machine running CentOS:

```bash
rpm -Uvh <riak-cs-package.rpm>
```

Replace `<riak-cs-package.rpm>` with the actual filename for the package you are installing.

## Installing Stanchion

In a Riak CS system, Stanchion is installed on only one of the nodes in the system. Running Stanchion on more than one node can lead to problems if Riak CS nodes are configured to communicate using multiple Stanchion nodes. In this situation, the uniqueness of bucket names and user email addresses might not be enforced which, in turn, could lead to unexpected behavior. Use the commands in the section for your operating system to install a pre-built Stanchion package on the node you choose for Stanchion.

### Installing Stanchion on Mac OS X

To install Stanchion on OS X, first download the appropriate package from the [[downloads|Download Riak CS]] page:

```bash
curl -O <stanchion-os-x.tar.gz>
```

Replace `<stanchion-os-x.tar.gz>` with the actual filename for the package you are installing.

Then, unpack the downloaded tarball:

```bash
tar -xvf <stanchion-os-x.tar.gz>
```

### Installing Stanchion on Ubuntu

The following command installs Stanchion on a machine running Ubuntu:

```bash
sudo dpkg -i <riak-cs-package.deb>
```

Replace `<riak-cs-package.deb>` with the actual filename for the package you are installing.

### Installing Stanchion on CentOS

The following command installs Stanchion on a machine running either Red Hat linux or CentOS:

```bash
sudo rpm -Uvh <stanchion-package.rpm>
```

Replace `<stanchion-package.rpm>` with the actual filename for the package you are installing.

<div class="note"><div class="title">Note</div>CentOS enables Security-Enhanced Linux (SELinux) by default. If you encounter errors during installation, try disabling SELinux.</div>

## Installing Riak
If you have not yet installed Riak, follow the [[Riak Installation|Installing and Upgrading]] documentation to do so.

## What's Next?
Once you've completed installation of Riak CS and Riak, you're ready to learn more about [[Configuring Riak CS]].
