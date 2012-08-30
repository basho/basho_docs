---
title: Installing Riak CS
project: riakcs
version: 0.10.0+
document: tutorial
toc: true
index: true
audience: beginner
keywords: [installation]
---

# Installing Riak CS

A fully functional Riak CS system is comprised of Riak CS, Stanchion, and Riak. Riak CS runs only on 64-bit platforms. The supported operating systems include Ubuntu 10.04, Ubuntu 11.04, CentOS 5, and CentOS 6. Riak CS is not supported on Microsoft Windows. You can install Riak CS on a single node or using an automated deployment tool.

For those of you like videos, here's a [[video|http://player.vimeo.com/video/42654313]] that demonstrates a typical Riak CS installation.

## Installing Riak CS on a Node

As a licensed Riak CS customer, you can use your Basho provided credentials to access Riak CS from the [downloads](https://help.basho.com/forums/20747106-riak-cs-downloads) section of the Basho help desk website.

After downloading Riak EE, Stanchion, and Riak CS, install them using your operating system's package management commands.

### Installing Riak CS on Ubuntu

The following command installs Riak CS on a machine running either Debian or Ubuntu.

```bash
sudo dpkg -i <riak-cs-package.deb>
```

Replace `<riak-cs-package.deb>` with the actual file name for the package you are installing.

### Installing Riak CS on CentOS

The following command installs Riak CS on a machine running CentOS.

```bash
rpm -Uvh <riak-cs-package.rpm>
```

Replace `<riak-cs-package.rpm>` with the actual file name for the package you are installing.

## Installing Stanchion

In a Riak CS system, Stanchion is installed on only one of the nodes in the system. Running Stanchion on more than one node can lead to problems if Riak CS nodes are configured to communicate using multiple Stanchion nodes. In this situation, the uniqueness of bucket names and user email addresses might not be enforced, which, in turn, could lead to unexpected behavior. Use the commands in the section for your operating system to install a pre-built Stanchion package on the node you choose for Stanchion.

### Installing Stanchion on Ubuntu

The following command installs Stanchion on a machine running Ubuntu.

```bash
sudo dpkg -i <riak-cs-package.deb>
```

Replace `<riak-cs-package.deb>` with the actual file name for the package you are installing.

### Installing Riak CS on CentOS

The following command installs Stanchion on a machine running either Red Hat linux or CentOS.

```bash
sudo rpm -Uvh <stanchion-package.rpm>
```

Replace `<stanchion-package.rpm>` with the actual file name for the package you are installing.


<div class="note">
<div class="title">Note</div>

CentOS enables Security-Enhanced Linux (SELinux) by default. If you encounter errors during installation, try disabling SELinux.

</div>

##Installing Riak

If you have not yet installed Riak, follow the [[Riak Installation|http://wiki.basho.com/Installation.html]] documentation to do so.
