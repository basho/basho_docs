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

A fully functional Riak CS system is comprised of Riak CS, Stanchion,
and Riak. The supported operating systems include Ubuntu, CentOS,
Fedora, Solaris, SmartOS, FreeBSD, and OS X. Riak CS is *not* supported
on Microsoft Windows.

You can install Riak CS on a single node or using an automated
deployment tool. Any Riak CS installation involves three components, all
of which must be installed separately:

* [Riak](http://docs.basho.com/riak/latest/) --- The distributed
  database on top of which Riak CS is built
* Riak CS itself
* [Stanchion](https://github.com/basho/stanchion) --- An application
  used to manage globally unique entities, such as users and buckets

[[Riak|Installing Riak CS#Installing-Riak]] and [[Riak CS|Installing
Riak CS#Installing-Riak-CS-on-a-Node]] must be installed on each node in
your cluster. [[Stanchion|Installing Riak
CS#Installing-Stanchion-on-a-Node]], however, needs to be installed on
only one node.

## Version Compatibility

The following combinations of Riak CS, Riak, and Stanchion are known to
function well in production environments.

Riak | Riak CS | Stanchion
:----|:--------|:---------
1.2.1 | 1.2.2 | 1.2.2
1.2.1 | 1.3.0 | 1.3.0
1.3.0 | 1.2.2 | 1.2.2
1.4.0 | 1.4.0 | 1.4.0
1.4.1 | 1.4.0 | 1.4.0
1.4.8 | 1.4.3 | 1.4.5
1.4.10 | 1.5.0 | 1.5.0

We strongly recommend using only one of the version combinations listed
above when installing and running Riak CS.

## Installing Riak

Riak itself must be installed on each node in your cluster. The first
step in installing Riak is to make sure that you have Erlang installed.
Instructions for all supported operating systems can be found in
[[Installing Erlang]].

Once Erlang has been installed on a node, you can install Riak either as
part of an OS-specific package or from source.

* [[Debian and Ubuntu|Installing on Debian and Ubuntu]]
* [[RHEL and CentOS|Installing on RHEL and CentOS]]
* [[Mac OS X|Installing on Mac OS X]]
* [[FreeBSD|Installing on FreeBSD]]
* [[SmartOS|Installing on SmartOS]]
* [[Solaris|Installing on Solaris]]
* [[SUSE|Installing on SUSE]]
* [[From Source|Installing Riak From Source]]

Riak is also officially supported on the following public cloud
infrastuctures:

* [[Amazon Web Services|Installing on AWS Marketplace]]
* [[Microsoft Azure|Installing on Windows Azure]]

Remember that you must repeat this installation process on each node in
your cluster. For future reference, you should make note of the
installation directory that you have used. On Debian, Ubuntu, RHEL, and
CentOS, for example, Riak is installed in `rel/riak` by default, whereas
Mac OS X does not have a default install directory.

## Installing Riak CS on a Node

Riak CS and Stanchion packages are available on the [[Download Riak CS]]
page. Similarly, Riak packages are available on the [Download Riak]
(http://docs.basho.com/riak/latest/downloads/) page.

After downloading Riak CS, Stanchion, and Riak, install them using your
operating system's package management commands.

<div class="note">
<div class="title">Note on Riak CS and public ports</div>
<strong>Riak CS is not designed to function directly on TCP port 80, and
it should not be operated in a manner that exposes it directly to the
public internet</strong>. Instead, consider a load-balancing solution
such as a dedicated device <a href="http://haproxy.1wt.eu">HAProxy</a>
or <a href="http://wiki.nginx.org/Main">Nginx</a> between Riak CS and
the outside world.
</div>

### Installing Riak CS on Mac OS X

To install Riak CS on OS X, first download the appropriate package from
the [[downloads|Download Riak CS]] page:

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/riak-cs/1.4/{{VERSION}}/osx/10.8/riak-cs-{{VERSION}}-OSX-x86_64.tar.gz
```

Then, unpack the downloaded tarball:

```bash
tar -xvzf riak-cs-{{VERSION}}-OSX-x86_64.tar.gz
```

At this point, you can move on to [[configuring Riak CS]].

### Installing Riak CS on Debian or Ubuntu

On Debian or Ubuntu, you can either use `apt` or install the `.deb`
package manually.

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

At this point, you can move on to [[installing Stancion|Installing Riak
CS#Installing-Stanchion-on-a-Node]].

#### Installing the `.deb` Package Manually (not recommended)

To install manually, use `dpkg`:

```bash
sudo dpkg -i <riak-cs-package.deb>
```

Replace `<riak-cs-package.deb>` with the actual filename for the package
you are installing.

At this point, you can move on to [[installing Stancion|Installing Riak
CS#Installing-Stanchion-on-a-Node]].

### Installing Riak CS on RHEL or CentOS

On RHEL or CentOS, you can either use `yum` or install the `.rpm`
package manually.

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

At this point, you can move on to [[installing Stancion|Installing Riak
CS#Installing-Stanchion-on-a-Node]].

#### Installing the `.rpm` Package Manually (not recommended)

```bash
rpm -Uvh <riak-cs-package.rpm>
```

Replace `<riak-cs-package.rpm>` with the actual filename for the package
you are installing.

At this point, you can move on to [[installing Stancion|Installing Riak
CS#Installing-Stanchion-on-a-Node]].

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
to create user accounts and buckets or the inability to enfore their
uniqueness.

Because only one Stanchion instance can be used at any given time, it's
not uncommon for a load balancer to be used to handle Stanchion failover
in the event that the primary Stanchion node becomes unavailable. You
can achieve this by specifying a load balancer IP as the Stanchion UP
in each Riak CS node's `app.config`. This load balancer must be
configured to send all requests to a single Stanchion node, failing over
to a secondary Stanchion node if the primary is unavailable. More
details can be found in [[Specifying the Stanchion Node|Configuring Riak
CS#Specifying-the-Stanchion-Node]].

### Installing Stanchion on Mac OS X

First, download the appropriate package from the [[downloads|Download
Riak CS#Stanchion-1-4-3]] page.

```bash
curl -O http://s3.amazonaws.com/downloads.basho.com/stanchion/1.4/1.4.3/osx/10.8/stanchion-1.4.3-OSX-x86_64.tar.gz
```

Then, unpack the downloaded tarball:

```bash
stanchion-1.4.3-OSX-x86_64.tar.gz
```

At this point, you can move on to [[configuring Riak CS]].

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

Now, install Riak CS:

```bash
sudo apt-get install stanchion
```

At this point, you can move on to [[configuring Riak CS]].

#### Installing the `.deb` Package Manually (not recommended)

```bash
sudo dpkg -i <stanchion-package.deb>
```

Replace `<riak-cs-package.deb>` with the actual filename for the package
you are installing.

At this point, you can move on to [[configuring Riak CS]].

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

At this point, you can move on to [[configuring Riak CS]].

#### Installing the `.rpm` Package Manually (not recommended)

```bash
sudo rpm -Uvh <stanchion-package.rpm>
```

Replace `<stanchion-package.rpm>` with the actual filename for the
package you are installing.

At this point, you can move on to [[configuring Riak CS]].

<div class="note">
<div class="title">Note on SELinux</div>
CentOS enables Security-Enhanced Linux (SELinux) by default. If you
encounter errors during installation, try disabling SELinux.
</div>

## What's Next?

Once you've completed installation of Riak CS and Riak, you're ready to
learn more about [[Configuring Riak CS]].
