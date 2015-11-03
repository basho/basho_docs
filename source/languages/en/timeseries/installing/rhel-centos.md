---
title: Installing on CentOS
project: riak_ts
version: 1.0.0+
document: tutorial
audience: beginner
keywords: [tutorial, installing, centos, linux]
download:
  key: rhel
  name: "CentOS"
---

Riak TS can be installed on CentOS-based systems using a binary
package.

>**Note on SELinux**
>
>CentOS enables SELinux by default, so you may need to disable SELinux if
you encounter errors.


## Installing with rpm

For versions of Riak prior to 2.0, Basho used a self-hosted
[rpm](http://www.rpm.org/) repository for CentOS and RHEL packages. For
versions 2.0 and later, Basho has moved those packages to the
[packagecloud.io](https://packagecloud.io/) hosting service.
Instructions for installing via shell scripts, manual installation,
Chef, and Puppet can be found in packagecloud's [installation
docs](https://packagecloud.io/basho/riak/install).

Platform-specific pages are linked below:

* [el5](https://packagecloud.io/basho/riak/packages/el/5/riak-{{VERSION}}-1.x86_64.rpm)
* [el6](https://packagecloud.io/basho/riak/packages/el/6/riak-{{VERSION}}-1.el6.x86_64.rpm)
* [el7](https://packagecloud.io/basho/riak/packages/el/7/riak-{{VERSION}}-1.el7.centos.x86_64.rpm)
* [Fedora 19](https://packagecloud.io/basho/riak/packages/fedora/19/riak-{{VERSION}}-1.fc19.x86_64.rpm)

Our documentation also includes instructions regarding signing keys and
sources lists, which can be found in the [[Advanced rpm
Installation|Installing on RHEL and CentOS#Advanced-rpm-Installation]]
section immediately below.

## Advanced rpm Installation

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
PACKAGE_CLOUD_RIAK_DIR=https://packagecloud.io/install/repositories/basho/riak
curl "${PACKAGE_CLOUD_RIAK_DIR}/config_file.repo?os=${OS}&dist=${DIST}&name=${HOSTNAME}" > $FILENAME
```

The `name` that you submit to packagecloud can be anything you like. The
`HOSTNAME` used above was for example purposes. The resulting file
should contents like the following:

```
[basho_riak]
name=basho_riak
baseurl=https://packagecloud.io/basho/riak/el/5/$basesearch
repo_gpgcheck=1
gpgcheck=0
enabled=1
gpgkey=https://packagecloud.io/gpg.key
sslverify=1
sslcacert=/etc/pki/tls/certs/ca-bundle.crt
```

With your `basho.repo` file population, you can update your rpm sources
list.

## Installing From Package

If you wish to install the RHEL/CentOS packages by hand, follow these
instructions.

### For Centos 5 / RHEL 5

You can install CentOS 5/RHEL 5 using yum, which we recommend:

```bash
sudo yum install http://yum.basho.com/gpg/basho-release-5-1.noarch.rpm
sudo yum install riak
```

Or you can install the `.rpm` package manually:

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{VERSION}}/rhel/5/riak-{{VERSION}}-1.el5.x86_64.rpm
sudo rpm -Uvh riak-{{VERSION}}-1.el5.x86_64.rpm
```

### For Centos 6 / RHEL 6

You can install using yum, which we recommend:

```bash
sudo yum install http://yum.basho.com/gpg/basho-release-6-1.noarch.rpm
sudo yum install riak
```

Or you can install the `.rpm` package manually:

```bash
wget http://s3.amazonaws.com/downloads.basho.com/riak/{{V.V}}/{{VERSION}}/rhel/6/riak-{{VERSION}}-1.el6.x86_64.rpm
sudo rpm -Uvh riak-{{VERSION}}-1.el6.x86_64.rpm
```

## Next Steps

Now that Riak is installed, check out [[Verifying a Riak Installation]].
