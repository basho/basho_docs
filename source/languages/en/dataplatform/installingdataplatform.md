---
title: Installing Basho Data Platform
project: dataplatform
version: 1.0.0+
document: tutorial
toc: false
index: true
audience: beginner
keywords: [data platform, install]
body_id: dataplatform-installing
simple: true
versions: true
---

[bdp aws]: 
[bdp compatibility]: http://docs.basho.com/dataplatform/latest/#supported-operating-systems
[bdp configure]: LINK
[bdp download]: http://docs.basho.com/dataplatform/latest/dataplatform-downloads/

#Installing Basho Data Platform
Basho Data Platform (BDP) enables you to extend Riak with Spark and Redis. This page will guide you through the process of installing BDP on most supported operating systems. If you're installing BDP on AWS, look [here][bdp aws]. 

>Note:
>BDP is supported on a limited number of platforms. See the list of supported OSes [here][bdp compatibility].

##Prerequisites
You need to have root or sudo access on the nodes you will be installing BDP on.

##Installing

1. First, [change the open-files limit.](#increase-the-openfiles-limit).
2. If you plan to use Spark, then [install Java 8](#java-8).
3. Finally, [install the BDP package](#install-bdp).

###Increase The Open-Files Limit

Before you can install BDP, both the total open-files limit and the per-user open-files limit must be high enough to allow BDP to function.

On most Linux distributions, the total limit for open files is controlled by `sysctl`.

```shell
sudo sysctl fs.file-max fs.file-max=65536
sudo sysctl -p
```

To change the per-user file limit, you need to edit /etc/security/limits.conf.

On CentOS systems, set a proper limit for the user you’re usually logging in with to do any kind of work on the machine, including managing Riak. On CentOS, `sudo` properly inherits the values from the executing user.

On Ubuntu systems, the following settings are recommended:


```
»USERNAME« hard nofile 65536
»USERNAME« soft nofile 65536
root hard nofile 65536
root soft nofile 65536
```

>Note:
>
>You may need to log out of your shell and then log back in for these changes to take effect.

### Java 8

If you are using or plan to use Spark, you must install Java 8 on **each node that will run Spark services** (unless you are using Spark with Python). Installing Java works a little differently depending on whether you are running Ubuntu or CentOS.

If you are on Ubuntu, run the following to install Java 8:

```Ubuntu
$ 
sudo apt-get install software-properties-common
sudo apt-get install python-software-properties
$ sudo add-apt-repository ppa:webupd8team/java
$ sudo apt-get update
$ sudo apt-get install -y oracle-java8-installer
```

If you are on CentOS, run the following to install Java 8:

```CentOS
$ cd /opt
$ sudo wget --no-cookies --no-check-certificate --header "Cookie: gpw_e24=http%3A%2F%2Fwww.oracle.com%2F; oraclelicense=accept-securebackup-cookie" "http://download.oracle.com/otn-pub/java/jdk/8u45-b14/jdk-8u45-linux-x64.rpm"
```

Regardless of your OS, once you have installed Java 8 you need to add a specific `JAVA_HOME` line to your /etc/environment. You can add the correct line by running: `grep JAVA_HOME /etc/environment >/dev/null 2>&1 || test $? -ne 0 && sudo bash -c "echo JAVA_HOME=$(dirname $(dirname $(readlink -f $(which javac)))) >>/etc/environment"`
export JAVA_HOME.

### Install BDP

Now that you've increased your [open-files limit](#increase-the-openfiles-limit) and [installed Java 8](#java-8) where necessary, you're ready to install the BDP package.

BDP packages for all supported operating systems are available for download on the [Download Basho Data Platform page][bdp download]. 

Choose the installation instructions below that match your OS: [Ubuntu](#ubuntu) or [CentOS](#centos).

####Ubuntu

1. Download the package from the [downloads][bdp download] page.
2. Unpack the package using `sudo dpkg -i`.

####CentOS

1. Download the package from the [downloads][bdp download] page.
2. Unpack the package using `sudo yum` and `sudo rpm`.

##Next Steps

Congratulations! You've successfully installed Basho Data Platform. Now, [set up a data platform cluster][bdp configure].
