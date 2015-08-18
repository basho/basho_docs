---
title: Installing Basho Data Platform on AWS
project: dataplatform
version: 1.0.0+
document: guide
audience: beginner
---

[bdp compatibility]: http://docs.basho.com/dataplatform/latest/#supported-operating-systems
[bdp configure aws]: LINK
[bdp download]: http://docs.basho.com/dataplatform/latest/dataplatform-downloads/
[bdp install]: LINK


Basho Data Platform (BDP) enables you to extend Riak with Spark and Redis. This page will guide you through the process of installing BDP on [Amazon Web Services (AWS)](https://aws.amazon.com/). If you were looking for the general install page, go [here][bdp install].

<div class="note">
AWS security profile must allow incoming and outgoing traffic from ip/ports used by Riak, Spark, and BDP.  A [list of default ports](LINK) is at the end of this document. 
</div>

##Prerequisites
You need to have root or sudo access on the nodes you will be installing BDP on.

##Installing

1. First, [change the open-files limit.](#Increase-the-open-files-limit).
2. If you plan to use Spark, then [install Java 8](#Java-8).
3. Finally, [install the BDP package](#install-bdp).

###Increase The Open-Files Limit

During normal operation Riak can consume a large number of open-file handles. You can increase the total limit for open files by running the following:

```
if [[ $(sysctl fs.file-max |grep 65536) == "" ]]; then
  sudo sysctl fs.file-max=65536
  sudo sysctl -p

  sudo bash -c "cat <<EOF_LIMITS >> /etc/security/limits.conf
*                soft    nofile          65536
*                hard    nofile          65536
EOF_LIMITS
"
fi
```

### Java 8

To install Java run the following `apt-get` commands:

```
sudo apt-get install python-software-properties
sudo add-apt-repository ppa:webupd8team/java
sudo apt-get update
sudo echo -e oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | sudo debconf-set-selections
sudo apt-get install -y oracle-java8-installer
```

###Setting Environment Variables

Next configure your environment by running:

```
JAVA_HOME=$(dirname $(dirname $(readlink -f $(which javac))))
if [[ "$JAVA_HOME" == "" ]]; then
echo "failed to install jdk 8"
exit 1
fi
grep JAVA_HOME /etc/environment >/dev/null 2>&1 || test $? -ne 0 && sudo bash -c "echo JAVA_HOME=$JAVA_HOME >>/etc/environment"
```

### Install BDP

Now that you've increased your [open-files limit](#Increase-the-open-files-limit) and [installed Java 8](#Java-8) where necessary, you're ready to install the BDP package.

BDP packages for all supported operating systems are available for download on the [Download Basho Data Platform page][bdp download]. 

1. Download the package from the [downloads][bdp download] page.
2. Unpack the package using `sudo dpkg -i`.

##Next Steps

Congratulations! You've successfully installed Basho Data Platform on AWS. Now, [set up a data platform cluster on AWS][bdp configure aws].
