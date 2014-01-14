---
title: Testing the Riak CS Installation
project: riakcs
version: 1.2.0+
document: tutorial
toc: true
index: true
audience: beginner
keywords: [tutorial, fast-track, installing]
prev: "[[Building a Virtual Testing Environment]]"
up:   "[[The Riak CS Fast Track]]"
---

## Installing & Configuring s3cmd

### Installation

The simplest way to test the installation is using the `s3cmd` script. We can install it on Ubuntu by typing:

``` bash
sudo apt-get -y install s3cmd
```

For our OS X users, either use the package manager of your preference or download the S3 cmd package at [[http://s3tools.org/download]].  You will need to extract the `.tar` file, change directories into the folder, and build the package. The process should look something like this:

``` bash
tar -xvf s3cmd-1.5.0-alpha1.tar.gz
cd s3cmd-1.5.0-alpha1
sudo python setup.py install
```

You will be prompted to enter your system password. Enter it and then wait for the installation to complete.

### Configuration

We need to configure `s3cmd` to use our Riak CS server rather than S3 as well as our user keys. To do that interactively, type the following:

``` bash
s3cmd -c ~/.s3cfgfasttrack --configure
```

If you are already using `s3cmd` on your local machine, the `-c` switch allows you to specify a `.s3cfg` file without overwriting anything you may have presently configured.

There are 4 default settings you should change:

* Access Key --- Use the Riak CS user access key you generated above.
* Secret Key --- Use the Riak CS user secret key you generated above.
* Proxy Server --- Use your Riak CS IP. If you followed the Virtual environment configuration, use `localhost`.
* Proxy Port --- The default Riak CS port is `8080`.

You should have copied your Access Key and Secret Key from the prior installation steps.

## Interacting with Riak CS via S3cmd

Once `s3cmd` is configured, we can use it to create a test bucket:

``` bash
s3cmd -c ~/.s3cfgfasttrack mb s3://test-bucket
```

We can see if it was created by typing:

``` bash
s3cmd -c ~/.s3cfgfasttrack ls
```

We can now upload a test file to that bucket:

``` bash
dd if=/dev/zero of=test_file bs=1m count=2 # Create a test file
s3cmd -c ~/.s3cfgfasttrack put test_file s3://test-bucket
```

We can see if it was properly uploaded by typing:

``` bash
s3cmd -c ~/.s3cfgfasttrack ls s3://test-bucket
```

We can now download the test file:

``` bash
# remove the local test file
rm test_file
# download from Riak CS
s3cmd -c ~/.s3cfgfasttrack get s3://test-bucket/test_file
# verify that the download was successful
ls -lah test_file
```

## What's Next
If you have made it this far, congratulations! You now have a working Riak CS test instance (either virtual or local). There is still a fair bit of learning to be done, so make sure and check out the Reference section (click "Reference" on the nav on the left side of this page). A few items that may be of particular interest:

* [[Details about API operations|RiakCS Storage API]]
* [[Information about the Ruby Fog client|Fog on Riak CS]]
* [[Release Notes]]
