---
title: Testing the Riak CS Installation
project: riakcs
version: 1.2.0+
document: tutorial
toc: true
index: true
audience: beginner
keywords: [tutorial, fast-track, installing]
prev: "[[Building a Virtual Test Environment]]"
up:   "[[The Riak CS Fast Track]]"
---

## Installing & Configuring s3cmd

### Installation
The simplest way to test the installation is using the `s3cmd` script. We can install it on Ubuntu by typing:

    sudo apt-get -y install s3cmd

For our OS X users, either use the package manager of your preference, or download the S3 cmd package at [[http://s3tools.org/download]].  You will need to extra the tar, change directories into the folder, and build the package.  The process should look something like:

``` bash
$ tar -xvf s3cmd-1.5.0-alpha1.tar.gz
$ cd s3cmd-1.5.0-alpha1
$ sudo python setup.py install
```
You will be prompted to enter your system password.  Do so, and wait for the installation to complete.

### Configuration

We need to configure `s3cmd` to use our Riak CS server rather than S3 as well as our user keys. To do that interactively, type:

    s3cmd -c ~/.s3cfgfasttrack --configure

If you are alread using s3cmd on your local machine the `-c` switch allows you to specify a .s3cfg file without overwriting anything you may have presently configured

There are 4 default settings you should change:

* Access Key - use the Riak CS user access key you generated above.
* Secret Key - use the Riak CS user secret key you generated above.
* Proxy Server - use your Riak CS IP. If you followed the Virtual environment configuration, use `localhost`
* Proxy Port - the default Riak CS port is 8080

You should have copied your Access Key and Secret Key from the prior installation steps.

## Interacting with Riak CS via S3cmd

Once `s3cmd` is configured, we can use it to create a test bucket:

    s3cmd -c ~/.s3cfgfasttrack mb s3://test-bucket

We can see if it was created by typing:

    s3cmd -c ~/.s3cfgfasttrack ls

We can now upload a test file to that bucket:

    dd if=/dev/zero of=test_file bs=1m count=2 # Create a test file
    s3cmd -c ~/.s3cfgfasttrack put test_file s3://test-bucket

We can see if it was uploaded by typing:

    s3cmd -c ~/.s3cfgfasttrack ls s3://test-bucket

We can now download the test file:

    rm test_file # remove the local test file
    s3cmd -c ~/.s3cfgfasttrack get s3://test-bucket/test_file


