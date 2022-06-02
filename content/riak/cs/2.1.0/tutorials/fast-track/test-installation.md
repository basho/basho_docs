---
title: "Testing the Riak CS Installation"
description: ""
menu:
  riak_cs-2.1.0:
    name: "Testing the Installation"
    identifier: "fast_track_test_install"
    weight: 103
    parent: "fast_track"
project: "riak_cs"
project_version: "2.1.0"
aliases:
  - /riakcs/2.1.0/cookbooks/tutorials/fast-track/Testing-the-Installation/
  - /riak/cs/2.1.0/cookbooks/tutorials/fast-track/Testing-the-Installation/
---

## Installing & Configuring s3cmd

### Installation

The simplest way to test the installation is using the `s3cmd` script.
We can install it on Ubuntu by typing:

``` bash
sudo apt-get -y install s3cmd
```

>**s3cmd versions**
>
> The above command will install s3cmd version 1.1.0~beta3, which is the latest
> version available in Debian's package reposiory. More current insatll
> instructions suggest using Python's [pip](https://pypi.python.org/pypi/pip)
> module to install s3cmd.
> If this method is used, s3cmd will use AWS Signature version 3 which is
> incompatible with RIak CS. As of s3cmd version 1.5.0, there is a
> `--signature-v2` flag that can be used to have s3cmd use the older format, and
> correctly interact with Riak CS.

For OS X users, either use the package manager of your preference or [download the S3 cmd package](http://sourceforge.net/projects/s3tools/files/s3cmd/).
You will need need to extract the `.tar` file, change directories into the
folder, and build the package. The process should look something like this:

``` bash
tar -xvzf s3cmd-1.6.1.tar.gz
cd s3cmd-1.6.1
sudo python setup.py install
```

You will be prompted to enter your system password. Enter it and then
wait for the installation to complete.

### Configuration

We need to configure `s3cmd` to use our Riak CS server rather than S3 as
well as our user keys. To do that interactively, type the following:

``` bash
s3cmd -c ~/.s3cfgfasttrack --configure
```

If you are already using `s3cmd` on your local machine, the `-c` switch
allows you to specify a `.s3cfg` file without overwriting anything you
may have presently configured.

There are 4 default settings you should change:

* Access Key --- Use the Riak CS user access key you generated above.
* Secret Key --- Use the Riak CS user secret key you generated above.
* Proxy Server --- Use your Riak CS IP. If you followed the virtual
  environment configuration, use `localhost`.
* Proxy Port --- The default Riak CS port is `8080`.

You should have copied your Access Key and Secret Key from the prior
installation steps.

## Interacting with Riak CS via S3cmd

>**Warning: s3cmd Signature Version**
>
> If you are using s3cmd version 1.5.0 or greater you will need to append the
> `--signature-v2` flag to every command that targets a Riak CS cluster to have
> s3cmd use the AWS Signature version 2 rather than the default AWS Signature
> version 3

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
dd if=/dev/zero of=test_file bs=1M count=2 # Create a test file
s3cmd -c ~/.s3cfgfasttrack put test_file s3://test-bucket
```

We can see if it was properly uploaded by typing:

``` bash
s3cmd -c ~/.s3cfgfasttrack ls s3://test-bucket
```

We can now download the test file. First, let's remove the file we
generated previously:

``` bash
rm test_file
```

Now, we can download the `test_file`stored in Riak CS:

```bash
s3cmd -c ~/.s3cfgfasttrack get s3://test-bucket/test_file
```

We should immediately see output like this:

```
s3://test-bucket/test_file -> ./test_file  [1 of 1]
 2097152 of 2097152   100% in    0s    59.63 MB/s  done
```

To verify that the file has been downloaded into the current directory:

```bash
ls -lah test_file
```

## What's Next

If you have made it this far, congratulations! You now have a working
Riak CS test instance (either virtual or local). There is still a fair
bit of learning to be done, so make sure and check out the Reference
section (click "Reference" on the nav on the left side of this page). A
few items that may be of particular interest:

* [Details about API operations]({{<baseurl>}}riak/cs/2.1.0/references/apis/storage)
* [Information about the Ruby Fog client]({{<baseurl>}}riak/cs/2.1.0/cookbooks/fog)
* [Release Notes]({{<baseurl>}}riak/cs/2.1.0/cookbooks/release-notes)
