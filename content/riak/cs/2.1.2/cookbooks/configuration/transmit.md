---
title: "Configuring Transmit"
description: ""
menu:
  riak_cs-2.1.2:
    name: "Configuring Transmit"
    identifier: "config_transmit"
    weight: 104
    parent: "api_s3"
project: "riak_cs"
project_version: "2.1.2"
aliases:
  - /riakcs/2.1.2/cookbooks/configuration/Configuring-Transmit/
  - /riak/cs/2.1.2/cookbooks/configuration/Configuring-Transmit/
  - /riak/cs/latest/cookbooks/configuration/transmit/
---

[Transmit](https://www.panic.com/transmit/) is an S3-compatible client with a
graphical user interface for Mac OS X. The following guide describes configuration of Transmit for use with Riak CS.

{{% note title="Note" %}}
S3 support was added in Transmit version 4.4, so ensure that you're following
along with a version that supports S3 before continuing.
{{% /note %}}

## Define a Connection

When Transmit is started, a new connection window appears. Ensure that you've
selected the **S3** tab, then complete the details in the **Connect to S3**
dialog as follows:

* **Server** --- Enter the fully qualified domain name of the Riak CS server here. Be sure that this matches the value specified for `cs_root_host` in the Riak CS `app.config`.

* **Access Key ID** --- Enter the Access Key ID (`key_id`) for the user account you will use to connect to Riak CS.

* **Secret** --- Enter the Access Key Secret (`key_secret`) matching the user account you entered for the Access Key ID above.

* **Initial Path** --- If you're connecting to a Riak CS instance with existing buckets to which the user account has access, you can optionally enter a specific bucket name to use for this connection here.

Defining a connection looks like this:

![Trasmit screenshot]({{<baseurl>}}images/riak_cs_transmit0.jpg)

> **Note**
>
> Transmit expects a secure connection, so ensure that your Riak CS proxy server is configured with SSL support. For information on configuring a software solution like HAProxy with SSL for use with Riak CS, see [Load Balancing and Proxy Configuration]({{<baseurl>}}riak/cs/2.1.2/cookbooks/configuration/load-balancing-proxy).

Finally, test the connection to Riak CS by clicking **Connect**.

## Create a Bucket

After successfully connecting to Riak CS, verify that you can create a bucket.

1. From the **File** menu, select **New Bucket...**
2. In the bucket creation dialog, enter the name of the new bucket
3. Click **Create**

The new bucket creation dialog looks like this:

![Trasmit screenshot]({{<baseurl>}}images/riak_cs_transmit1.jpg)

The newly created bucket is listed in the right hand pane of the Transmit interface:

![Trasmit screenshot]({{<baseurl>}}images/riak_cs_transmit2.jpg)

## Copy Files

Now that you've created a bucket, you can perform a basic file copy test.

Double-click the bucket icon in the right hand pane of the Transmit interface
to access the bucket.

Drag and drop one or more files to the right hand pane to initiate
copying of the files to the bucket.

After copying, the files will appear in the bucket:

![Trasmit screenshot]({{<baseurl>}}images/riak_cs_transmit3.jpg)

You have now successfully configured a Transmit connection to Riak CS and
verified basic file copying capabilities.
