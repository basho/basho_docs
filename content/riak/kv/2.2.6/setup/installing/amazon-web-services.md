---
title_supertext: "Installing on"
title: "Amazon Web Services"
description: ""
project: "riak_kv"
project_version: "2.2.6"
menu:
  riak_kv-2.2.6:
    name: "Amazon Web Services"
    identifier: "installing_amazon_web_services"
    weight: 301
    parent: "installing"
toc: true
aliases:
  - /riak/2.2.6/ops/building/installing/Installing-on-AWS-Marketplace
  - /riak/kv/2.2.6/ops/building/installing/Installing-on-AWS-Marketplace
  - /riak/2.2.6/installing/amazon-web-services/
  - /riak/kv/2.2.6/installing/amazon-web-services/
---


## Launching Riak VMs via the AWS Marketplace

{{% note title="Note" %}}
The AWS Marketplace does not always have the most recent versions of Riak available. To use the latest version of Riak, please refer to the [Installing from Package](#installing-from-package) section underneath.
{{% /note %}}

In order to launch a Riak virtual machine via the AWS Marketplace, you will first need to sign up for an [Amazon Web Services](http://aws.amazon.com) account.

1. Navigate to [https://aws.amazon.com/marketplace/](https://aws.amazon.com/marketplace/) and sign in with your Amazon Web Services account.

2. Locate Riak in the **Databases & Caching** category or search for Riak from any page.

3. Set your desired AWS region, EC2 instance type, firewall settings, and key pair

    ![AWS Marketplace Instance Settings]({{<baseurl>}}images/aws-marketplace-settings.png)

4. Click the **Accept Terms and Launch with 1-Click** button.

### Security Group Settings

Once the virtual machine is created, you should verify that your selected EC2 security group is properly configured for Riak.

1. In the AWS EC2 Management Console, click **Security Groups**, then click the name of the security group for your Riak VM.

2. Click on the **Inbound** tab in the lower pane.  Your security group should include the following open ports:

  * 22 (SSH)
  * 8087 (Riak Protocol Buffers Interface)
  * 8098 (Riak HTTP Interface)

3. You will need to add additional rules within this security group to allow your Riak instances to communicate.  For each port range below, create a new **Custom TCP rule** with the source set to the current security group ID (found on the **Details** tab).

  * Port range: 4369
  * Port range: 6000-7999
  * Port range: 8099

4. When complete, your security group should contain all of the rules listed below. If you are missing any rules, add them in the lower panel and then click the **Apply Rule Changes** button.

    ![EC2 Security Group Settings]({{<baseurl>}}images/aws-marketplace-security-group.png)

We also recommend that you read more about Riak's [Security and Firewalls]({{<baseurl>}}riak/kv/2.2.6/using/security/).

## Clustering Riak on AWS

You will need need to launch at least 3 instances to form a Riak cluster.  When the instances have been provisioned and the security group is configured, you can connect to them using SSH or PuTTY as the ec2-user.

You can find more information on connecting to an instance on the official [Amazon EC2 instance guide](http://docs.amazonwebservices.com/AWSEC2/latest/UserGuide/AccessingInstances.html).

{{% note title="Note" %}}
The following clustering setup will _not_ be resilient to instance restarts
unless deployed in Amazon VPC.
{{% /note %}}

{{% note title="Note on Package Based Installation" %}}
 If installing to AWS by package, further configuration to _riak.conf_ to set the node name and listening IP addresses is necessary for the below steps to function.
{{% /note %}}

1. On the first node, obtain the internal IP address:

    ```bash
    curl http://169.254.169.254/latest/meta-data/local-ipv4
    ```

2. For all other nodes, use the internal IP address of the first node:

    ```bash
    sudo riak-admin cluster join riak@<ip.of.first.node>
    ```

3. After all of the nodes are joined, execute the following:

    ```bash
    sudo riak-admin cluster plan
    ```

    If this looks good:

    ```bash
    sudo riak-admin cluster commit
    ```

    To check the status of clustering use:

    ```bash
    sudo riak-admin member_status
    ```

You now have a Riak cluster running on AWS.


## Installing From Package

#### AWS (2)

You can install on AWS 2 using yum, which we recommend:

```bash
wget https://files.tiot.jp/riak/kv/2.2/2.2.6/amazon/2/riak-2.2.6-1.amzn2x86_64.rpm
sudo yum localinstall -y riak_2.2.6-1.amzn2x86_64.rpm
```

Or you can install the `.rpm` package manually:

```bash
wget https://files.tiot.jp/riak/kv/2.2/2.2.6/amazon/2/riak-2.2.6-1.amzn2x86_64.rpm
sudo rpm -i riak_2.2.6-1.amzn2x86_64.rpm
```


#### AWS (2016.09)

You can install on AWS 2 using yum, which we recommend:

```bash
wget https://files.tiot.jp/riak/kv/2.2/2.2.6/amazon/2016.09/riak-2.2.6-1.amzn1x86_64.rpm
sudo yum localinstall -y riak_2.2.6-1.amzn1x86_64.rpm
```

Or you can install the `.rpm` package manually:

```bash
wget https://files.tiot.jp/riak/kv/2.2/2.2.6/amazon/2016.09/riak-2.2.6-1.amzn1x86_64.rpm
sudo rpm -i riak_2.2.6-1.amzn1x86_64.rpm
```
## Next Steps

Now that Riak is installed and you have set the [Security Group Settings](#security-group-settings), check out [Verifying a Riak Installation][install verify].
