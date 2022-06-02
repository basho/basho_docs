---
title: "Installing on AWS Marketplace"
description: "Installing on AWS Marketplace"
menu:
  riak_ts-1.3.0:
    name: "AWS Marketplace"
    identifier: "installing_on_aws"
    weight: 250
    parent: "installing"
project: "riak_ts"
project_version: "1.3.0"
toc: true
version_history:
  locations:
    - ["1.3.0-1.3.1", "installing/aws"]
    - ["1.4.0+",      "setup/installing/aws"]
aliases:
    - /riakts/1.3.0/installing/aws
    - /riak/ts/1.3.0/installing/aws-marketplace/
---


[AWS]: http://aws.amazon.com
[download]: ../../downloads/
[ec2 guide]: http://docs.amazonwebservices.com/AWSEC2/latest/UserGuide/AccessingInstances.html
[security basics]: {{<baseurl>}}riak/kv/2.1.4/using/security/basics


Riak TS can be installed on AWS virtual machines (VMs) using a binary
package available [here][download]. This page will walk you through the process of setting up your AWS environment and installing Riak TS on it.


## Launch Riak VMs via the AWS Marketplace

Get started by launching a Riak TS virtual machine via the AWS Marketplace. (You will need an [Amazon Web Services][AWS] account.)

1. Navigate to [https://aws.amazon.com/marketplace/](https://aws.amazon.com/marketplace/) and sign in with your Amazon Web Services account.

2. Locate Riak TS in the **Databases & Caching** category or search for Riak TS from any page.

3. Set your desired AWS region, EC2 instance type, firewall settings, and key pair.

    ![AWS Marketplace Instance Settings]({{<baseurl>}}images/aws-marketplace-settings.png)

4. Then click the **Accept Terms and Launch with 1-Click** button.


### Security Group Settings

Once the virtual machine is created, you should verify that your selected EC2 security group is properly configured for Riak TS.

1. In the AWS EC2 Management Console, click **Security Groups**, then click the name of the security group for your Riak TS VM.

2. Click on the **Inbound** tab in the lower pane. Your security group should include the following open ports:

  * 22 (SSH)
  * 8087 (Riak Protocol Buffers Interface)
  * 8098 (Riak HTTP Interface)

3. You will need to add additional rules within this security group to allow your Riak TS instances to communicate.  For each port range below, create a new **Custom TCP rule** with the source set to the current security group ID (found on the **Details** tab).

  * Port range: 4369
  * Port range: 6000-7999
  * Port range: 8099

4. When complete, your security group should contain all of the rules listed below. If you are missing any rules, add them in the lower panel and then click the **Apply Rule Changes** button.

    ![EC2 Security Group Settings]({{<baseurl>}}images/aws-marketplace-security-group.png)

We also recommend that you read more about [Security and Firewalls][security basics]. 


## Create a Riak Cluster on AWS

You will need need to launch at least 3 instances to form a Riak cluster.  When the instances have been provisioned and the security group is configured, you can connect to them using SSH or PuTTY as the ec2-user.

You can find more information on connecting to an instance on the official [Amazon EC2 instance guide][ec2 guide].

>Note: The following clustering setup will not be resilient to instance restarts unless deployed in Amazon VPC.


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


## Configure riak shell

In order to function, riak shell must know about each node in the cluster.

1. On each node, obtain the internal IP addresses:
    ```bash
    curl http://169.254.169.254/latest/meta-data/local-ipv4
    ```

2. On each node in the cluster, edit `/etc/riak/riak_shell.config`, adding the IPs of each node. You should end up with a config that is similar to:
   ```
    %%% -*- erlang -*-
    [
     {riak_shell, [
                  {logging, off},
                  {cookie, riak},
                  {show_connection_status, false},
                  {nodes, [
                       riak@10.13.37.221,
                       riak@10.22.19.97,
                       riak@10.2.54.31
                          ]}
                 ]}
    ].
    ```

3. Run `riak-shell` and ping the nodes to test the configuration:
    ```bash
    sudo riak-shell
    ```

    You should see a riak shell prompt:    
    ```bash
    [ec2-user ~]$ sudo riak-shell 
    Erlang R16B02_basho10 (erts-5.10.3) [source] [64-bit] [smp:2:2] [async-threads:10] [hipe] [kernel-poll:false] [frame-pointer]
    
    version "riak_shell 0.9/sql 1.3", use 'quit;' or 'q;' to exit or 'help;' for helpConnected...
    riak-shell(1)>
    ```

    Run a `ping` to test the configuration:
    ```bash
    riak-shell(1)>ping;
    'riak@10.13.37.221':  (connected)
    'riak@10.22.19.97':  (connected)
    'riak@10.2.54.31':  (connected)
    riak-shell(2)>
    ```

riak shell is now configured and ready for use.
