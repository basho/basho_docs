---
title: Installing on AWS Marketplace
project: riak
version: 1.2.1+
document: tutorial
audience: beginner
keywords: [tutorial, installing, AWS, marketplace, amazon]
prev: "[[Installing on Windows Azure]]"
up:   "[[Installing and Upgrading]]"
next: "[[Installing Riak from Source]]"
---

## Launching Riak VMs via the AWS Marketplace

In order to launch a Riak virtual machine via the AWS Marketplace, you will first need to sign up for an [Amazon Web Services] (http://aws.amazon.com) account.

1. Navigate to [https://aws.amazon.com/marketplace/] (https://aws.amazon.com/marketplace/) and sign in with your Amazon Web Services account.

2. Locate Riak in the "Databases & Caching" category or search for Riak from any page.

3. Set your desired AWS region, EC2 instance type, firewall settings, and key pair

	![AWS Marketplace Instance Settings](/images/aws-marketplace-settings.png)

4. Click the "Accept Terms and Launch with 1-Click" button.

### Security Group Settings

Once the virtual machine is created you should verify your selected EC2 security group is configured properly for Riak.  

1. In the AWS EC2 Management Console, click "Security Groups", then click the name of the security group for your Riak VM.

2. Click on the "Inbound" tab in the lower pane.  Your security group should include the following open ports:
	- 22 (SSH)
	- 8087 (Riak Protocol Buffers Interface)
	- 8098 (Riak HTTP Interface)

3. You will need to add additional rules within this security group to allow your Riak instances to communicate.  For each port range below, create a new "Custom TCP rule" with the source set to the current security group ID (found on the "Details" tab).  
	- Port range: 4369
	- Port range: 6000-7999
	- Port range: 8099 

4. When complete, your security group should contain all of the rules listed below.  If you are missing any rules, add them in the lower panel and then click the "Apply Rule Changes" button. 

	![EC2 Security Group Settings](/images/aws-marketplace-security-group.png)

You can read more about Riak's [[Security and Firewalls]].

## Clustering Riak on AWS

You will need need to launch at least 3 instances to form a Riak cluster.  When the instances have been provisioned and the security group is configured you can connect to them using SSH or PuTTY as the ec2-user. 

 You can find more information on connecting to an instance on the official [Amazon EC2 instance guide] (http://docs.amazonwebservices.com/AWSEC2/latest/UserGuide/AccessingInstances.html).

1. On the first node obtain the internal IP address:

	```text
	curl http://169.254.169.254/latest/meta-data/local-ipv4 
	```

2. For all other nodes, use the internal IP address of the first node:

	```text
	sudo riak-admin cluster join riak@<ip.of.first.node>
	```

3. After all of the nodes are joined, execute the following:

	```text
	sudo riak-admin cluster plan
	```

	If this looks good:

	```text
	sudo riak-admin cluster commit
	```

	To check the status of clustering use:

	```text
	sudo riak-admin member_status
	```

You now have a Riak cluster on AWS.

Further Reading:

- [[Basic Riak API Operations]]
