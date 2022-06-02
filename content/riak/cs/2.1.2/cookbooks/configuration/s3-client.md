---
title: "Configuring an S3 Client"
description: ""
menu:
  riak_cs-2.1.2:
    name: "Configuring an S3 Client"
    identifier: "config_s3_client"
    weight: 101
    parent: "api_s3"
project: "riak_cs"
project_version: "2.1.2"
aliases:
  - /riakcs/2.1.2/cookbooks/configuration/Configuring-an-S3-Client/
  - /riak/cs/2.1.2/cookbooks/configuration/Configuring-an-S3-Client/
  - /riak/cs/latest/cookbooks/configuration/s3-client/
---

This tutorial will show you how to use [s3cmd](http://s3tools.org/s3cmd)
as an S3 client. While it won't cover all of the client's features, it
will show you how to create a configuration and run some basic commands.

>**Warning: s3cmd Signature Version**
>
> If you are using s3cmd version 1.5.0 or greater you will need to append the
> `--signature-v2` flag to every command that targets a Riak CS cluster to have
> s3cmd use the AWS Signature version 2 rather than the default AWS Signature
> version 3

## Initial Setup

To use s3cmd in conjunction with Riak CS, you must configure it to
interact with your Riak CS system. One way to do so is to create a
`.s3cfg` file and store it in your home directory. When you run any
s3cmd-related command, the contents of that file will be read by
default.  Alternatively, you can specify a non-default configuration
file location using the `-c` flag.  Here's an example:

```bash
s3cmd -c /PATH/TO/CONFIG/FILE <command>
```

Another way to configure s3cmd is to run `s3cmd --configure`, which
launches an interactive tool that will assemble a configuration file for
you on the basis of what you enter.

In the next section you'll find a few sample `.s3cfg` files that can be
used to configure s3cmd to interact with Riak CS.

## Sample s3cmd Configuration File for Local Use

Use this `.s3cfg` configuration file example to interact with Riak CS
locally via port `8080` with s3cmd (remember to use information specific
to your Riak CS installation where necessary):

```config
[default]
access_key = 8QON4KC7BMAYYBCEX5J+
bucket_location = US
cloudfront_host = cloudfront.amazonaws.com
cloudfront_resource = /2010-07-15/distribution
default_mime_type = binary/octet-stream
delete_removed = False
dry_run = False
enable_multipart = False
encoding = UTF-8
encrypt = False
follow_symlinks = False
force = False
get_continue = False
gpg_command = /usr/local/bin/gpg
gpg_decrypt = %(gpg_command)s -d --verbose --no-use-agent --batch --yes --passphrase-fd %(passphrase_fd)s -o %(output_file)s %(input_file)s
gpg_encrypt = %(gpg_command)s -c --verbose --no-use-agent --batch --yes --passphrase-fd %(passphrase_fd)s -o %(output_file)s %(input_file)s
gpg_passphrase = password
guess_mime_type = True
host_base = s3.amazonaws.com
host_bucket = %(bucket)s.s3.amazonaws.com
human_readable_sizes = False
list_md5 = False
log_target_prefix =
preserve_attrs = True
progress_meter = True
proxy_host = localhost
proxy_port = 8080
recursive = False
recv_chunk = 4096
reduced_redundancy = False
secret_key = rGyDLBi7clBuvrdrkFA6mAJkwJ3ApUVr4Pr9Aw==
send_chunk = 4096
simpledb_host = sdb.amazonaws.com
skip_existing = False
socket_timeout = 300
urlencoding_mode = normal
use_https = False
verbosity = WARNING
signature_v2 = True
```

## Sample s3cmd Configuration File for Production Use

Use this `.s3cfg` configuration file example to interact with Riak CS
using s3cmd in a production system:

```config
[default]
access_key = EJ8IUJX9X0F2P9HAMIB0
bucket_location = US
cloudfront_host = cloudfront.amazonaws.com
cloudfront_resource = /2010-07-15/distribution
default_mime_type = binary/octet-stream
delete_removed = False
dry_run = False
enable_multipart = False
encoding = UTF-8
encrypt = False
follow_symlinks = False
force = False
get_continue = False
gpg_command = /usr/local/bin/gpg
gpg_decrypt = %(gpg_command)s -d --verbose --no-use-agent --batch --yes --passphrase-fd %(passphrase_fd)s -o %(output_file)s %(input_file)s
gpg_encrypt = %(gpg_command)s -c --verbose --no-use-agent --batch --yes --passphrase-fd %(passphrase_fd)s -o %(output_file)s %(input_file)s
gpg_passphrase = password
guess_mime_type = True
host_base = <YOUR DOMAIN HERE>
host_bucket = %(bucket)s.<YOUR DOMAIN HERE>
human_readable_sizes = False
list_md5 = False
log_target_prefix =
preserve_attrs = True
progress_meter = True
proxy_host =
proxy_port = 0
recursive = False
recv_chunk = 4096
reduced_redundancy = False
secret_key = XOY/9IFKVEDUl6Allrkj7oyH9XW+CANnFLEVuw==
send_chunk = 4096
simpledb_host = sdb.amazonaws.com
skip_existing = False
socket_timeout = 300
urlencoding_mode = normal
use_https = True
verbosity = WARNING
signature_v2 = True
```

To configure the s3cmd client for the user, you must change the
`access_key` and `secret_key` settings.

## Specifying Storage Location

By default, the `.s3cfg` file uses the Amazon S3 service as the storage
backend. For a Riak CS system, change the following settings to point to
your storage system:

* `host_base` --- Specify the domain name or the path to your data
  storage, such as `data.example.com`
* `host_bucket` --- Specify the bucket location, such as
  `my_cs_bucket.data.example.com`.

## Enabling SSL in the Client

If you are using SSL, set `use_https` equal to `True`.
