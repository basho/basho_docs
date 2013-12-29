---
title: Configuring an S3 Client
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, configuration]
---

<div class="note">
<div class="title">Configuration Update</div>
A previous version of the following sample `.s3cfg` files omitted the following line:

<tt>enable_multipart = False</tt>

Without this setting, <tt>s3cmd</tt> will fail to upload files greater than approximately 10MB in size.
</div>

If you use `s3cmd` as your S3 client, you must configure the application to interact with your Riak CS system. The `s3cmd` configuration file is named `.s3cfg` and resides in the home directory of the user. The settings to change depend on your Riak CS system.

`s3cmd` uses a configuration file `.s3cfg` which should be located in the user's home directory. Running `s3cmd --configure` launches an interactive tool to generate a configuration. A configuration file may also be specified using the` -c` option. Here are a couple of sample `.s3cfg` files that can be used to configure `s3cmd` to interact with Riak CS.

## Sample `.s3cfg` File for Local Use

Use this `.s3cfg` configuration file example to interact with Riak CS locally via port `8080` with `s3cmd`:

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
```

### Sample `.s3cfg` File for Production Use

Use this `.s3cfg` configuration file example to interact with Riak CS running in a production configuration with `s3cmd`:

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
```

### Specifying User Credentials

To configure the `s3cmd` client for the user, you must change the `access_key` and `secret_key` settings.

### Specifying Storage Location

By default, the `.s3cfg` file uses the Amazon S3 service as the storage backend. For a Riak CS system, change the following settings to point to your storage system:

* `host_base` &mdash; Specify the domain name or the path to your data storage, such as data.example.com.
* `host_bucket` &mdash; Specify the bucket location, such as `%(bucket)s.data.example.com`.

### Enabling SSL in the Client

If you are using SSL, set `use_https` equal to `True`.
