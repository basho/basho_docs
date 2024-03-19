---
title: "Keystone Configuration Sample"
description: ""
project: "riak_cs"
project_version: "2.1.2"
lastmod: 2019-04-09T00:00:00-00:00
sitemap:
  priority: 0.3
aliases:
  - /riakcs/2.1.2/cookbooks/Keystone-Conf-Sample/

---

The following displays the contents of a sample `keystone.conf` file
that can be used to test Riak CS with the Keystone authentication
service on a `localhost` setup.

```config
[DEFAULT]
# A "shared secret" between keystone and other openstack services
admin_token = ADMIN

# The IP address of the network interface to listen on
bind_host = 127.0.0.1

# The port number which the public service listens on
public_port = 5000

# The port number which the public admin listens on
admin_port = 35357

# The port number which the OpenStack Compute service listens on
# compute_port = 8774

# Path to your policy definition containing identity actions
# TODO(dolph): This config method will probably be deprecated during grizzly
# policy_file = policy.json

# Rule to check if no matching policy definition is found
# FIXME(dolph): This should really be defined as [policy] default_rule
# policy_default_rule = admin_required

# === Logging Options ===
# Print debugging output
verbose = True

# Print more verbose output
# (includes plaintext request logging, potentially including passwords)
debug = True

# Name of log file to output to. If not set, logging will go to stdout.
log_file = keystone.log

# The directory to keep log files in (will be prepended to --logfile)
log_dir = log/keystone

# Use syslog for logging.
use_syslog = False

# syslog facility to receive log lines
# syslog_log_facility = LOG_USER

# If this option is specified, the logging configuration file specified is
# used and overrides any other logging options specified. Please see the
# Python logging module documentation for details on logging configuration
# files.
#log_config = logging.conf

# A logging.Formatter log message format string which may use any of the
# available logging.LogRecord attributes.
# log_format = %(asctime)s %(levelname)8s [%(name)s] %(message)s

# Format string for %(asctime)s in log records.
# log_date_format = %Y-%m-%d %H:%M:%S

# onready allows you to send a notification when the process is ready to serve
# For example, to have it notify using systemd, one could set shell command:
# onready = systemd-notify --ready
# or a module with notify() method:
# onready = keystone.common.syst/

[sql]
# The SQLAlchemy connection string used to connect to the database
connection = sqlite:///keystone.db

# the timeout before idle sql connections are reaped
idle_timeout = 200

[identity]
driver = keystone.identity.backends.sql.Identity

[catalog]
# dynamic, sql-based backend (supports API/CLI-based management commands)
#driver = keystone.catalog.backends.sql.Catalog

# static, file-based backend (does *NOT* support any management commands)
driver = keystone.catalog.backends.templated.TemplatedCatalog

template_file = ./etc/default_catalog.templates

[token]
# driver = keystone.token.backends.kvs.Token

# Amount of time a token should remain valid (in seconds)
# expiration = 86400

[policy]
driver = keystone.policy.backends.sql.Policy

[ec2]
# driver = keystone.contrib.ec2.backends.kvs.Ec2

[ssl]
#enable = True
#certfile = /etc/keystone/ssl/certs/keystone.pem
#keyfile = /etc/keystone/ssl/private/keystonekey.pem
#ca_certs = /etc/keystone/ssl/certs/ca.pem
#cert_required = True

[signing]
token_format = UUID
#certfile = /etc/keystone/ssl/certs/signing_cert.pem
#keyfile = /etc/keystone/ssl/private/signing_key.pem
#ca_certs = /etc/keystone/ssl/certs/ca.pem
#key_size = 1024
#valid_days = 3650
#ca_password = None

[ldap]
# url = ldap://localhost
# user = dc=Manager,dc=example,dc=com
# password = None
# suffix = cn=example,cn=com
# use_dumb_member = False
# allow_subtree_delete = False
# dumb_member = cn=dumb,dc=example,dc=com

# user_tree_dn = ou=Users,dc=example,dc=com
# user_filter =
# user_objectclass = inetOrgPerson
# user_id_attribute = cn
# user_name_attribute = sn
# user_mail_attribute = email
# user_pass_attribute = userPassword
# user_enabled_attribute = enabled
# user_enabled_mask = 0
# user_enabled_default = True
# user_attribute_ignore = tenant_id,tenants
# user_allow_create = True
# user_allow_update = True
# user_allow_delete = True

# tenant_tree_dn = ou=Groups,dc=example,dc=com
# tenant_filter =
# tenant_objectclass = groupOfNames
# tenant_id_attribute = cn
# tenant_member_attribute = member
# tenant_name_attribute = ou
# tenant_desc_attribute = desc
# tenant_enabled_attribute = enabled
# tenant_attribute_ignore =
# tenant_allow_create = True
# tenant_allow_update = True
# tenant_allow_delete = True

# role_tree_dn = ou=Roles,dc=example,dc=com
# role_filter =
# role_objectclass = organizationalRole
# role_id_attribute = cn
# role_name_attribute = ou
# role_member_attribute = roleOccupant
# role_attribute_ignore =
# role_allow_create = True
# role_allow_update = True
# role_allow_delete = True

[filter:debug]
paste.filter_factory = keystone.common.wsgi:Debug.factory

[filter:token_auth]
paste.filter_factory = keystone.middleware:TokenAuthMiddleware.factory

[filter:admin_token_auth]
paste.filter_factory = keystone.middleware:AdminTokenAuthMiddleware.factory

[filter:xml_body]
paste.filter_factory = keystone.middleware:XmlBodyMiddleware.factory

[filter:json_body]
paste.filter_factory = keystone.middleware:JsonBodyMiddleware.factory

[filter:user_crud_extension]
paste.filter_factory = keystone.contrib.user_crud:CrudExtension.factory

[filter:crud_extension]
paste.filter_factory = keystone.contrib.admin_crud:CrudExtension.factory

[filter:ec2_extension]
paste.filter_factory = keystone.contrib.ec2:Ec2Extension.factory

[filter:s3_extension]
paste.filter_factory = keystone.contrib.s3:S3Extension.factory

[filter:url_normalize]
paste.filter_factory = keystone.middleware:NormalizingFilter.factory

[filter:stats_monitoring]
paste.filter_factory = keystone.contrib.stats:StatsMiddleware.factory

[filter:stats_reporting]
paste.filter_factory = keystone.contrib.stats:StatsExtension.factory

[app:public_service]
paste.app_factory = keystone.service:public_app_factory

[app:service_v3]
paste.app_factory = keystone.service:v3_app_factory

[app:admin_service]
paste.app_factory = keystone.service:admin_app_factory

[pipeline:public_api]
pipeline = stats_monitoring url_normalize token_auth admin_token_auth xml_body json_body debug ec2_extension user_crud_extension public_service

[pipeline:admin_api]
pipeline = stats_monitoring url_normalize token_auth admin_token_auth xml_body json_body debug stats_reporting ec2_extension s3_extension crud_extension admin_service

[pipeline:api_v3]
pipeline = stats_monitoring url_normalize token_auth admin_token_auth xml_body json_body debug stats_reporting ec2_extension s3_extension service_v3

[app:public_version_service]
paste.app_factory = keystone.service:public_version_app_factory

[app:admin_version_service]
paste.app_factory = keystone.service:admin_version_app_factory

[pipeline:public_version_api]
pipeline = stats_monitoring url_normalize xml_body public_version_service

[pipeline:admin_version_api]
pipeline = stats_monitoring url_normalize xml_body admin_version_service

[composite:main]
use = egg:Paste#urlmap
/v2.0 = public_api
/v3 = api_v3
/ = public_version_api

[composite:admin]
use = egg:Paste#urlmap
/v2.0 = admin_api
/v3 = api_v3
/ = admin_version_api
```
