---
title: Load Balancing and Proxy Configuration
project: riakcs
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [operator, configuration]
---

The recommended best practice mode of production Riak CS operation suggests
placing Riak CS behind a load balancing or proxy solution, either hardware or
software based. You should not expose Riak CS directly to your public facing
network interfaces.

Riak CS users have reported success in using Riak CS with a variety of load
balancing and proxy solutions. Common solutions include proprietary hardware
based load balancers, cloud based load balancing options, such as Amazon's
Elastic Load Balancer, and open source software based projects like HAProxy
and Nginx.

This guide briefly explores the commonly used open source software based
solutions HAProxy and Nginx, and provides some configuration and operational
tips gathered from community users and operations oriented engineers at Basho.

While it is by no means an exhaustive overview of the topic, this guide should
provide a starting point for implementing an HAProxy or Nginx based solution.

## HAProxy

[HAProxy](http://haproxy.1wt.eu/) is a fast and reliable open source solution
for load balancing and proxying of HTTP and TCP based application traffic.

Users have reported success in using HAProxy in combination with Riak CS in a
number of configurations and scenarios. Much of the information and example
configuration for this section is drawn from experiences of users in the
Riak CS community in addition to suggestions from Basho engineering.

### Example Configuration

The following is an example starting point configuration for HAProxy to act
as a load balancer to a Riak CS installation.

<div class="info">The operating system's open files limits need to be
  greater than 256000 for the example configuration that follows. Consult
  the [[Open Files Limit|Open-Files-Limit]] documentation for details on
  configuring the value for different operating systems.</div>
  
```
global
    log 127.0.0.1     local0
    log 127.0.0.1     local1 notice
    maxconn           256000
    spread-checks     5
    daemon

defaults
    log               global
    option            dontlognull
    option            redispatch
    option            allbackups
    no option         httpclose
    retries           3
    maxconn           256000
    timeout connect   5000 
    timeout client    5000 
    timeout server    5000
 
frontend riak_cs
    bind              10.0.24.100:8080
    # Example bind for SSL termination
    # bind            10.0.24.100:8443 ssl crt /opt/local/haproxy/etc/data.pem
    mode              http
    option            httplog
    capture           request header Host len 64
    acl good_ips      src -f /opt/local/haproxy/etc/gip.lst
    block if          !good_ips
    use_backend       riak_cs_backend if good_ips

backend riak_cs_backend
    mode              http
    balance           roundrobin
    # Ping Riak CS to determine health
    option            httpchk GET /riak-cs/ping 
    timeout connect 60s
    timeout http-request 60s
    server riak1 r1s01.example.com:8081 weight 1 maxconn 1024 check
    server riak2 r1s02.example.com:8081 weight 1 maxconn 1024 check backup
    server riak3 r1s03.example.com:8081 weight 1 maxconn 1024 check backup
    server riak4 r1s04.example.com:8081 weight 1 maxconn 1024 check backup
    server riak5 r1s05.example.com:8081 weight 1 maxconn 1024 check backup
```

Note that the above example is considered a starting point and is a work in progress. You should carefully examine the configuration and change it
according to your specific environment.

Some specific configuration details from the example worth noting are the
commented option for SSL termination. As of version 1.5 HAProxy supports
SSL directly. Provided that your HAProxy instance was build with OepnSSL
support, you can enable it by uncommenting the example line, and modifying it
to suit your environment. More information is available in the
[HAProxy documentation](http://cbonte.github.io/haproxy-dconv/configuration-1.5.html#5-ssl).

Also note the option for checking Riak CS health via the `/riak-cs/ping`
endpoint. This option is essential for checking each Riak CS node as part of
the round robin load balancing method.

## Nginx

Some users have reported success in using the [Nginx](http://nginx.org/) HTTP
server to proxy requests for Riak CS. An example that provides access to
Riak CS is provided here for reference.

### Example Configuration

The following is an example starting point configuration for Nginx to act as a front end proxy to Riak CS.

```
upstream riak_cs_host {
  server  10.0.1.10:8080;
}

server {
  listen   80;
  server_name  _;
  access_log  /var/log/nginx/riak_cs.access.log;

  location / {
      proxy_set_header Host $http_host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_redirect off;

      proxy_connect_timeout      90;
      proxy_send_timeout         90;
      proxy_read_timeout         90;

      proxy_buffer_size          64k;  # If set to a smaller value,
                                       # nginx can complain with a
                                       # "headers too large" error
      
      proxy_pass http://riak_cs_host;
    }
}
```

Note that the directive `proxy_set_header Host $http_host` is essential to
ensure that the `HTTP Host:` header is passed to Riak CS as received, rather
than being translated into hostname or address of the Riak CS backend server.
