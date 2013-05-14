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
software based.

Riak CS users have reported success in using Riak CS with a variety of load
balancing and proxy solutions. Common solutions include proprietary hardware
based load balancers, cloud based load balancing options, such as Amazon's
Elastic Load Balancer, and open source software based projects like HAProxy.

This guide briefly explores the commonly used open source software based
solution HAProxy, and provides some configuration and operational tips
gathered from community users and operations oriented engineers at Basho.

While it is by no means an exhaustive overview of the topic, this guide should
provide a starting point for implementing an HAProxy solution.

## HAProxy

[HAProxy](http://haproxy.1wt.eu/) is a fast and reliable open source solution
for load balancing and proxying of HTTP and TCP based application traffic.

Users have reported success in using HAProxy in combination with Riak CS in a
number of configurations and scenarios. Much of the information and example
configuration for this section is drawn from experiences of users in the
Riak CS community in addition to suggestions from Basho engineering.

### Example Configuration

The following is an example starting point configuration for HAProxy to act
as a load balancer to a Riak CS cluster for access by clients using
the HTTP interface.

<div class="info">The operating system's open files limits need to be
  greater than 256000 for the example configuration that follows. Consult
  the [[Open Files Limit|Open-Files-Limit]] documentation for details on
  configuring the value for different operating systems.</div>
  
```
global
        log 127.0.0.1     local0
        log 127.0.0.1     local1 notice
        maxconn           12083
        spread-checks     5
        daemon

defaults
        log               global
        option            dontlognull
        option            redispatch
        option            allbackups
        no option         httpclose
        retries           3
        maxconn           12083
        contimeout        5000
        srvtimeout        60000
frontend riak_cs
        bind              10.0.24.100:8080
        # Example bind below for SSL termination
        # bind              10.0.24.100:8443 ssl crt /opt/local/haproxy/etc/data.pem
        mode              http
        option            httplog
        capture           request header Host len 64
        acl good_ips      src -f /opt/local/haproxy/etc/gip.lst
        block if          !good_ips
        use_backend       riak_cs_backend if good_ips

backend riak_cs_backend
        mode              http
        balance           roundrobin
        timeout connect 60s
        timeout http-request 60s
        server riak1 r1s01.example.com:8081 weight 1 maxconn 12083  check
        server riak2 r1s02.example.com:8081 weight 1 maxconn 12083  check
        server riak3 r1s03.example.com:8081 weight 1 maxconn 12083  check
        server riak4 r1s04.example.com:8081 weight 1 maxconn 12083  check
        server riak5 r1s05.example.com:8081 weight 1 maxconn 12083  check
```

Note that the above example is considered a starting point and is a work in progress. You should carefully examine the configuration and change it
according to your specific environment.
