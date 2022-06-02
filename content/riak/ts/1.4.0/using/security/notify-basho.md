---
title_supertext: "Security"
title: "Notifying Basho"
description: "Contacting Basho about security issues."
menu:
  riak_ts-1.4.0:
    name: "Notifying Basho"
    identifier: "security_notify_basho"
    weight: 140
    parent: "security"
project: "riak_ts"
project_version: "1.4.0"
toc: true
aliases:
    - /riakts/1.4.0/using/security/notify-basho
---

Data security is an important and sensitive issue. A real-world approach to security allows us to balance appropriate levels of security and related overhead while creating a fast, scalable, and operationally straightforward database.

## Continuous Improvement

Though we make every effort to thwart security vulnerabilities whenever
possible, no system is completely secure. We will never claim that Riak is 100% secure (and you should seriously doubt anyone who claims their solution is). What we can promise is that we openly accept all vulnerabilities from the community. When appropriate, we'll publish and make every attempt to quickly
address these concerns.

## Balance

More layers of security increase operational and administrative costs.
Sometimes those costs are warranted, sometimes they are not. Our
approach is to strike an appropriate balance between effort, cost, and
security.

For example, Riak does not have fine-grained role-base security. Though
it can be an attractive bullet-point in a database comparison chart,
you're usually better off finely controlling data access through your
application or a service layer.

## Contact Basho

If you discover a potential security issue, please email us at
**security@basho.com**, and allow us 48 hours to reply.

We appreciate the effort of the community working with us to create the most secure solution we can. This allows us to open a dialogue with the security
community on how best to handle a possible exploit without putting any
users at risk.

For sensitive topics, you may send a secure message. The security team
has the following GPG key:

```
-----BEGIN PGP PUBLIC KEY BLOCK-----
Version: GnuPG v1.4.12 (Darwin)

mQENBFAQM40BCADGjCmwn9Q9xpWfJ4HpKGwt5kGyf4Oq4PglC28MhtscT9cGwtJv
gRK1ckzkwhCdw6uQKRN3o3iVFHFp+uD8G28zs1fGNfpUZls7WV29WyxfIgB3f01Q
Ll6tiZ2fLG69lSlLTPn7JlzZz1sRVrAKdwUVEYRKCidF0bqaztBCkKbcNAmIvV1E
TboEGMPLXqOnK2134NP+tp0B15oNwSQd9jmOrClvhCF5NR4ATQguS5ecp05/GldZ
8vQQ1XOBc2uiuWpzvhD2CAXQ/Spxir8JjbqpzjPo6d4yte7pYvx6wfnJ9b2KC+sn
AtdqqQslZ3saceXAFXFOIGk7NOq8LSattmRbABEBAAG0GkJhc2hvIDxzZWN1cml0
eUBiYXNoby5jb20+iQE4BBMBAgAiBQJQEDONAhsDBgsJCAcDAgYVCAIJCgsEFgID
AQIeAQIXgAAKCRDEq056TdGVhHl7B/9rXnzZOdC7M8NN+BAEO8kucw0dXGhgcahs
zS81WDRpRJD1fi+QBinfohGg2phIq5TlrXNmduFwCpvyujNkeiCr+Nh00mp6SdU2
m7XFzfPIz3ZWR0YNdvruaf0W5K6jAaHcJkkc3Xwpgk6rxTcNwWUqYRGD7zie4Iad
At0WLJXMUvJH2XoMf8MGO5mHspkqC5M/HvNvH3ZG5CldIHPqgZdg4NXMcGtFAr8z
72wFamick31oCpJyWq+AloOxh3mJpfhp94EBrc/lGbbOD/Sg4oyT+B/4Ee0zWqN5
hDBefi3FCyjo2NuhM1YyRrrvWe7Kwaj8iuItYPIpEwGUqEJzZ7kYuQENBFAQM40B
CAC4J0Pb1WXjGpsQnfOdzZUq57x63RaVA74IIuLSU7v//04wNgNGiLdMbz4isr6K
5NfXTu0i+GqQdcj7UnajwxYCUEnXYpKQBLfT82tTgdw/DPXYgSnxIC02POrwCnhr
wSDbUryuTdbZFS13HPrQPdOXZlmG8oHOgu04a9vPUlkshYmUZm+zRY2FIuW8fJ44
ysJBm49hxkF9WuyGnNiU8UJEvw0sS63x4EUkYdJXLzzdC9T8/t8HGV3aKFEZ3km0
GgYUlt04FdWtFjYcMQnrhJSf7atxwQLpfH78sFCyEH+PFIRfnkirVx9TbN0QSw/z
VaRNxJQde2SHfEft66mf0RJ5ABEBAAGJAR8EGAECAAkFAlAQM40CGwwACgkQxKtO
ek3RlYRPFwf+LiHlf9tCqRLwmI2X8bBmoQTV/Eb4pbPF/1WR6W/afAMp4ZiLpWtn
XeZ9UNdnQDPJIMPhaWrPHB4oLCnDBm1m6wq6FVjHcDur+s7QtWnnTuaVKBDKY42T
NkFj+WP3ZBsfDBtt49KRLm0bWqzkhK7IA+1DMKRmTUhf0tIeLb0um0hL+mXNucrE
dMk+Fdh/54IfHMMw3GwtNd+ZMLf8cht+z3Z0Y0qONe0ClfkiligYItD+P5tufhew
HtU5clY0rP8W/Nr7tC+ZGH2bjT1bmN1E9IM4wjBdyWGTosvY6ciIxuY5p5Iy/UhB
7Xk9zl4ZkKcsVnuscYQPNE2jb393XAhFEg==
=1KRp
-----END PGP PUBLIC KEY BLOCK-----
```
