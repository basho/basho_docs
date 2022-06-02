---
title: "Getting Started with Python"
description: ""
project: "riak_kv"
project_version: "2.0.0"
menu:
  riak_kv-2.0.0:
    name: "Python"
    identifier: "getting_started_python"
    weight: 102
    parent: "developing_getting_started"
toc: true
aliases:
  - /riak/2.0.0/dev/taste-of-riak/python
  - /riak/kv/2.0.0/dev/taste-of-riak/python
---



If you haven't set up a Riak Node and started it, please visit [Running A Cluster]({{<baseurl>}}riak/kv/2.0.0/using/running-a-cluster) first.

To try this flavor of Riak, a working installation of Python is
required, with Python 2.7 preferred. One of the Python package managers,
e.g. `setuptools` or `pip`, is also required to install the client
package.

You may install `setuptools` on OS X through MacPorts by running `sudo
port install py-distribute`. `setuptools` and `pip` are included in the
Homebrew formula for Python on OS X as well. Just run `brew install
python`.

## Prerequisites

First, you must install some packages needed by the Riak Python client:

* `python-dev` --- Header files and a static library for Python
* `libffi-dev` --- Foreign function interface library
* `libssl-dev` --- libssl and libcrypto development libraries

### Ubuntu (12.04 & 14.04)

```bash
sudo apt-get install python-dev libffi-dev libssl-dev
```

## Client Setup

The easiest way to install the client is with `easy_install` or `pip`.
Either of the commands below will ensure that the client and all its
dependencies are installed and on the load path. Depending on where your
Python libraries are held, these may require `sudo`.

```bash
easy_install riak
pip install riak
```

To install from source, download the latest Python client from GitHub
([zip](https://github.com/basho/riak-python-client/archive/master.zip),
[GitHub repository](https://github.com/basho/riak-python-client)), and
extract it to your working directory.

Now, let's build the client.

```bash
python setup.py install
```

## Connecting to Riak

Now, let's start the Python REPL and get set up. Enter the following
into the Python REPL:

```python
import riak
```
If you are using a single local Riak node, use the following to create a
new client instance:

```python
myClient = riak.RiakClient(pb_port=8087, protocol='pbc')

# Because the Python client uses the Protocol Buffers interface by
# default, the following will work the same:
myClient = riak.RiakClient(pb_port=8087)
```

If you set up a local Riak cluster using the [[five-minute install]]
method, use this code snippet instead:

```python
myClient = riak.RiakClient(pb_port=10017, protocol='pbc')
```

We are now ready to start interacting with Riak.

## Next Steps

[CRUD Operations]({{<baseurl>}}riak/kv/2.0.0/developing/getting-started/python/crud-operations)
