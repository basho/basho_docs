---
title_supertext: "Spark-Riak Connector Add-on (Riak KV)"
title: "Building and Testing"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Building & Testing"
    identifier: "spark_riak_build_test"
    weight: 103
    parent: "addons_spark_riak"
toc: true
canonical_link: "docs.basho.com/riak/kv/latest/add-ons/spark-riak-connector/building-testing"
---

If you want to download the source code of the Spark-Riak connector, build it, and install the results in your local repo, this is the document for you! Keep reading for instructions on downloading, building, and installing the connector.

If you just want to add the connector as a dependency to your application, you'll want to go [here](../getting) instead.


## Prerequisites

In order to build the Spark-Riak connector, you'll need to have the following installed: 

* [Java OpenJDK 8](http://openjdk.java.net/install/)
* [Maven 3](https://maven.apache.org/download.cgi)
* [Spark 1.6](http://spark.apache.org/docs/latest/#downloading)
* [Riak KV](http://docs.basho.com/riak/kv/2.2.0/setup/installing/)


## Download

Once you've installed all of the prerequisites, you need to clone the Spark-Riak connector GitHub repository. 

Make sure you've navigated to the directory you want the Spark-Riak connector to be in, then run:

```bash
git clone https://github.com/basho/spark-riak-connector.git
```

## Build

After cloning this repository, you can build the Spark-Riak connector:

```bash
mvn clean install
```

When you run `install`, the integration tests will execute with the Maven Failsafe Plugin. This ensures the Spark-Riak connector can be built even if there is no Riak cluster running.

The following command should be used to skip tests:

```bash
mvn clean install -DskipTests
```

Once the connector is built there are several jars that are produced:
`spark-riak-connector/target/` contains `spark-riak-connector-{{version}}.jar` - this is the connector jar. 

You'll find the results of the build in your local maven repository in the com/basho/riak directory. Most likely that will be in your home directory and the path will look like this: `~/.m2/repository/com/basho/riak/`


## Test

For the Spark-Riak connector, unit tests are separated from integration tests. 
If there is no Riak installation running, it is still possible to successfully run unit tests:

```bash
mvn clean test
```

If Riak is installed it is possible to run both unit tests and integration test. Futhermore, KV-specific integration tests are separated from TS-specific ones. To choose which set of tests to run appropriate maven profile should be selected: 

Profile name |Tests                                      | Default |
-------------|-------------------------------------------|---------|
riak_ts      | TS-specific tests and majority of KV-tests| no      |
riak_kv      | KV-only tests                             | yes     |

```bash
mvn clean verify -P riak_ts
mvn clean verify -P riak_kv
```

A Riak host can be provided in `com.basho.riak.pbchost` variable:

```bash
mvn clean verify -P riak -Dcom.basho.riak.pbchost=myhost:8087
```

If Riak was installed with devrel and is running on localhost on 10017 port, it is possible to use special `devrel` Maven profile instead:

```bash
mvn clean verify -P devrel,riak
```

Or

```bash
mvn clean verify -P riak -Denvironment=devrel
```

Both of the above will accomplish the same as:

```bash
mvn clean verify -P riak -Dcom.basho.riak.pbchost=localhost:10017
```
