---
title: "Getting the Spark-Riak Connector Add-on (Riak KV)"
description: ""
project: "riak_kv"
project_version: "2.1.4"
menu:
  riak_kv-2.1.4:
    name: "Get Spark-Riak Connector"
    identifier: "spark_riak_get"
    weight: 102
    parent: "addons_spark_riak"
toc: true
canonical_link: "docs.basho.com/riak/kv/latest/add-ons/spark-riak-connector/getting"
---

> **Note:**
>
> Please be sure to check the lastest release version by going to https://github.com/basho/spark-riak-connector/releases/latest

If you are using Scala or Java, the best way to get the Spark-Riak connector library to use with your project is to have SBT or Maven download the library at build time. This can be done by including the coordinates of the Spark-Riak connector library in the your `build.sbt` or `pom.xml` file. You can also manually download the Spark-Riak connector library before build time and include it in a local repository.

An alternative way to get the Spark-Riak connector library for Scala or Java is to include the Spark-Riak connector library in the packages option when submitting your app to Spark. The Spark-Riak connector will automatically be downloaded from spark-packages.org.

## Scala

To add the Spark-Riak connector to your Scala project, add the following to your `build.sbt` file:

```
resolvers += "Basho Bintray Repo" at "https://dl.bintray.com/basho/data-platform"

libraryDependencies +=  "com.basho.riak" % "spark-riak-connector" % "$version" classifier "uber"
          exclude("org.apache.spark", "spark-core")
          exclude("org.apache.spark", "spark-sql")
```

## Java

To add the Spark-Riak connector to your Java project, add the following to your `pom.xml` file:

```
<dependencies>
    <dependency>
        <groupId>com.basho.riak</groupId>
        <artifactId>spark-riak-connector</artifactId>
        <version>$version</version>
        <classifier>uber</classifier>
    </dependency>
</dependencies>

<repository>
    <id>bintray</id>
    <url>https://dl.bintray.com/basho/data-platform</url>
</repository>
```

## Alternative (Scala, Java)

As an alternative, you can add the Spark-Riak connector library in the packages option when submitting the app to Spark. The Spark-Riak connector can be found on spark-packages.org. You can either download it and supply Spark with its local address or have spark automatically download it when starting up the Spark shell as show by the following.

**Scala, Java**
```
$SPARK_HOME/bin/spark-submit \
--repositories https://dl.bintray.com/basho/data-platform \
--packages com.basho.riak:spark-riak-connector:{{version}}
```

**Scala**
```
$SPARK_HOME/bin/spark-shell \
--repositories https://dl.bintray.com/basho/data-platform \
--packages com.basho.riak:spark-riak-connector:{{version}}
```
