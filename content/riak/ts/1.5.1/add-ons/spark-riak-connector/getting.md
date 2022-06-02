---
title: "Getting the Spark-Riak Connector Add-on (Riak TS)"
description: ""
project: "riak_ts"
project_version: "1.5.1"
menu:
  riak_ts-1.5.1:
    name: "Get Spark-Riak Connector"
    identifier: "spark_riak_get"
    weight: 102
    parent: "addons_spark_riak"
toc: true
---

> **Note:**
>
> Please be sure to check the lastest release version by going to https://github.com/basho/spark-riak-connector/releases/latest

If you are using Scala or Java, the best way to get the Spark-Riak connector library to use with your project is to have SBT or Maven download the library at build time. This can be done by including the coordinates of the Spark-Riak connector library in the your `build.sbt` or `pom.xml` file. You can also manually download the Spark-Riak connector library before build time and include it in a local repository.

If you are using Python, the best way to get the Spark-Riak connector is to download the library and include its location in the Spark classpath when submitting your project to Spark.

An alternative way to get the Spark-Riak connector library for Scala, Java, and Python is to include the Spark-Riak connector library in the packages option when submitting your app to Spark. The Spark-Riak connector will automatically be downloaded from spark-packages.org.

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

## Python

You can download the Spark-Riak connector library by going to https://github.com/basho/spark-riak-connector/releases/latest and clicking on the latest release jar file.

You can also download and install the library in your home directory by running:

```bash
curl https://bintray.com/artifact/download/basho/data-platform/com/basho/riak/spark-riak-connector/»VERSION«/spark-riak-connector-»VERSION«-uber.jar \
                    -o ~/spark-riak-connector-»VERSION«-uber.jar
```

Once you've downloaded the connector, you can add it to the driver classpath when submitting your Python app to Spark, like this:

```bash
/path/to/spark-submit \
    --master "local[*]" \
    --driver-class-path /path/to/spark-riak-connector-»VERSION«-uber.jar \
    /path/to/your-python-script.py
```

## Alternative (Scala, Python, Java)

As an alternative, you can have Spark automatically download it when starting up the Spark shell as show by the following:

For Scala, Python, & Java:

```
$SPARK_HOME/bin/spark-submit \
--repositories https://dl.bintray.com/basho/data-platform \
--packages com.basho.riak:spark-riak-connector:»VERSION«
```

Alternative for Scala:

```
$SPARK_HOME/bin/spark-shell \
--repositories https://dl.bintray.com/basho/data-platform \
--packages com.basho.riak:spark-riak-connector:»VERSION«
```

Alternative for Python:

```
$SPARK_HOME/bin/pyspark \
--repositories https://dl.bintray.com/basho/data-platform \
--packages com.basho.riak:spark-riak-connector:»VERSION«
```
