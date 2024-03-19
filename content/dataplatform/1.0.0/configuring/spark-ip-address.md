---
title: "Set Spark IP Address"
description: ""
project: "dataplatform"
project_version: "1.0.0"
menu:
  dataplatform-1.0.0:
    name: "Set Spark IP Address"
    identifier: "configuring_spark_ip"
    weight: 104
    parent: "configuring"
toc: true
aliases:
  - /dataplatform/1.0.0/using-dataplatform/configuration/advanced/set-spark-ip-address/
  - /dataplatform/latest/configuring/spark-ip-address/
---

To bind Spark Master to a specific host you can manually set the Spark Master IP Address with:

```bash
sudo bash -c "echo 'SPARK_MASTER_IP=»YOUR PUBLIC IP« >> »YOUR_PATH_TO BDP«/priv/spark-master/conf/spark-env.sh'"
```
