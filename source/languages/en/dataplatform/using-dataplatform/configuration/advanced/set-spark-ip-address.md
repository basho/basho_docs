---
title: Set Spark IP Address
project: dataplatform
version: 1.1.0+
document: guide
index: true
audience: advanced
---

To bind Spark Master to a specific host you can manually set the Spark Master IP Address with:

```bash
sudo bash -c "echo 'SPARK_MASTER_IP=»YOUR PUBLIC IP« >> »YOUR_PATH_TO BDP«/priv/spark-master/conf/spark-env.sh'"
```
