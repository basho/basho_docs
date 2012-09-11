---
title: Enabling Search
project: riak
version: 1.0.0+
document: cookbook
audience: beginner
keywords: [search]
---

## Enabling Riak Search 

Riak Search is enabled in the [[app.config|Configuration-Files#app.config]] file. Simply change the setting to "true" in Riak Search Config section (shown below).

```erlang 
%% Riak Search Config
{riak_search, [
               %% To enable Search functionality set this 'true'.
               {enabled, false}
              ]},
```

You will have to make this change on every node in your Riak cluster, and it will require you to shut down and restart the node for the changes to take effect. (You can use [[Riaknostic|http://riaknostic.basho.com/]] to check if Search is enabled on all your nodes.)

After you have made these changes, Riak Search will automatically start up when [[Riak is started|Installation]].
