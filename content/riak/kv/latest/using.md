---
layout: latest_redirect
sitemap_exclude: true
project: riak_kv
sitemap:
  priority: 0
replace_text: latest
latest_text: "{latest}"
aliases:
  - "/riak/latest/using/"
  - "/riakkv/latest/using/"
---

# Redirect page

Redirects `someroot/latest/somepath` to `someroot/{latest}/somepath`
where `{latest}` is the `project_descriptions.{project}.latest` value
and `{project}` is the metadata value of `project` above.

This page exists solely to redirect from the generated URL to the latest version of
that url.
