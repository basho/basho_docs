---
title: Blog Posts, Articles and Other Content
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [use-cases]
moved: {
  '1.4.0-': '/cookbooks/use-cases/blogs'
}
---

## Simple Case

The simplest way to model blog posts, articles or other content is by creating a bucket in Riak with some unique attribute for logical division of content - perhaps  "blogs" "articles" or something similar. Keys could be unique identifiers for posts - perhaps the article title, a combination of the title and time/date, or perhaps an integer that can be used as part of a URL string. You can store content anyway you want - HTML blobs, plain text, JSON or XML, or another document type. Keep in mind that data in Riak is opaque, so Riak won't "know" about the object unless it is indexed with Riak Search.


## Complex Case

Setting up a data model for content gets more complex based on the querying and search requirements of your application or its various aspects. For example, you may have different kinds of content you want to generate in a view - not just a post, but comments, users and profile information, etc. For many Riak developers, it will make sense to divide out content into different buckets - a bucket for comments, for example, that would be stored in the Riak cluster along with the posts bucket. Comments for a given post could be stored as a document with the same key as the content post - only the bucket/key combination must be unique. Or you could store each comment with its own ID. Loading the full view with comments would mean your application would need to call from the posts and comments bucket to assemble the view. Another common case that is slightly more complex is when you want to perform search and query operations on content beyond just retrieving key/value pairs. Riak Search, our full-text search engine that implements a SOLR-like API and query model, is a great use case for text content, and users like Clipboard have some great work available on how to optimize search performance. For lighter-weight querying, secondary indexes allow you to add additional metadata to objects for querying on exact match or range values. Using secondary indexes, you could tag posts with dates, timestamps, topic areas or others of interest. It's important to make sure that your dataset will be a use case with 2i, as it can be performance-prohibitive in clusters with over 512 partitions.

## Community Examples

<table class="links">
  <tr>
    <td><a href="http://blog.clipboard.com/2012/03/18/0-Milking-Performance-From-Riak-Search" class="vid_img" target="_blank"><img src="/images/milking-perf-from-riak.png" title="Milking Performance"></a>
    </td>
    <td>Clipboard on <a href="http://blog.clipboard.com/2012/03/18/0-Milking-Performance-From-Riak-Search" target="_blank">storing and searching data in Riak.</a>
  </tr>
  <tr>
    <td><a href="http://media.basho.com/pdf/Linkfluence-Case-Study-v2-1.pdf" class="vid_img" link target="_blank"><img src="/images/linkfluence-case-study.png" title="Milking Performance"></a>
    </td>
    <td>Linkfluence case study on using Riak to <a href="http://media.basho.com/pdf/Linkfluence-Case-Study-v2-1.pdf" target="_blank">store social web content</a>.
  </tr>
  <tr>
    <td><a href="http://basho.com/assets/Basho-Case-Study-ideeli.pdf" class="vid_img" link target="_blank"><img src="/images/ideeli-case-study.png" title="Milking Performance"></a>
    </td>
    <td>ideeli case study on <a href="http://basho.com/assets/Basho-Case-Study-ideeli.pdf" target="_blank">serving web pages with Riak</a>.
  </tr>
</table>
