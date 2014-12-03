---
title: Using Riak as a Document Store
project: riak
version: 2.0.0+
document: tutorials
audience: intermediate
keywords: [document-store, developers, search, json]
---

Although Riak wasn't explicitly created as a document store, two
features recently added to Riak---[[Riak Search|Using Search]] and
[[Riak Data Types|Using Data Types]]---make it possible to use Riak as a
highly scalable document store with rich querying capabilities. In this
tutorial, we'll build a basic implementation of a document store using
[[Riak maps|Using Data Types#Maps]].

## Basic Approach

Riak Search enables you to implement a document store in Riak in a
variety of ways. You could, for example, store and query JSON objects or
XML and then retrieve them later via Solr queries. In this tutorial,
however, we will store data in [[Riak maps|Using Data Types#Maps]],
index that data using Riak Search, and then run Solr queries against
those stored objects.

You can think of these Search indexes as **collections**. Each indexed
will have a document ID generated automatically by Search, and because
we're not interested in running normal [[key/value queries|Key/Value
Modeling]] on these objects, we'll allow Riak to assign [[keys|Keys and
Object]] automatically. This means that all we have to do is worry about
the bucket type and/or bucket when storing objects.

## Use Case

Let's say that we're building a WordPress-style CMS and storing blog
posts in Riak. We will be storing the following information about each
post:

* Title
* Author
* Content (the body of the post)
* Keywords associated with the post
* Date posted
* Whether the post has been published on the site

For each of those pieces of information, we'll need to decide on (a)
which Riak Data Type most directly corresponds and (b) which Solr type
we want to associate with the info. It's important to bear in mind that
Riak Data Types can be indexed as a wide variety of things, e.g.
registers as Solr text fields, sets as multi-valued datetimes, etc. The
table below shows which Riak Data Type and Solr type we'll be using for
each field in our Riak maps.

Info | Riak Data Type | Solr type
:----|:---------------|:---------
Post title | Register | String
Post author | Register | String
Post content | Register | Text
Keywords | Set | Multi-valued string
Date posted | Register | Datetime
Whether the post is currently in draft form | Flag | Boolean

Before we start actually creating and storing blog posts, let's set up
Riak Search with an appropriate index and schema.

## Creating a Schema and Index

In the documentation on [[search schemas|Search Schema]], you'll find a
baseline schema to be used for creating custom schemas. We'll use that
baseline schema here and add the following fields to the `<fields>`
list:

```xml
<dynamicField name="title_register" type="string" indexed="true" stored="true" />
<dynamicField name="author_register" type="string" indexed="true" stored="true" />
<dynamicField name="content_register" type="text" indexed="true" stored="true" />
<dynamicField name="keywords_set" type="string" indexed="true" stored="true" multiValued="true" />
<dynamicField name="date_register" type="datetime" indexed="true" stored="true" />
<dynamicField name="published_flag" type="boolean" indexed="true" stored="true" />
```

You can see the full schema [on
GitHub](https://github.com/basho/basho_docs/raw/master/source/data/blog_post_schema.xml).
Let's store that schema in a file called `blog_post_schema.xml` and
upload that schema to Riak:

```curl
curl -XPUT $RIAK_HOST/search/schema/blog_post_schema \
     -H 'Content-Type: application/xml' \
     --data-binary @blog_post_schema.xml
```

With our schema uploaded, we can create an index called `blog_posts` and
associate that index with our schema:

```curl
curl -XPUT $RIAK_HOST/search/index/blog_posts \
     -H 'Content-Type: application/json' \
     -d '{"schema": "blog_post_schema"}'
```

## How Collections will Work

Collections are not a concept that is native to Riak but we can easily
mimic collections by thing of a bucket type as a collection. When we
associate a bucket type with a Riak Search index, all of the objects
stored in any bucket of that bucket type will be queryable on the basis
of that one index. For this tutorial, we'll create a bucket type called
`cms` and think of that as a collection. We could also restrict our
`blog_posts` index to a single bucket just as easily and think of that
as a queryable collection, but we will not do that in this tutorial.

The advantage of the bucket-type-based approach is that we could store
blog posts from different blogs in different blog posts and query them
all at once as part of the same index. It depends on the use case at
hand. In this tutorial, we'll only be storing posts from one blog, which
is called "Cat Pics Quarterly" and provides in-depth theoretical
discussions of cat pics with a certain number of Reddit upvotes. All of
the posts in this blog will be stored in the bucket
`cat_pics_quarterly`.

First, let's create our `cms` bucket type and associate it with the
`blog_posts` index:

```bash
riak-admin bucket-type create cms '{"props":{"datatype":"map","search_index": "blog_posts"}}'
riak-admin bucket-type activate cms
```

Now, any object stored in any bucket of the type `cms` will be indexed
as part of our "collection."

## Storing Blog Posts as Maps

Now that we know how each element of a blog post can be translated into
one of the Riak Data Types, we can create an interface in our
application to serve as that translation layer. Using the method
described in [[Data Modeling with Riak Data Types]], we can construct a
class that looks like this:

```java
import java.util.Set;

public class BlogPost {
    private String title;
    private String author;
    private String content;
    private Set<String> keywords;
    private DateTime datePosted;
    private Boolean published;
    private static final String bucketType = "cms";

    private Location location;

    private RiakClient client;

    public BlogPost(RiakClient client
                    String bucketName,
                    String title,
                    String author,
                    Set<String> keywords,
                    DateTime datePosted,
                    Boolean published) {
      this.client = client;
      this.location = new Location(new Namespace(bucketType, bucketName), null);
      this.title = title;
      this.author = author;
      this.keywords = keywords;
      this.datePosted = datePosted;
      this.published = published;
    }

    public void store() throws Exception {
        RegisterUpdate titleUpdate = new RegisterUpdate(title);
        RegisterUpdate authorUpdate = new RegisterUpdate(author);
        SetUpdate keywordsUpdate = new SetUpdate();
        for (String keyword : keywords) {
            keywordsUpdate.add(keyword);
        }
        RegisterUpdate dateUpdate =
            new RegisterUpdate(datePosted.toString("YYYY-MM-DD HH:MM:SS"));
        if (published) {
            FlagUpdate published = new FlagUpdate(published);
        }
        FlagUpdate publishedUpdate = new FlagUpdate(published);
        MapUpdate mapUpdate = new MapUpdate()
            .update("title", titleUpdate)
            .update("author", authorUpdate)
            .update("keywords", keywordsUpdate)
            .update("date", dateUpdate)
            .update("published", publishedUpdate);
        UpdateMap storeBlogPost = new UpdateMap.Builder(location)
            .build();
        client.execute(storeBlogPost);
    }
}
```

Now, we can store some blog posts. We'll start with just one:

```java
Set<String> keywords = new HashSet<String>();
keywords.add("adorbs");
keywords.add("cheshire");

BlogPost post1 = new BlogPost(client, // client object
                              "cat_pics_quarterly", // bucket
                              "This one is so lulz!", // title
                              "Cat Stevens", // author
                              keywords, // keywords
                              new DateTime(), // date posted
                              true); // published
try {
    post1.store();
} catch (Exception e) {
    System.out.println(e);
}
```

To store each blog post as a map, follow the example in [[Data Modeling
with Riak Data Types]]. That will show you how to create a translation
layer between blog objects and Riak maps.

## Querying

Now that we have some blog posts stored in our "collection," we can
start querying for whatever we'd like. Let's say that we want to find
all blog posts with the keyword `funny` (after all, some cat pics are
quite serious).

```curl
curl "$RIAK_HOST/search/query/blog_posts?wt=json&q=keywords_set:funny"
```

Or we can find posts that contain the word `furry`:

```curl
curl "$RIAK_HOST/search/query/blog_posts?wt=json&q=content_register:furry"
```

Here are some more possible queries:

Info | Query
:----|:-----
Unpublished posts | `published_flag:false`
Titles that begin with `Loving*` | `title_register:Loving*`
Post bodies containing the words `furry` and `jumping` | `content_register:[furry AND jumping]`

## Creating an Interface

While one way to

## Conclusion

The nice thing about this approach is that we don't have to limit
ourselves to normal K/V operations. We didn't even have to concern
ourselves with keys. All we had to do 
