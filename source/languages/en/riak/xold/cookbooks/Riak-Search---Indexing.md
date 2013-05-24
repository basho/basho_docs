---
title: Indexing Search
project: riak
version: 0.14.0+
document: cookbook
toc: true
audience: intermediate
keywords: [search, 2i]
---

Indexing a document is the act of:

1. Reading a document.
2. Splitting the document into one or more fields.
3. Splitting the fields into one or more terms.
4. Normalizing the terms in each field.
5. Writing the {Field, Term, DocumentID} postings to an index.

There are numerous ways to index a document in Riak Search.

## Indexing from the Command Line

The easiest way to index documents stored on the filesystem is to use the "search-cmd" command line tool:


```bash
bin/search-cmd index <INDEX> <PATH>
```

Parameters:

* *&lt;INDEX&gt;* - The name of an index.
* *&lt;PATH&gt;* - Relative or absolute path to the files or directories to recursively index. Wildcards are permitted.

For example:


```bash
bin/search-cmd index my_index files/to/index/*.txt
```

The documents will be indexed into the default field defined by the Index's schema, using the base filename plus extension as the document ID.


## Deleting from the Command Line

To remove previously indexed files from the command line, use the "search-cmd" command line tool:


```bash
bin/search-cmd delete <INDEX> <PATH>
```


Parameters:

* *&lt;INDEX&gt;* - The name of an index.
* *&lt;PATH&gt;* - Relative or absolute path to the files or directories to recursively delete. Wildcards are permitted.

For example:


```bash
bin/search-cmd delete my_index files/to/index/*.txt
```


Any documents matching the base filename plus extension of the files found will be removed from the index. The actual contents of the files are ignored during this operation.

## Indexing using the Erlang API

The following Erlang functions will index documents stored on the filesystem:


```erlang
search:index_dir(Path).
```



```erlang
search:index_dir(Index, Path).
```

Parameters:

* *Index* - The name of the index.
* *Path* - Relative or absolute path to the files or directories to recursively index. Wildcards are permitted.

For example:


```erlang
search:index_dir(<<"my_index">>, "files/to/index/*.txt").
```


The documents will be indexed into the default field defined by the Index's schema, using the base filename plus extension as the document ID.

Alternatively, you can provide the fields of the document to index:


```bash
search:index_doc(Index, DocId, Fields)
```


Parameters:

* *&lt;INDEX&gt;* - The name of the index.
* *&lt;DocID&gt;* - The document ID.
* *&lt;Fields&gt;* - A key/value list of fields to index.

For example:


```erlang
search:index_doc(<<"my_index">>, <<"my_doc">>, [{<<"title">>, <<"The Title">>}, {<<"content">>, <<"The Content">>}])
```

## Deleting using the Erlang API

The following Erlang functions will remove documents from the index:


```erlang
search:delete_dir(Path).
```



```erlang
search:delete_dir(Index, Path).
```


Parameters:

* *Index* - The name of the index. Defaults to `search`.
* *Path* - Relative or absolute path to the files or directories to recursively delete. Wildcards are permitted.

For example:


```erlang
search:delete_dir(<<"my_index">>, "files/to/index/*.txt").
```


Any documents matching the base filename plus extension of the files found will be removed from the index. The actual contents of the files are ignored during this operation.

Alternatively, you can delete a document by it's id:


```erlang
search:delete_doc(<<"my_index">>, <<"my_doc">>).
```


Parameters:

* *Index* - The name of the index.
* *DocID* - The document ID of the document to delete.

## Indexing using the Solr Interface

Riak Search supports a Solr-compatible interface for indexing documents via HTTP. Documents must be formatted as simple Solr XML documents, for example:


```xml
<add>
  <doc>
    <field name="id">DocID</field>
    <field name="title">Zen and the Art of Motorcycle Maintenance</field>
    <field name="author">Robert Pirsig</field>
    ...
  </doc>
  ...
</add>
```


Additionally, the Content-Type header must be set to 'text/xml'.

Riak Search currently requires that the field determining the document ID be named "id", and does not support any additional attributes on the "add", "doc", or "field" elements. (In other words, things like "overwrite", "commitWithin", and "boost" are not yet supported.)

The Solr interface does NOT support the &lt;commit /&gt; nor &lt;optimize /&gt; commands. All data is committed automatically in the following stages:

* Incoming Solr XML document is parsed. If XML is invalid, an error is returned.
* Documents fields are analyzed and broken into terms. If there are any problems, an error is returned.
* Documents terms are indexed in parallel. Their availability in future queries is determined by the storage backend.

By default, the update endpoint is located at "http://hostname:8098/solr/update?index=INDEX".

Alternatively, the index can be included in the URL, for example "http://hostname:8098/solr/INDEX/update".

To add data to the system with Curl:


```bash
curl -X POST -H text/xml --data-binary @tests/books.xml http://localhost:8098/solr/books/update
```


Alternatively, you can index Solr files on the command line:


```bash
bin/search-cmd solr my_index path/to/solrfile.xml
```

## Deleting using the Solr Interface

Documents can also be deleted through the Solr interface via two methods, either by Document ID or by Query.

To delete documents by document ID, post the following XML to the update endpoint:


```xml
<delete>
  <id>docid1</id>
  <id>docid2</id>
  ...
</delete>
```


To delete documents by Query, post the following XML to the update endpoint:


```xml
<delete>
  <query>QUERY1</query>
  <query>QUERY2</query>
  ...
</delete>
```


Any documents that match the provided queries will be deleted.

## Behind the Scenes

The details of how Riak Search works, and how it manages your indexes, are generally not something you'll be concerned with.  It's important, however, to be aware of the more "opinionated" aspects of Riak Search, which are very similar to those found in Solr itself.

## Stop Words

Riak Search implements Stop Words, much as you might find in Solr itself:

[[http://wiki.apache.org/solr/AnalyzersTokenizersTokenFilters#solr.StopFilterFactory]]

The source code for Riak Search's "default analyzer factory" can be found here:

[[http://github.com/basho/riak_search/blob/master/src/text_analyzers.erl]]

In short, the following words will be skipped when indexing. The official list is maintained in the source file, linked above:


```erlang
is_stopword(Term) when length(Term) == 2 ->
    ordsets:is_element(Term, ["an", "as", "at", "be", "by", "if", "in", "is", "it", "no", "of", "on", "or", "to"]);
is_stopword(Term) when length(Term) == 3 ->
    ordsets:is_element(Term, ["and", "are", "but", "for", "not", "the", "was"]);
is_stopword(Term) when length(Term) == 4 ->
    ordsets:is_element(Term, ["into", "such", "that", "then", "they", "this", "will"]);
is_stopword(Term) when length(Term) == 5 ->
    ordsets:is_element(Term, ["their", "there", "these"]);
```


If you plan to use these words in an 'exact phrase' query, you may want to use another analyzer factory.  Be aware, however, that these words can quickly clog up your index.  Future analyzers can fill the void, while maintaining some degree of efficiency.

As an example of how this might affect your queries: If you search using the exact phrase:

```bash
?q=\"the dog is\"
```

With the aforementioned stop words enabled, your query is reduced to "dog".  Perhaps not what you had in mind.
