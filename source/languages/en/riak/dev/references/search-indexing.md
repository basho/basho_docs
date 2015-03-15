---
title: Riak Search 1.0 Indexing Reference
project: riak
version: 1.0.0+
document: reference
toc: true
audience: advanced
keywords: []
moved: {
  '1.4.0-': '/cookbooks/Riak-Search---Indexing'
}
---

{{#2.0.0+}}
<div class="info">This document refers to the now <em>deprecated</em> Riak Search. Visit [[Using Search]] for information about the new Riak Search (codenamed Yokozuna). Note that the new Riak Search does not currently support the manual index management that this document covers, instead preferring to automatically index Riak values.</div>
{{/2.0.0+}}


There are numerous ways to index a document in Riak Search.

## Indexing from the Command Line

The easiest way to index documents stored on the filesystem is to use the "search-cmd" command line tool:

```bash
$ bin/search-cmd index <INDEX> <PATH>
```

Parameters:

* *&lt;INDEX&gt;* - The name of an index.
* *&lt;PATH&gt;* - Relative or absolute path to the files or directories to recursively index. Wildcards are permitted.

This documents will be indexed into the default field defined by the Index's schema, using the base filename plus extension as the document ID.

```bash
$ bin/search-cmd index my_index files/to/index/*.txt
```

## Deleting from the Command Line

To remove previously indexed files from the command line, use the "search-cmd" command line tool.

```bash
$ bin/search-cmd delete <INDEX> <PATH>
```

Parameters:

* *&lt;INDEX&gt;* - The name of an index.
* *&lt;PATH&gt;* - Relative or absolute path to the files or directories to recursively delete. Wildcards are permitted.

For example:

```bash
$ bin/search-cmd delete my_index files/to/index/*.txt
```

Any documents matching the base filename plus extension of the files found will be removed from the index. The actual contents of the files are ignored during this operation.

## Indexing using the Erlang API

The following Erlang functions will index documents stored on the filesystem:


```erlang
search:index_dir(Path).
search:index_dir(Index, Path).
```

Parameters:

* *Index* - The name of the index.
* *Path* - Relative or absolute path to the files or directories to recursively index. Wildcards are permitted.

The documents will be indexed into the default field defined by the Index's schema, using the base filename plus extension as the document ID.

```erlang
search:index_dir(<<"my_index">>, "files/to/index/*.txt").
```

Alternatively, you can provide the fields of the document to index.

```erlang
search:index_doc(Index, DocId, Fields)
```

Parameters:

* *&lt;INDEX>* - The name of the index.
* *&lt;DocID>* - The document ID.
* *&lt;Fields>* - A key/value list of fields to index.

For example:

```erlang
search:index_doc(<<"my_index">>, <<"my_doc">>, [{<<"title">>, <<"The Title">>}, {<<"content">>, <<"The Content">>}])
```

## Deleting using the Erlang API

The following Erlang functions will remove documents from the index:

```erlang
search:delete_dir(Path).
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


```curl
$ curl -XPOST http://localhost:8098/solr/books/update \
       -H 'content-type:text/xml' --data-binary @tests/books.xml
```

Alternatively, you can index Solr files on the command line:

```bash
$ bin/search-cmd solr my_index path/to/solrfile.xml
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
