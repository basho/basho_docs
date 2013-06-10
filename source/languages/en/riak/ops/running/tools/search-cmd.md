---
title: search Command Line
project: riak
version: 0.10.0+
document: reference
toc: true
audience: beginner
keywords: [command-line, search]
moved: {
    '1.4.0-': '/references/Command-Line-Tools---search-cmd/'
}
---

# Command Line Tools - `search-cmd`

This script is used to interact with the Search functionality included with
Riak.  Make sure you've first
[[enabled  Search|Configuration Files#riak_search]]. The `INDEX` argument is
optional for all commands and defaults to `search`.


    search-cmd set-schema [INDEX] SCHEMAFILE : Set schema for an index.
    search-cmd show-schema [INDEX]           : Display the schema for an index.
    search-cmd clear-schema-cache            : Empty the schema cache on all nodes.
    search-cmd search [INDEX] QUERY          : Perform a search.
    search-cmd search-doc [INDEX] QUERY      : Perform a document search.
    search-cmd explain [INDEX] QUERY         : Ouputs the query plan.
    search-cmd index [INDEX] PATH            : Index files in a path.
    search-cmd delete [INDEX] PATH           : De-index files in a path.
    search-cmd solr [INDEX] PATH             : Run the Solr file.
    search-cmd install BUCKET                : Install kv/search integration hook
    search-cmd uninstall BUCKET              : Uninstall kv/search integration hook
    search-cmd test PATH                     : Run a test package


## set-schema

    set-schema [INDEX] SCHEMAFILE

Set the [[schema|Riak Search - Schema]] for a given index.  If you don't
explicitly set the schema for an index it will use the default schema.


## show-schema

    show-schema [INDEX]

Show the [[schema|Riak Search - Schema]] for a given index.


## clear-schema-cache

    clear-schema-cache

Search stores its schemas in Riak just like any other object.  However, to
avoid the costliness of getting an object each time the schema information is
needed it caches the schema object locally on each node.  If you've modified
your schema you'll want to clear this cache in order to make sure the latest
version is read from Riak.


## search

    search [INDEX] QUERY

Execute the given query on the index returning the document id, properties, and
score.  The [[query syntax|Riak Search - Querying]] is the same as Lucene.


## search-doc

    search-doc [INDEX] QUERY

Much like `search` but also returns all the fields too.

## explain

    explain [INDEX] QUERY

Outputs the query plan for the specified index query.


## index

    index [INDEX] PATH

Index the document at the given path.  See the 
[[indexing section|Search Indexing Reference#Indexing-from-the-Command-Line]] for 
more details.


## delete

    delete [INDEX] PATH

Used to delete a document from the index.  See the 
[[indexing section|Search Indexing Reference#Deleting-from-the-Command-Line]] 
for more details.


## solr

    solr [INDEX] PATH

Index solr documents.  See the 
[[indexing section|Search Indexing Reference#Indexing-using-the-Solr-Interface]]
for more details.


## install

    install BUCKET

Install the Search precommit hook on the given bucket.  This allows one to
[[index incoming objects|Search Indexing Reference]].


## uninstall

    uninstall BUCKET

Uninstall the Search precommit hook on the given bucket.


## test

    test PATH

Run the Search test script at the given path.
