---
title: Advanced Search Schema
project: riak
version: 0.14.0+
document: cookbook
toc: true
audience: intermediate
keywords: [search, schema]
moved: {
  '1.4.0-': '/cookbooks/Riak-Search---Schema'
}
---

Riak Search was designed to work seamlessly with Riak. As a result, it retains many of the same properties as Riak, including a schema-free design. In other words, you can start adding data to a new index without having to explicitly define the index fields.

That said, Riak Search does provide the ability to define a custom schema. This allows you to specify required fields and custom analyzer factories, among other things.

## The Default Schema

The default schema treats all fields as strings, unless you suffix your field name as follows:

* *FIELDNAME_num* - Numeric field. Uses Integer analyzer. Values are padded to 10 characters.
* *FIELDNAME_int* - Numeric field. Uses Integer analyzer. Values are padded to 10 characters.
* *FIELDNAME_dt* - Date field. Uses No-Op analyzer.
* *FIELDNAME_date* - Date field. Uses No-Op analyzer.
* *FIELDNAME_txt* - Full text field. Uses Standard Analyzer.
* *FIELDNAME_text* - Full text field. Uses Standard Analyzer.
* All other fields use the Whitespace analyzer.

The default field is named *value*.

## Defining a Schema

The schema definition for an index is stored in the Riak bucket `_rs_schema`, with a key of the same name as the index. For example, the schema for the "books" index is stored under `_rs_schema/books`. Writing to the `_rs_schema` bucket is highly discouraged.

Alternatively, you can set or retrieve the schema for an index
using command line tools:


```bash
# Set an index schema.
bin/search-cmd set-schema Index SchemaFile

# View the schema for an Index.
bin/search-cmd show-schema Index
```

Note that changes to the Schema File *will not* affect previously indexed data. It is recommended that if you change field definitions, especially settings such as "type" or "analyzer_factory", that you re-index your documents by listing the appropriate keys, reading and rewriting that document to Riak.

Below is an example schema file. The schema is formatted as an Erlang term. Spacing does not matter, but it is important to match opening and closing brackets and braces, to include commas between all list items, and to include the final period after the last brace:


```erlang
{
    schema,
    [
        {version, "1.1"},
        {default_field, "title"},
        {default_op, "or"},
        {n_val, 3},
        {analyzer_factory, {erlang, text_analyzers, whitespace_analyzer_factory}}
    ],
    [
        %% Don't parse the field, treat it as a single token.
        {field, [
            {name, "id"},
            {analyzer_factory, {erlang, text_analyzers, noop_analyzer_factory}}
        ]},

        %% Parse the field in preparation for full-text searching.
        {field, [
            {name, "title"},
            {required, true},
            {analyzer_factory, {erlang, text_analyzers, standard_analyzer_factory}}
        ]},

        %% Treat the field as a date, which currently uses noop_analyzer_factory.
        {field, [
            {name, "published"},
            {type, date}
        ]},

        %% Treat the field as an integer. Pad it with zeros to 10 places.
        {field, [
            {name, "count"},
            {type, integer},
            {padding_size, 10}
        ]},

        %% Alias a field
        {field, [
            {name, "name"},
            {analyzer_factory, {erlang, text_analyzers, standard_analyzer_factory}},
            {alias, "LastName"},
            {alias, "FirstName"}
        ]},

        %% A dynamic field. Anything ending in "_text" will use the standard_analyzer_factory.
        {dynamic_field, [
            {name, "*_text"},
            {analyzer_factory, {erlang, text_analyzers, standard_analyzer_factory}}
        ]},

        %% A dynamic field. Catches any remaining fields in the
        %% document, and uses the analyzer_factory setting defined
        %% above for the schema.
        {dynamic_field, [
            {name, "*"}
        ]}
    ]
}.
```


## Schema-level Properties

The following properties are defined at a schema level:

* *version* - Required. A version number, currently unused.
* *default_field* - Required. Specify the default field used for searching.
* *default_op* - Optional. Set to "and" or "or" to define the default boolean. Defaults to "or".
* *n_val* - Optional. Set the number of replicas of search data. Defaults to 3.
* *analyzer_factory* - Optional. Defaults to "com.basho.search.analysis.DefaultAnalyzerFactory"

## Fields and Field-Level Properties

Fields can either by static or dynamic. A static field is denoted with `field` at the start of the field definition, whereas a dynamic field is denoted with `dynamic_field` at the start of the field definition.

The difference is that a static field will perform an exact string match on a field name, and a dynamic field will perform a wildcard match on the string name. The wildcard can appear anywhere within the field, but it usually occurs at the beginning or end. (The default schema, described above, uses dynamic fields, allowing you to use fieldname suffixes to create fields of different data types.)


<div class="info">Field matching occurs in the order of appearance in the schema definition. This allows you to create a number of static fields followed by a dynamic field as a "catch all" to match the rest.</div>


The following properties are defined at a field level, and apply to both static and dynamic fields:

* *name* - Required. The name of the field. Dynamic fields can use wildcards. Note that the unique field identifying a document *must* be named "id".
* *required* - Optional. Boolean flag indicating whether this field is required in an incoming search document. If missing, then the document will fail validation. Defaults to false.
* *type* - Optional. The type of field, either "string" or "integer". If "integer" is specified, and no field-level analyzer_factory is defined, then the field will use the Whitespace analyzer. Defaults to "string".
* *analyzer_factory* - Optional. Specify the analyzer factory to use when parsing the field. If not specified, defaults to the analyzer factory for the schema. (Unless the field is an integer type. See above.)
* *skip* - Optional. When "true", the field is stored, but not indexed. Defaults to "false".
* *alias* - Optional. An alias that should be mapped to the current field definition, effectively indexing multiple fields of different names into the same field. You can add as many `alias` settings as you like.
* *padding_size* - Optional. Values are padded up to this size. Defaults to 0 for string types, 10 for integer types.
* *inline* - Optional. Valid values are "true", "false", and "only" (default is "false"). When "only", the field will not be searchable by itself but can be used as a "filter" for searches on other fields. This will enhance the performance of some queries (such as ranges in some cases) but will consume more storage space because the field value is stored "inline" with the indexes for other fields.  When "true", the field will be stored normally in addition to inline. Filtering on inline fields is currently only supported via the [[Solr|Using Search#Query-Interfaces]] interface.

<div class="info"><div class="title">A Note on Aliases</div>

1. You should never attempt to give an alias the same name as a field name. Attempting to do so will cause a field value to be indexed under an undetermined name.
2. Multiple aliases will be concatenated with a space. If Name has two aliases, the value {LastName:"Smith", FirstName:"Dave"} would store as "Smith Dave".
</div>

## Analyzers

Riak Search ships with a number of different analyzer factories:

## Whitespace Analyzer Factory

The Whitespace Analyzer Factory tokenizes a field by splitting the text according to whitespace, including spaces, tabs, newlines, carriage returns, etc.

For example, the text "It's well-known fact that a picture is worth 1000 words." is split into the following tokens: ["It's", "a", "well-known", "fact", "that", "a", "picture", "is", "worth", "1000", "words."]. Notice that capitalization and punctuation is preserved.

To use the whitespace analyzer, set the *analyzer_factory* setting as seen below:

```erlang
{analyzer_factory, {erlang, text_analyzers, whitespace_analyzer_factory}}}
```


## Standard Analyzer Factory

The Standard Analyzer Factory mimics the Java/Lucene Standard Tokenizer. The Standard Analyzer is useful for full-text searches across documents written in English. It tokenizes a field according to the following rules:

1. Split on all punctuation except for periods followed by a character.
2. Lowercase all tokens.
3. Strip out any tokens smaller than 3 characters as well as stopwords (common English words).

The stopwords are defined as: "an", "as", "at", "be", "by", "if", "in", "is", "it", "no", "of", "on", "or", "to", "and", "are", "but", "for", "not", "the", "was", "into", "such", "that", "then", "they", "this", "will""their", "there", "these".

The text "It's well-known fact that a picture is worth 1000 words." will result in the following tokens: ["well", "known", "fact", "picture", "worth", "1000", "words"].

To use the standard analyzer, set the *analyzer_factory* setting as seen below:

```erlang
{analyzer_factory, {erlang, text_analyzers, standard_analyzer_factory}}}
```


## Integer Analyzer Factory

The Integer Analyzer Factory tokenizes a field by finding any integers within the field. An integer is defined as as string of numbers without any punctuation between them, possibly starting with a '-' to indicate a negative number.

For example, the text "It's well-known fact that a picture is worth 1000 words." will result in only one token, "1000".

To use the integer analyzer, set the *analyzer_factory* setting as seen below:

```erlang
{analyzer_factory, {erlang, text_analyzers, integer_analyzer_factory}}}
```

## No-Op Analyzer Factory

The No-Op Analyzer Factory doesn't tokenize a field, it simply returns back the full value of the field. For this reason, it is useful for identity fields.

For example, the text "WPRS10-11#B" will tokenize unchanged into "WPRS10-11#B".

To use the no-op analyzer, set the *analyzer_factory* setting as seen below:

```erlang
{analyzer_factory, {erlang, text_analyzers, noop_analyzer_factory}}}
```

## Custom Analyzers

You can create your own custom analyzers in Erlang.

Some tips:

* Model your custom analyzer after an existing analyzer.  See [[https://github.com/basho/riak_search/blob/master/src/text_analyzers.erl]] for sample code.

* The analyzer should take a string and configuration parameters and return a list of tokens. The order of tokens is important for proximity searching.

* Make sure to put your compiled analyzer on the code path.
