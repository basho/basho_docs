---
title: Comparing MapReduce, RiakSearch, and Secondary Indexes
project: riak
version: 0.10.0+
document: reference
toc: true
audience: intermediate
keywords: [mapreduce, search, indexes, comparison]
---

<table>
    <tr>
        <th>&nbsp;</th>
        <th>MapReduce</th>
        <th>Riak Search</th>
        <th>Secondary Indexes</th>
    </tr>
    <tr>
        <td><em>Query Types</em></td>
        <td>Ad-hoc queries composed of an arbitrary number of Map phases and
            Reduce phases</td>
        <td>SOLR style queries supporting combinations of free-text, wildcards, 
            proximity, and boolean operators</td>
        <td>Equality and range query support</td>
    </tr>
    <tr>
        <td><em>Index Locality</em></td>
        <td>N/A</td>
        <td>Indexes for terms are replicated to N vnodes (i.e. term-based 
            partitioning) and stored in a merge index backend, regardless of the 
            Riak KV backend used</td>
        <td>Indexes are located on the same vnodes as the object (i.e. 
            document-based partitioning) and stored in the LevelDB backend along 
            with the document</td>
    </tr>
    <tr>
        <td><em>Vnodes Queried</em></td>
        <td>Depends on input</td>
        <td>1 per term queried; 1/N for trailing wildcard</td>
        <td>1/N of all KV vnodes per a request</td>
    </tr>
    <tr>
        <td><em>Supported Data Types</em></td>
        <td>Any datatype with Erlang MapReduce functions; valid UTF8 JSON with
            Javascript functions. [[Links]] per the specification</td>
        <td>Integer, Date, and Text</td>
        <td>Binary and Integer</td>
    </tr>
    <tr>
        <td><em>Extraction</em></td>
        <td>Map phases can be used to extract data for later Map and Reduce
            phases</td>
        <td>Tokenization performed by one of the provided analyzers (Whitespace, 
            Standard, Integer, and No-Op) or a Custom Analyzer</td>
        <td>Indexed values are submitted as metadata on the object, thus the 
            application is responsible for tokenization</td>
    </tr>
    <tr>
        <td><em>Anti-Entropy / Fault Tolerance</em></td>
        <td>N/A</td>
        <td>No anti-entropy features. If a search partition is lost, the entire
            search index needs to be rebuilt</td>
        <td>Anti-entropy is carried over from KV; if a partition is lost,
            secondary indexes will be rebuilt along side the KV data by read 
            repair</td>
    </tr>
    <tr>
        <td><em>Limitations</em></td>
        <td>MapReduce operations are performed in memory and must be completed 
            within the timeout period</td>
        <td>Querying low cardinality terms could tax a small subset of vnodes; 
            composite queries are expensive; documents must be structured (JSON 
            or XML) or plain text</td>
        <td>Only available with the LevelDB backend; no composite queries</td>
    </tr>
    <tr>
        <td><em>Suggested Use Cases</em></td>
        <td>Performing calculations based on a known set of bucket-key pairs</td>
        <td>Searching objects with full-text data</td>
        <td>Retrieving all objects tagged with a particular term</td>
    </tr>
    <tr>
        <td><em>Poor Use Cases</em></td>
        <td>Performing complex operations on large numbers of objects (e.g. analyzing every object in a bucket)</td>
        <td>Searching for common (low cardinality) terms in documents</td>
        <td>Searching prosaic text</td>
    </tr>
</table>
