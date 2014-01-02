---
title: Documentation Style Guide
project: riak
version: 1.4.1+
document: appendix
toc: true
index: true
keywords: [community]
---

The adage that no documentation is better than poor quality documentation should always be borne in mind when we write documentation. We should strive to provide accurate and current information in a clear and consistent style that is appropriate for the target audience.

For the sake of delivering clear and consistent documentation, this document provides a common set of writing guidelines, complete with document structures, styles, and conventions that we try to follow.

## Essential Style Concepts

### Audience

Before you begin authoring a document or response, be sure to *accurately identify your intended audience*. This should always be the first thing you
do before even outlining or typing a single word.

Once you've identified your audience, try to keep their needs in mind when
writing. Our audience fortunately consists mainly of highly technical people, but you should still avoid excessive use of jargon and opt to keep language clear and simple wherever possible.

### Voice

We prefer to prove that we're human by using the active voice in our documentation and customer responses. The collective pronouns *we* or *us* are preferred over *I* in most cases, except for when writing in informal communication styles, such as in blog posts.

### Style for Purpose

It should always be asked of a document: is this document primarily descriptive, prescriptive, or explanatory in nature? Our documentation is generally split into two major document types: *guides* and *references*. Once you've identified your audience, the next step is to choose the type of document that you will write from these two primary forms.

#### Guides

A **guide** helps the user through a particular process often by using detailed and stepwise instructions presented in a narrative style.

When writing guides, it is helpful to use ordered lists when presenting a series of steps, examples of the exact commands required to complete those steps, and the expected output from each command. 

#### References

In contraste to a guide, a **reference** is more of a collection of raw knowledge that strives for correctness and completeness. In references, the  focus is on compact explanation of specific features, API methods, and similarly fine-grained domains of knowledge. It is important to include all available information in a reference.

Guides and references are complementary documents and should always exist in separate form. Avoid the temptation to mix the two styles in one document.

### Consistency

Consistency is a key element in authoring technical content. Using the same term for something every single time is very important if you seek to avoid confusing the reader. While a concept may be well grounded in your mind due to frequent use, communicating to a person unfamiliar with the concept is less effective when you use terminology inconsistently.

Even minor inconsistencies, such as the addition of an extra word to a term or an unconventional abbreviation, can confuse the reader. This confusion will ultimately impair their ability to effectively understand your writing and, by extension, our technologies.

## Document Design & Structure

The following pointers will help you structure a document and highlight
important content in a consistent way.

### Chunks and Scanning

Well-written documentation is organized and presented in way that plays to our tendency to rapidly scan content and find the information we need.

To that end, write in small, easily scanned chunks of related information, use common, everyday vocabulary suited for the target audience, and avoid dense paragraphs stuffed with technical terminology whenever possible.
 
### Headings, Lists, & Emphasis

Proper organization and flow in a document helps the reader more quickly locate the information they need. The following section describes document organization methods which facilitate scanning and findability while maintaining logical content ordering.

In general, it's a good idea to use ordered---i.e. numbered---lists to denote stepwise operations and unordered lists for grouping related items, and to introduce both types of list with a complete sentence that ends in a colon (`:`).

This is a sample unordered list:

* Riak
* Riak Enterprise
* Riak CS

#### Headings

Using descriptive headings helps to summarize detailed content within a section of documentation. This also helps the reader scan a document and quickly locate the section of content most appropriate for their needs.

Try to describe document sections with well-worded headings, and keep heading
levels to a minimum to reduce document structure complexity.

When writing headings, use an uppercase initial letter for all nouns, verbs, and adjectives. Use an uppercase initial letter for all conjunctions and
prepositions longer than 4 characters. Use all lowercase letters for conjunctions and prepositions shorter than 4 characters.

When in doubt, there's a web tool that helps with correct [title capitalization](http://titlecapitalization.com/) in headers.

##### Performance and Tuning

### Formatting Conventions

The following describes text formatting conventions useful for all customer-facing product documentation and issue responses.

#### Italics

Use text set in *italics* to introduce new technology terminology, provided that it is followed by a corresponding explanation. It's also good form to italicize non-English words, *exempli gratia*.

#### Bold

Use **bold** text when describing the targets of user interface options or
locations in an interface or system, such as "click **Save**".

#### Fixed Width (Monospace)

Use `fixed-width` text by wrapping items in single backticks (\\`) to indicate
command names, code, filenames, or text strings that a user is expected to actually type in.

Typically, bare fixed-width text will indicate the actual commands and arguments needed. When the placeholder arguments are used in an example and the placeholder arguments should be replaced with actual data, a note should
be made about doing so.

Example:

After modifying `vm.args`, restart the node with `riak restart`.

### Preformatted Text Blocks

Use three backticks (\\`) with an optional language designator to indicate blocks of code, configuration data, or other preformatted text.

Trim line lengths to less than 80 characters, and don't include actual prompts for command line examples, as doing so can be troublesome for readers who copy and paste command line examples.

Erlang code example:

```erlang
length([P || {riak_kv_vnode, P, _} <- riak_core_vnode_manager:all_vnodes()]).
```

Shell commands example:

```bash
riak-admin member-status
riak-admin transfers
```

### Callouts & Notes

Inline notes are formatted in Markdown by wrapping text in single-asterisk
(`*`) characters.

Inline note example:

*Note that when attempting to identify the database...*

For blockstyle callouts in documentation, use an HTML `<div>` element with either *info* or *note* classes. When using the *info* class, an icon is included in the styling of the block for additional visual impact. You can optionally specify a title for the block style note with a second `<div>` element inside the note or info `<div>` with a class of *title*.
  
Block note with title example:

```html
<div class="info"><div class="title">Note on Magic Switch</div>There is a third party switch located on the cabinet. Please do not change the switch setting from **more magic** to **magic** while the machine is in production, as a crash could occur.</div>
```

*Note that Markdown syntax can also be intermixed within the HTML `<div>`
elements as well.*
  
### Grammar, Punctuation & Spelling

Consistent grammar, punctuation, and spelling usage within our documentation
strengthens reader confidence and helps to avoid confusion.

#### Abbreviations

Abbreviation is the shortening of a word form. In writing about time, for example, it is common to abbreviate *ante meridiem* as "am" and *post meridiem* as "pm".

When using abbreviation, avoid creating new forms of abbreviation, as this can cause confusion. It is also unnecessary to explain the meaning of familiar abbreviations (as in the above example).

#### Acronyms

An acronym is the representation of a multi-word term and is usually formed by taking the first letter of each word from the term, capitalizing it, and combining it with the other letters. The acronym for *application programming interface*, for example, is "API".

When you first use an acronym in a document, you should write out the full term and enclose the acronym in parentheses immediately after the term.

Avoid creating new acronyms in an ad-lib manner, as they can cause confusion.

Example:

This guide explains tuning Riak for operating clusters within the Amazon Web Services (AWS) infrastructure.

#### Apostrophe

Use apostrophes primarily for making contractions. In the case of internationalization of documentation, contractions should be avoided.
Pluralization of acronyms is *not* a correct use of apostrophes.

Example:

Don't forget that you cannot easily change the `ring_creation_size` after it has been initially specified.

#### Articles

Avoid using the definite article "the" to begin any of the following:

* Headings
* Document titles
* Section or chapter titles
* Callouts or notes
* Figure or table captions

#### Captions

Captions do not require a period at the end and use the same general rules as headings.

#### Comma

The comma (`,`) should be used in series fashion when using lists of three or more items and after introductory clauses. As a rule, we use the Oxford comma for the sake of clarity.

Example:

The memory, disk, and network utilization are graphed using Graphite.

#### Commands

Avoid using commands as verbs.

#### Colon

Use a colon to introduce sequences or steps in a process or when a command line example is to be shown. Colons should not, however, be used in headings.

Example:

Follow these steps on each node to modify the configuration:

1. Stop the node.
2. Update the `app.config` file.
3. Verify the configuration with `riak chkconfig`.
4. If the configuration is valid, start the node with `riak start`.

#### Hyphen

Hyphens are common in technical documentation, but they are also tricky to use properly. Fundamentally, a hyphen should join two words to form a single concept, but there are other uses for hyphens as well, such as denoting fractional measurements.

Examples:

* two-phase commit
* decision-making tree
* time-sensitive data

When in doubt about correct hyphen usage, consult a [[reference|Documentation Style Guide#References]].

#### Key Names

All keyboard key names should be capitalized and bolded.

Example:

Press **Control**+**Alt**+**Delete** to reboot.

#### Numbers and Numerals

Since this is technical documentation, the normal rules of spelling small numbers don't apply all that much. Most of our usage of numbers are for precision. We'd rather see the sentence:

"You should aim for 8 vnodes per node."

As opposed to the wordy variant:

"You should aim for eight vnodes per node."

Try and avoid starting sentences with numbers.

Examples:

* An 8-core processor
* Roughly 7 million operations per second
* The output consisted of four 25MB log files

#### Parentheses

Use parentheses to encapsulate acronyms on first use and for alternative terms, but do not use them for complete asides or additional explanatory text. Parentheses are also useful for referencing a specific section of a system manual page.

Examples:

* Read about tuning the ZFS Second-Level Adaptive Replacement Cache (L2ARC).
* See the sysctl(8) manual page for more details.

#### Period

The period (`.`) is used to end complete sentences, at the end of each step in a stepwise procedure, and in certain circumstances like acronyms and abbreviations.

Example:

Complete steps 4 through 9 for each node that is a cluster member.

#### Proper Names

Proper names, such as *Java*, *Python*, *Ruby*, and *Justin* should always be capitalized, with the exception of referring to commands of the same name, as in the case of `java`, `python`, and `ruby`.

Example:

Justin typed `python` and pressed **Enter** to start the Python interactive
interpreter.

#### Quotations

Use quotations for emphasizing text from other sources when used verbatim. We prefer quotations in web documents to underlining as an alternate emphasis method beyond bold or italics.

Example:

"Yes, it can silently fail if you don't check the returned error code."

#### Blockquotes

For multiple sentence quotes, consider a blockquote, which is a line started with a less than (>) character.

Example:

> To achieve this level of availability, Dynamo sacrifices consistency under certain failure scenarios. It makes extensive use of object versioning and application-assisted conflict resolution in a manner that provides a novel interface for developers to use.

#### Semicolons

Some people like semicolons, but they can be tricky to use. We recommend avoiding them; unless you really paid attention in English class.

#### Dashes

Unlike semicolons, dashes are hard to use incorrectly. But their overuse can make copy harder to read---use sparingly to separate ideas. You can create a proper [em dash](http://en.wikipedia.org/wiki/Dash#Em_dash) in markdown by typing three [en dashes](http://en.wikipedia.org/wiki/Dash#En_dash) (`---`).

## Terminology

A number of technical terms related to distributed computing environments and specific to Riak are common to our product documentation. Consistent use of these terms is critical to composing accurate documentation that inspires confidence.


### Basho Product Specific Terminology

#### Erlang

*Erlang* is the primary programming language used to develop Riak and Riak
related products. It should always be capitalized, as there is no `erlang`
command.

#### Fullsync, realtime 

When describing Riak Enterprise replication, eschew hyphens in *fullsync* and *realtime*.

#### MapReduce

*MapReduce* is written as "MapReduce;" avoid the use of incorrect versions of the term, such as "Map/Reduce", "M/R", or any lowercase-only form (e.g., "mapreduce").

#### Multi-Datacenter Replication

One of the features of Riak Enterprise is Multi-Datacenter Replication. It should be capitalized in both headings and sentences.

#### Protocol Buffers

When referring to the Google *Protocol Buffers* data serialization mechanism, refer to it as "Protocol Buffers" or "protocol buffers." Avoid using project
name abbreviations like "protobuffs."

When writing about the Riak Protocol Buffers Client or API, use of the abbreviation "PBC" or "PBC API" is acceptable, provided that the full term is
introduced at an earlier point in the document or response.

#### Riak

*Riak* is a product name and should always be spelled with a capital 'R'.

On the other hand, `riak` is a command name, is case-sensitive, and should always be spelled in all lowercase letters and enclosed in backticks when using Markdown.

#### Riak Enterprise

*Riak Enterprise* is a product name. Avoid references to "Riak EE," "Riak EDS," or just "Enterprise".

### Localized Date Conventions

Should use the date convention for the document audience and avoid abbreviations in date components, such as month names. Time should be expressed in 24-hour format with a note about the specific time zone in use. If in doubt, use ISO 8601: `YYYY-MM-DD`.

## References

1. [A Short Guide to Writing Guides](https://gist.github.com/coderoshi/3729593)
2. [American English grammar rules from the Chicago Manual of Style](http://www.chicagomanualofstyle.org/home.html)
3. [Hyphen Use from the Purdue Online Writing Lab](http://owl.english.purdue.edu/owl/resource/576/01/)
