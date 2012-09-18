# Riak Docs 2.0

*It's been a long time a-comin', baby...*

## Develoment Mode

Easy:

```
gem install bundler
bundle install
RIAK_VERSION=1.2.0 bundle exec middleman server
```

## Writing Docs

### Structure

All documents and resources (like images, css/scss, javascript/coffeescript) live under the `source` directory.

Since part of the goal of this rewrite is to be more deliberate about the type of documents we produce, docs are seperated into directories according to their types. A document should either be a fully realized "reference", or a "guide" (thumb through [A Short Guide to Writing Guides](https://github.com/basho/internal_wiki/wiki/A-Short-Guide-to-Writing-Guides) for more on this idea). Further distinction can then be placed under subdirectories. `references` contains `apis`, `appendices`; `guides` contains `tutorials`, `cookbooks`.

### Wiki Links

Besides the standard link styles relative to the given document type (markdown, slim, org, etc), you can use wiki-style links, eg: `[[The Riak Fast Track]]` or `[[The Fast Track|The Riak Fast Track]]`. Note that this if you have a name collision, it will just link to a random one.

### Document Metadata

At the top of every document is a metadata block. This allows us to append any information we want to a document, and alter the page generation accordingly. Here is an example:

```
---
title: Loading Data and Running MapReduce
project: riak
version: 0.10.0+
document: tutorial
toc: true
index: false
audience: beginner
keywords: [tutorial, fast-track]
prev: ["Basic HTTP Operations", "Basic-Riak-API-Operations.html"]
up:   ["The Riak Fast Track", "index.html"]
next: ["Links and Link Walking", "Links-and-Link-Walking.html"]
---
```

The title will dictate the page name, rather than the old method of using the file name. This allows us more flexibility in our urls and specify names for wiki-links (which will first use title, before dropping back to the file name). The title will appear at the top of the document.

The `project` assocates this file with a particular project, most of that time that is riak. It could also be `riakcs`, or `riakee`.

The `version` is a range for which this document is true. This allows the system to trim out any unnecessary documents if we render earlier or later versions (eg. if we render documents for version 1.3.0, but a document is no longer valid, it won't exist for that version). The ranges are specified using either greater/less than signs, plus/minus, or a version range.

* **{{1.0.0+}}** _(greater than 1.0.0, inclusive)_
* **{{>=1.0.0}}** _(greater than 1.0.0, inclusive)_
* **{{<1.0.0}}** _(less than 1.0.0, exclusive)_
* **{{1.0.0-1.2.0}}** _(between 1.0 and 1.2, inclusive)_


The `document` labels what kind of document this is. So far I've been using: `tutorial`, `cookbook`, `reference`, `api`, `appendix`. These allow alternative look/feel for different kinds of pages.

Set `toc` to false if you do not want a table-of-contents generated for this page. Otherwise, a list of links will be generated for every `h2` tag on the main article.

The `index` flag is just a marker that this page is largely an index page for navigation, and not really a content page. It's useful for downgrading it's importance in code generation (see the HTTP/PBC API page).

The `audience` value is either `beginner`, `intermediate`, or `advanced`. We're not doing much with this yet, but it's a good note and reminder on who this document is meant for, for future updates.

`keywords` is an array of words associated with this page. There can be any number of them. Then, each keyword links to a page that is an index of all other pages with that matching keyword. Eg. Commit-Hooks and Eventual-Consistency pages both have the keyword "concepts", so they both get generated with a link to a page /keywords/concepts, that just lists out and links to these two pages along with others.

`prev`, `next`, and `up` are indended for multi-page tutorial navigation. They corrospond to the previous page, the next page, and moving up to the index (generally, the start of the tutorial). They accept an array with two values, the first is the link text, the second is a relative link.

### Versioning

A big change in this rewrite was how we handle document versions. Each version of the documents will be generated and deployed seperately into it's own directory structure. This provides a few advantages:

* Clarify to users exactly what version a given document is valid for, without making little footnotes in the pages
* Add/remove documents without breaking SEO
* Altering the site navigation/layout/style won't break older versions

The first thing to note is how the projects are structured on the deployment server. When a version is deployed, all projects will be placed under their project name followed by their version.

For example, API pages for Riak version 1.2.0 will be under this directory: `/riak/1.2.0/references/apis/`, yet for version 1.1.0 `/riak/1.1.0/references/apis/`.

Besides versioning entire files, you can also version segments of a file by enclosing a version in brackets, starting with a hash and ending with a slash `{{#version}}...{{/version}}`.

For example:

```
{{#1.2.0+}}PBC includes an interface for Riak search.{{/1.2.0+}}
```

This sentence will only be rendered for versions greater than or equal to 1.2.0.

Since you cannot wrap an individual list item in markdown et al, placing a version on the same line as a list item will remove that whole item

```
* PBC Index {{1.2.0+}}
* PBC MapReduce
* PBC Search {{1.2.0+}}
```

Will render as this for version 1.1.0:

* PBC MapReduce

Finally, there is a file `data/version.yml` that give a list of all project versions. This is used to generate a link bar where readers can click to view older versions of a document.

### Navigation

There is a file named `ROOT/data/global_nav.yml`. Change this file to alter the global navigation menu.

Beyond the global navigation, there is also a secondary navigation of pages that are related to the current page. So if you are reading an FAQ, other FAQs will be listed as well. It currently populates based on matching keywords, however, it could be improved to be based on social or traffic metrics.

Tutorials optionally have multi-page navigation. They can be specified by the `prev`, `next`, and `up` meta data values.

There is the time-based version navigation mentioned previously (this may be rewritten to be generated on the client-side by javascript).

Each page can optionally generate an inline table-of-contents (see the `toc` setting above). A list of links will be generated for every `h2` tag on the main article.

Finally, there is the keyword-based navigation. Every page with a keyword contains a link to an index, which lists other links to pages sharing that keyword.

# Deploying

## Testing deploy mode

To try out the thin server in the way production functions, first build the static files:

```
bundle exec middleman build
```

Then you can run the thin server locally:

```
bundle exec thin start -p 3000
```

To have MiddleMan auto-build on each save, we use the Watchr gem. It will auto-build on each save in the source directory:

```
bundle exec watchr ./Watchrfile
```

## Deploying to S3

Before deployment you must specify an env var with Basho's S3 access/secret key. In bash, something like this in your ~/.bash_login file should do it.

```bash
export RIAK_DOCS_ACCESS_KEY="XXXXX"
export RIAK_DOCS_SECRET_KEY="XXXXX"
```

**Keep it secret. Keep it safe.**

Then to deploy, simply run middleman build with a riak version:

```
RIAK_VERSION=1.2.0 middleman build
```

## Deploying the Search Index

This is still a work in progress, but adding `INDEX=true` will deploy the docs to yokozuna.

```
RIAK_VERSION=1.2.0 INDEX=true middleman build
```
