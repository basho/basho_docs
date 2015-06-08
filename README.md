# Riak Docs

This repo holds all the content (and other bits, e.g. CSS, JavaScript,
etc.) for the most-excellent [Riak Docs](http://docs.basho.com), home of
the documentation for Riak and Riak CS. This document tells you how to
contribute to the docs (please do!) as well as to deploy them if you
have our secret key.

## Prerequisites

We use [Middleman](http://middlemanapp.com/), a Ruby-powered static site
generator, to generate the [Riak Docs](http://docs.basho.com).

Another tool used with this repository is the Ruby Version Manager
(RVM). Get it [here](https://rvm.io/).

### Mac OS X

As with any Ruby project, Xcode & the Xcode CLI Tools are needed to
install and run everything smoothly on OS X.

You can install Xcode for free via the App Store. Once installed, run
`xcode-select --install` to get the Xcode CLI Tools installed as well.

If you don't want Xcode, you can also download the [OS X GCC
Installer](https://github.com/kennethreitz/osx-gcc-installer), which
gives you the essential compilers needed to build the docs.

## How to Contribute

You can treat the `basho_docs` repo much like you would any other code
repo.  You can contribute in two ways:

1. Submit a new [issue](https://github.com/basho/basho_docs/issues)
2. (**bonus points**) Make a change and submit a pull request.

To make a change&mdash;be it as simple as a typo fix or as weighty as
the creation of an entirely new page full of Python client code
snippets&mdash;follow these instructions:

1. Clone a copy of the docs to your local machine:

    ```shell
    $ git clone https://github.com/basho/basho_docs.git
    ```

2. Install Middleman

Navigate to the `/basho_docs` directory and execute the following
instructions. This should install Middleman and launch the Middleman
API:

    ```shell
    $ sudo gem install bundler
    $ bundle install
    $ bundle exec middleman
    ```

If you run into errors, [Middleman's install
page](http://middlemanapp.com/basics/getting-started/) is a good place
to start.

Now point a browser at [http://0.0.0.0:4567](http://0.0.0.0:4567). Here
you'll find Middleman serving up static HTML on the fly (generated from
Markdown source files).

3. Create a new branch

	```shell
    $ git checkout -b <new-branch-name>
	```

4. Make changes on your branch

5. Review your changes locally

	```shell
	$ bundle exec middleman
	```

6. Send us a pull request

If it's a small or obvious change, we're likely to merge it right away.
If we have questions, we'll communicate with you using the pull
request's issue page.

## Writing Docs

### Structure

All documents and resources (like images, CSS/SCSS,
JavaScript/CoffeeScript) live under the `source` directory.

Since part of the goal of this rewrite is to be more deliberate about
the type of documents we produce, docs are separated into directories
according to their types. A document should either be a fully realized
"reference" or a "guide" (thumb through [A Short Guide to Writing
Guides](https://gist.github.com/coderoshi/3729593) for more on this
idea). Further distinctions can then be placed under subdirectories:
`references` contains `apis` and `appendices`; `guides` contains
`tutorials` and `cookbooks`.

### Languages

Set the language in which you wish to generate the docs. The default is
`en` (English). To alter your document language, you can set an
environment variable like this:

```shell
export RIAK_DOCS_LANG=jp
```

_All docs live under `source/languages/en` or `source/languages/jp`. In
general, you shouldn't need to modify other directories._

### Wiki Links

Besides the standard link styles relative to the given document type
(markdown, slim, org, etc), you can use wiki-style links, e.g. `[[The
Basics]]` or `[[Start Here|The Basics]]`. Note that if you have a name
collision, it will just link to a random one.

### Document Metadata

At the top of every document is a metadata block. This allows us to
append any information we want to a document and alter the page
generation accordingly.

Here is an example:

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
prev: "[[Basic HTTP Operations]]"
up:   "[[The Riak Fast Track]]"
next: "[[Links and Link Walking]]"
---
```

The title will dictate the page name rather than relying on the old
method of using the filename. This allows us more flexibility in our
URLs and in specifying names for wiki links (which will first use title
before dropping back to the filename). The title will appear at the top
of the document.

The `project` associates this file with a particular project. In most
cases the project will be `riak`, but it could also be `riakcs` (Riak
Cloud Storage) or `riakee` (Riak Enterprise Edition).

The `version` is a range for which this document is true. This allows
the system to trim out any unnecessary documents if we render earlier or
later versions (e.g. if we render documents for version 1.3.0 but a
document is no longer valid, in which case it won't exist for that
version). The ranges are specified using either greater/less than or
plus/minus signs or a version range.

* **{{1.0.0+}}** _(greater than 1.0.0, inclusive)_
* **{{1.0.0-}}** _(less than 1.0.0, exclusive)_
* **{{>=1.0.0}}** _(greater than 1.0.0, inclusive)_
* **{{<1.0.0}}** _(less than 1.0.0, exclusive)_
* **{{1.0.0-1.2.0}}** _(between 1.0 and 1.2, inclusive)_


The `document` labels what kind of document this is. So far I've been
using the following labels: `tutorial`, `cookbook`, `reference`, `api`,
`appendix`. These allow alternative look/feel combinations for different
kinds of pages.

Set `toc` to `false` if you do not want a table of contents generated
for this page. Otherwise, a list of links will be generated for every
`h2` tag on the main article (`##` in Markdown).

The `index` flag is just a marker that this page is largely an index
page for navigation and not really a content page. It's useful for
downgrading its importance in code generation (see the HTTP/PBC API
page).

The `audience` value is either `beginner`, `intermediate`, or
`advanced`. We're not doing much with this yet, but it's a good note and
reminder on the target audience of the doc for the sake of future
updates.

`keywords` is an array of words associated with this page. There can be
any number of them. Each keyword links to a page that is an index of all
other pages with that matching keyword. For example, Commit-Hooks and
Eventual-Consistency pages both have the keyword `concepts`, so they
both are generated with a link to a page `/keywords/concepts` that
simply lists out and links to these two pages along with others.

`prev`, `next`, and `up` are intended for multi-page tutorial
navigation. They correspond to the previous page, the next page, and
moving up to the index (generally, the start of the tutorial). They
accept an array with two values: the first is the link text while the
second is a relative link.

### Versioning

A big change in this rewrite was how we handle document versions. Each
version of the documents will be generated and deployed separately into
its own directory structure. This provides a few advantages:

* Clarifies to users exactly what version a given document is valid for,
  without making footnotes or other indicators on specific pages
* Allows for adding and removing documents without breaking SEO
* Alters the site navigation/layout/style in a way that won't break
  older versions

The first thing to note is how the projects are structured on the
deployment server. When a version is deployed, all projects will be
placed under their project name followed by their version.

For example, API pages for Riak version 1.2.0 will be under this
directory: `/riak/1.2.0/references/apis/`. But for version 1.1.0:
`/riak/1.1.0/references/apis/`.

Besides versioning entire files, you can also version segments of a file
by enclosing a version in brackets, starting with a hash and ending with
a slash `{{#version}}...{{/version}}`.

For example:

```
{{#1.2.0+}}PBC includes an interface for Riak search.{{/1.2.0+}}
```

This sentence will only be rendered for versions greater than or equal
to `1.2.0`.

Since you cannot wrap an individual list item in Markdown and in other
doc generation systems, placing a version on the same line as a list
item will remove that whole item. And so this

```
* PBC Secondary Indexes {{1.2.0+}}
* PBC MapReduce
* PBC Search {{1.2.0+}}
```

will render as this for version 1.1.0:

```
* PBC MapReduce
```

Finally, there is a `data/version.yml` file that provides a list of all
project versions. This is used to generate a link bar where readers can
click to view older versions of a document.

### Navigation

There is also a file named `ROOT/data/global_nav.yml`. Change this file
to alter the global navigation menu.

Beyond the global navigation, there is also a secondary navigation of
pages that are related to the current page. So if you are reading an FAQ
other FAQs will be listed as well. It currently populates based on
matching keywords, but it could be improved to be based on social or
traffic metrics.

Tutorials optionally have multi-page navigation. They can be specified
by the `prev`, `next`, and `up` metadata values.

There is also the time-based version navigation mentioned previously
(this may later be rewritten to be generated on the client-side by
JavaScript).

Each page can optionally generate an inline table of contents (see the
`toc` setting above). A list of links will be generated for every `h2`
tag on the main article.

Finally, there is keyword-based navigation. Every page with a keyword
contains a link to an index which lists other links to pages sharing
that keyword.

### Style Guide

#### Notes and Asides

Inline notes and conversational asides should be denoted by italics (in
Markdown, single asterisk): `*Note: Keep also in mind ... etc*`.

Paragraph-level block notes should use a div with the class `note:

```html
<div class="note">my note</div>
```

# Deploying

## Testing deploy mode

_Note that the layout will look ugly. At the moment shared files like
`.css` or `.js` files won't show up here because of the way deployment
pushes to production._

To try out the thin server in the way production functions, first build
the static files:

```shell
$ RIAK_VERSION=1.3.1 bundle exec middleman build
```

Then you can run the thin server locally:

```shell
$ thin start -p 4567
```

Or alternatively:

```shell
$ bundle exec thin start -p 4567
```

## New Releases

### Downloads

To update the downloads page for a new release, edit the contents of
`data/versions.yml`. Assuming `X.Y.Z`, releases incrementing to `Z`
append to existing arrays for that version. Releases incrementing `Y` go
on a new line with a single element array containing the version number
as a string:

```yml
riak:
  - ['1.0.0']
  - ['1.1.0', '1.1.4']
  - ['1.2.0', '1.2.1']
  - ['1.3.0', '1.3.1', '1.3.2']
```

Running `bundle exec middleman build` after that should update
`data/downloads_gen.yml`, which drives the downloads page.

### Tagging

You should provide a tag any time a new version is released, for the
sake of future reference and for easing the co-existence of multiple
long-running branches. To tag, say, version 1.2.3:

```bash
git tag -a 1.2.3
```

## Deploying to S3

Before deployment, you must specify an environment variable with Basho's
S3 access/secret key. You also must specify the S3 bucket to which we're
deploying as well as the CloudFront ID (the CDN that we must invalidate
to force a publication to be found).

In Bash, something like this in your `~/.bash_login`, `~/.bashrc`, or
`~/.bash_profile` file should do the trick:

```bash
export AWS_ACCESS_KEY_ID="XXXXX"
export AWS_SECRET_ACCESS_KEY="XXXXX"
export AWS_S3_BUCKET='riakdocs.en'
export AWS_CLOUDFRONT_DIST_ID="E2Q6TQ5O0XT58T"
export RIAK_DOCS_LANG=en
```

**Note**: The bucket and CloudFront values are per language. For the
foreseeable future, you'll typically only need to worry about English.

Language | Bucket      | CloudFront ID  | Language
---------|-------------|----------------|----------
English  | riakdocs.en | E2Q6TQ5O0XT58T | en
Japanese | riakdocs.jp | ENDQVZ5Y7OVJN  | jp
Chinese  | riakdocs.cn | E3NADMYQ20Y7EJ | cn


**Keep it secret. Keep it safe.**

Then to deploy, run the `deploy.rb` scripts with a Riak version:

```bash
./deploy.rb
```

If you need to deploy `riak` and `riakcs` with different versions, add
the CS version at the end. This would be riak 1.3.1 and riakcs 1.1:

```bash
./deploy.rb 1.3.1 1.1.0
```

**Note**: this does more than deploy to S3; it also invalidates the
cache for CloudFront (CF), our CDN. Even if all of the files are
successfully pushed to S3, you won't see the new files on
[docs.basho.com](http://docs.basho.com) until the CF cache is
invalidated.

## Ad-hoc Changes in S3

There are times when it doesn't make a lot of sense to run a full docs
deploy to S3, for example if you need to make a change to a single doc
that you know will be ephemeral. In those cases, you can use a tool like
[s3cmd](http://s3tools.org/s3cmd) to manually fetch files, modify them
locally, and upload them to S3. All files are located in the bucket
located at `s3://riakdocs.en`. The following would fetch an HTML file:

```bash
s3cmd get s3://riakdocs.en/riak/latest/dev/using/basics/index.html
```

Once you've modified the HTML locally, you can use a `PUT` operation to
upload the file:

```bash
s3cmd put index.html s3://riakdocs.en/riak/latest/dev/using/basics/index.html
```

To explore the contents of a given subdirectory, you can use the `ls`
command:

```bash
s3cmd ls s3://riakdocs.en/riak/latest/ops/
```

## Deploying the Search Index

This is still a work in progress, but adding `INDEX=true` will deploy
the docs to [Yokozuna](https://github.com/basho/yokozuna), the Riak/Solr
tool used to power the docs' search engine:

```
RIAK_VERSION=1.4.0 INDEX=true DEPLOY=true bundle exec middleman build
```
