# Riak Docs

This repo holds all the content (and other bits) for the most-excellent [Riak Docs](http://docs.basho.com), home of documentation for Riak, Riak CS, and Riak Enterprise.  This document tells you how to contribute to the docs (please do!), and deploy them, should you have our secret key.

## Prerequisites

On OS-X specifically, as with any Ruby project XCode & the XCode CLI Tools are needed to install, make and install everything smoothly.

![XCode CLI Tooling](http://adronhall.smugmug.com/Software/Software-Development/Basho-Misc/i-xPGs6Tf/1/S/cli-S.jpg)

Another tool used with this repository is rvm. Get it here https://rvm.io/

We use [Middleman](http://middlemanapp.com/), a Ruby-powered static site generator, to generate [Riak Docs](http://docs.basho.com).  

## How to Contribute 

You can treat the basho_docs repo much like you would a code repo.  You can contribute in two ways: 1) submit a new [issue](https://github.com/basho/basho_docs/issues), or 2) (bonus points) make a change and submit a pull request.  To make a change (be it a typo fix or entirely new page full of Python client code snippets) follow these instructions:


1.	Download a copy of the docs to your local machine

	```
	git clone https://github.com/basho/basho_docs.git
	```

2.	Install middleman

	Navigate to the /basho_docs directory and execute the following instructions.  This should install middleman and launch 							the middleman API.  

	```
	gem install bundler
	bundle install
	middleman
	```
	If you run into errors, [middleman's install page](http://middlemanapp.com/getting-started/welcome/) is a good place to 	start.

	Now point a browser at http://localhost:4567.  Here you'll find middleman serving up static HTML on the fly (generated from Markdown source files).  


3. 	Create a new branch

	```
    git checkout -b <branch-name>
	```

4. 	Make changes on your branch

5.	Review your changes locally 

	```
	RIAK_VERSION=1.2.0 middleman
	```

6.	Send us a pull request

If it's a small or obvious change, we're likely to merge it right away.  If we have questions, we'll communicate with you using the pull request's issue page.

## Writing Docs

### Structure

All documents and resources (like images, css/scss, javascript/coffeescript) live under the `source` directory.

Since part of the goal of this rewrite is to be more deliberate about the type of documents we produce, docs are separated into directories according to their types. A document should either be a fully realized "reference", or a "guide" (thumb through [A Short Guide to Writing Guides](https://github.com/basho/internal_wiki/wiki/A-Short-Guide-to-Writing-Guides) for more on this idea). Further distinction can then be placed under subdirectories. `references` contains `apis`, `appendices`; `guides` contains `tutorials`, `cookbooks`.

### Wiki Links

Besides the standard link styles relative to the given document type (markdown, slim, org, etc), you can use wiki-style links, eg: `[[The Riak Fast Track]]` or `[[The Fast Track|The Riak Fast Track]]`. Note that if you have a name collision, it will just link to a random one.

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

The title will dictate the page name, rather than the old method of using the file name. This allows us more flexibility in our urls and specifying names for wiki-links (which will first use title, before dropping back to the file name). The title will appear at the top of the document.

The `project` associates this file with a particular project, most of that time that is riak. It could also be `riakcs`, or `riakee`.

The `version` is a range for which this document is true. This allows the system to trim out any unnecessary documents if we render earlier or later versions (eg. if we render documents for version 1.3.0, but a document is no longer valid, it won't exist for that version). The ranges are specified using either greater/less than signs, plus/minus, or a version range.

* **{{1.0.0+}}** _(greater than 1.0.0, inclusive)_
* **{{>=1.0.0}}** _(greater than 1.0.0, inclusive)_
* **{{<1.0.0}}** _(less than 1.0.0, exclusive)_
* **{{1.0.0-1.2.0}}** _(between 1.0 and 1.2, inclusive)_


The `document` labels what kind of document this is. So far I've been using: `tutorial`, `cookbook`, `reference`, `api`, `appendix`. These allow alternative look/feel for different kinds of pages.

Set `toc` to false if you do not want a table-of-contents generated for this page. Otherwise, a list of links will be generated for every `h2` tag on the main article.

The `index` flag is just a marker that this page is largely an index page for navigation, and not really a content page. It's useful for downgrading its importance in code generation (see the HTTP/PBC API page).

The `audience` value is either `beginner`, `intermediate`, or `advanced`. We're not doing much with this yet, but it's a good note and reminder on who this document is meant for, for future updates.

`keywords` is an array of words associated with this page. There can be any number of them. Then, each keyword links to a page that is an index of all other pages with that matching keyword. Eg. Commit-Hooks and Eventual-Consistency pages both have the keyword "concepts", so they both get generated with a link to a page /keywords/concepts, that just lists out and links to these two pages along with others.

`prev`, `next`, and `up` are intended for multi-page tutorial navigation. They correspond to the previous page, the next page, and moving up to the index (generally, the start of the tutorial). They accept an array with two values, the first is the link text, the second is a relative link.

### Versioning

A big change in this rewrite was how we handle document versions. Each version of the documents will be generated and deployed separately into its own directory structure. This provides a few advantages:

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

### Style Guide

#### Notes and Asides

Inline notes and conversational asides should be denoted by italics (in Markdown, single asterisk): `*Note: Keep also in mind ... etc*`.

Paragraph level block notes should use a div with class ```note```:
```<div class="note">my note</div>```

# Deploying

## Testing deploy mode

_Note that the layout will look ugly. At the moment shared files, like css or js, won't show up here because of the way deployment pushes to production._

To try out the thin server in the way production functions, first build the static files:

```
RIAK_VERSION=1.2.1 middleman build
```

If that doesn't work, try adding bundle exec

```
RIAK_VERSION=1.2.1 bundle exec middleman build
```

Then you can run the thin server locally:

```
thin start -p 4567
```

_(or `bundle exec thin start -p 4567`)_

To have MiddleMan auto-build on each save, we use the Watchr gem. It will auto-build on each save in the source directory:

```
watchr ./Watchrfile
```

_(or `bundle exec watchr ./Watchrfile`)_

## Deploying to S3

Before deployment you must specify an env var with Basho's S3 access/secret key. You also must specify the S3 bucket we're deploying to, and the cloudfront id (the CDN we must invalidate to force a publication to be found).

In bash, something like this in your ~/.bash_login file should do it.

```bash
export AWS_ACCESS_KEY_ID="XXXXX"
export AWS_SECRET_ACCESS_KEY="XXXXX"
export AWS_S3_BUCKET='riakdocs.en'
export AWS_CLOUDFRONT_DIST_ID="E2Q6TQ5O0XT58T"
```

_The bucket and Cloudfront values are per language._

language | bucket      | cloudfront id
---------|-------------|---------------
English  | riakdocs.en | E2Q6TQ5O0XT58T
Japanese | riakdocs.jp | ENDQVZ5Y7OVJN


**Keep it secret. Keep it safe.**

Then to deploy, run the deploy.rb scripts with a riak version:

```
./deploy 1.2.0
```

If you need to deploy riak and riakcs with different versions, add the CS version at the end. This would be riak 1.2 and riakcs 1.1.

```
./deploy 1.2.0 1.1.0
```

Note: this does more than deploy to S3, it also invalidates the CloudFront (CF) cache, our CDN. Even if all the files are successfully pushed to S3, until CF is invalidated, you'll not see the new files on http://docs.basho.com.

## Deploying the Search Index

This is still a work in progress, but adding `INDEX=true` will deploy the docs to yokozuna.

```
RIAK_VERSION=1.2.1 INDEX=true DEPLOY=true middleman build
```
