# Riak Docs

This repo holds all the content (and other bits, e.g. CSS, JavaScript,
etc.) for the most-excellent [Riak Docs](http://docs.basho.com), home of
the documentation for Riak (soon to be Riak KV), Riak CS (soon to be 
Riak S2), and Data Platform (coming soon). This document shows you how to
contribute to the docs, which we very much hope you will do!

## How to Contribute

The `basho_docs` repo works much like any other code repo.  You can 
contribute in two ways:

1. Submit a new [issue](https://github.com/basho/basho_docs/issues)
2. Make a change and submit a pull request.
3. (**bonus points**) Write up an entirely new page.

To make any change&mdash;be it as simple as a typo fix or as weighty as
the creation of an entirely new page full of Python client code
snippets&mdash;follow these instructions:

1. Fork the basho_docs repo.
2. Clone your fork to your local machine:

    ```shell
    $ git clone https://github.com/»YOURUSERNAME«/basho_docs.git
    ```

3. Add basho/basho_docs as an [upstream](https://help.github.com/articles/adding-a-remote/):

    ```shell
    $ git remote add basho https://github.com/basho/basho_docs.git
    ```

4. Create a new branch:

    ```shell
    $ git checkout -b »NEWBRANCHNAME«
    ```

5. Make changes on your branch and commit them.
6. Push to your fork:

    ```shell
    $ git push origin »BRANCHNAME«
    ```
    
7. Send us a pull request! **Make sure to submit your pull request to
the right version branch (e.g. riak/2.1.1)**, as we have removed all
content from the Master branch.

If it's a small or obvious change, we're likely to merge it right away.
If we have questions, we'll communicate with you in the pull
request's comments.

>Note
>If you're new to Git or rusty with Git, [GitHub Help](https://help.github.com/)
>has many step-by-step guides to dealing with forks, remote branches,
>pull requests, etc.

### Writing Docs

If you are writing a brand new page, first: Thanks! Second, you will want to
check out our updated [Style Guide](LINK), to make sure we can accept your
submission.

#### Repo Structure

All documents and resources (like images, CSS/SCSS,
JavaScript/CoffeeScript) live under the `source` directory.

At the moment, the docs are separated into directories according 
to their types, either a "reference" or a "guide". References are 
explicitly named, guides are everything else.

Put your new document in the place that seems the best. If it needs
to be moved, we'll let you know.

#### Document Metadata

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
URLs. The title will appear at the top of the document.

The `project` associates this file with a particular project. In most
cases the project will be `riak`, but it could also be `riakcs` (Riak
Cloud Storage).

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


The `document` labels what kind of document this is. Such as: `tutorial`, 
`reference`, `api`,`appendix`. These allow alternative look/feel 
combinations for different kinds of pages.

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


#### Navigation

There is a file named `ROOT/data/global_nav.yml`. You should add
your new page to its proper place in this file to get your page
to show up in the navigation menu.
