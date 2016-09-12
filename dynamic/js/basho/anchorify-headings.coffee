###
Heading Anchorification
=======================
Relies on Hugo behavior; automatic generation of <h1>-<h6> 'id's.
https://gohugo.io/extras/crossreferences/#how-to-generate-a-heading-anchor

This file sets up a single onReady() call that will introspect into (nearly)
every heading, extract that heading's pre-generated id, and wrap the heading in
an anchor that links to the extracted id.
###


## onReady() Executions
## ====================
$ ->
  $("main").each ->
    article = $(this)
    headings = article.find('h1')
      .add(article.find('h2'))
      .add(article.find('h3'))
      .add(article.find('h4'))
      .add(article.find('h5'))
      .add(article.find('h6'))
    #TODO: Figure out if there are headings we don't want to act as links (e.g.
    #      Table-of-Contents? Top-level titles?) and filter them out of the
    #      `headings` list.

    headings.each ->
      that     = $(this) # Memoize the lookup.
      id       = that.attr('id')
      contents = that.html()
      anchor   = "<a href=\"\##{id}\">#{contents}</a>"
      that.html(anchor)
