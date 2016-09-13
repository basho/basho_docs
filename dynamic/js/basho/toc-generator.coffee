###
Table of Contents Generation
============================
Assumes that <h1>-<h6> elements wrap anchor elements (anchorify-headings.coffee
does exactly that).

This file sets up a single onReady() call that will introspect into (nearly)
every heading, extract that heading's contents (making the assumption that the
content is an anchor element), and construct a Table of Contents.
###

## onReady() Executions
## ====================
$ ->
  $("main").each ->
    toc = $(".table-of-contents")

    # Early out in case `$display_toc == false`
    return if toc.length == 0

    h2s = $(this).find('h2')

    # Early out in case there are fewer than 3 <h2> elements
    return if h2s.length < 3

    # Build DOM elements in JQuery, to be appended later.
    toc_title   = $('<h3 class="table-of-contents__title">Contents</h3>')
    toc_wrapper = $('<div class="table-of-contents__wrapper"/>')
    toc_items   = $('<ol class ="table-of-contents__items"/>')

    toc_wrapper.addClass("table-of-contents__wrapper--multi") if h2s.length >= 6

    h2s.each ->
      # Because this is being run after anchors have been applied to all
      # header elements on the page, the `$(this).html()` will include the
      # correct anchor.
      content = $(this).html()
      toc_items.append($("<li/>", {
            class: "table-of-contents__item",
            html:  $(this).html()
          }))

    toc_title.appendTo(toc)
    toc_items.appendTo(toc_wrapper)
    toc_wrapper.appendTo(toc)
