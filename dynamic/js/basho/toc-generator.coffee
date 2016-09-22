###
Table of Contents Generation
============================
Assumes that <h1>-<h6> elements wrap anchor elements (anchorify-headings.coffee
does exactly that).

This file immediately introspects into (nearly) every heading, extracts that
heading's `.innerHTML` (making the assumption that the content is an anchor
element), and construct a Table of Contents out of that content.
###

## Immediate DOM Manipulations
## ===========================
#NB. This portion of code modifies the .table-of-contents element, and relies on
#    all heading elements (<h1>-<h6>) that are children of the <main> element
#    having already been created.
#    THIS CODE MUST BE LOADED _AFTER_ THE CLOSING </main> TAG.
$("main").each ->
  toc = $(".table-of-contents")

  # Early out in case the .table-of-contents element isn't present for
  # modification (in case `$display_toc == false`).
  return if toc.length == 0

  h2s = $(this).find('h2')

  # Early out in case there are fewer than 3 <h2> elements
  #TODO: Figure out if we should be removing the .table-of-contents element
  #      here, of if an empty one doesn't influence the flow at all.
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
