###
Table of Contents Generation
============================
This file immediately (not waiting for an on-ready events) introspects into all
<h2> headings, extracts their `.innerHTML` content, and construct a Table of
Contents out of that content.

This file makes the assumption that all header elements have been given a unique
ID (likely by Hugo) and been modified to wrap anchors that link to the given
header ID.

THIS FILE RELIES ON anchorify-headings.coffee
THIS CODE MUST BE LOADED _AFTER_ THE CLOSING </main> TAG.
###

## Immediate DOM Manipulations
## ===========================

# Find the .table-of-contents element and early out in case it isn't present
# (e.g. in case `$display_toc == false` in the Hugo template).
toc = $(".table-of-contents")
return if not toc.length

# Finda all h2 elements and early out if there are fewer than 3 of them.
h2s = $('main').find('h2')
return if h2s.length < 3

# Build DOM elements in JQuery, to be appended later.
toc_title   = $('<h3 class="table-of-contents__title">Contents</h3>')
toc_wrapper = $('<div class="table-of-contents__wrapper"/>')
toc_items   = $('<ol class ="table-of-contents__items"/>').appendTo(toc_wrapper)

toc_wrapper.addClass("table-of-contents__wrapper--multi") if h2s.length >= 6

h2s.each ->
  # Because this is being run after anchors have been applied to all
  # header elements on the page, the `$(this).html()` will include the
  # correct anchor after stripping the .anchor-icon class from each link
  toc_items.append($("<li/>", {
        class : "table-of-contents__item",
        html  : $(this).html().replace(/anchor-icon/, "")
      }))

toc_title.appendTo(toc)
toc_wrapper.appendTo(toc)
