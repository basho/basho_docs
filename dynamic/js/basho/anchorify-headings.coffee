###
Heading Anchorification
=======================
Relies on Hugo behavior; automatic generation of <h1>-<h6> 'id's.
https://gohugo.io/extras/crossreferences/#how-to-generate-a-heading-anchor

This file immediately introspects into (nearly) every heading within the <main>
element, extracts that heading's pre-generated id, and wraps the heading's
`.innerHTML` in an anchor that links to the extracted id.
###


## Immediate DOM Manipulations
## ===========================
#NB. This portion of code modifies all heading elements (<h1>-<h6>) that are
#    children of <main>.
# THIS CODE MUST BE LOADED _AFTER_ THE CLOSING </main> TAG.
$("main").each ->
  main = $(this)
  headings = main.find('h1')
    .add(main.find('h2'))
    .add(main.find('h3'))
    .add(main.find('h4'))
    .add(main.find('h5'))
    .add(main.find('h6'))

  headings.each ->
    that     = $(this) # Memoize the lookup.
    id       = that.attr('id')
    contents = that.html()
    anchor   = "<a class='anchor-icon' href='##{id}'></a>"
    that.wrapInner(anchor)
