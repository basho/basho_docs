# Please note that this file must be run after anchor-generator.coffee.
$ ->
  $("article").each ->
    article = $(this)
    toc = $("#toc")
    return if toc.length == 0
    toc_items = $("#toc-items")

    h2s = article.find('h2')
    # no need for a TOC with 3 or fewer sections
    if h2s.length >= 3
      h2s.each ->
        return if $(this).hasClass('title')
        # Because this is being run after anchors have been applied to all
        # header elements on the page, the `$(this).html()` will include the
        # correct anchor.
        content = $(this).html()
        toc_items.append("<li>#{content}</li>")

      $('#toc-wrapper').addClass('multi') if h2s.length >= 6
    else
      toc.remove()
