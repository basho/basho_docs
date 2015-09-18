$ ->
  $("article").each ->
    article = $(this)
    toc = $("#toc")
    return if toc.length == 0
    toc_items = $("#toc-items")

    # no need for a TOC with 3 or less sections
    h2s = article.find('h2')
    if h2s.length >= 3
      h2s.each ->
        return if $(this).hasClass('title')
        content = $(this).html()
        name = content.replace(/\W+/g,'-')
        # name = content.replace(' ', '+')
        # name = encodeURI(content)
        anchor = "<a href=\"##{name}\">#{content}</a>";
        $(this).attr('id', name)
        $(this).html(anchor)
        toc_items.append("<li><a href=\"##{name}\">#{content}</a></li>")
       
      $('#toc-wrapper').addClass('multi') if h2s.length >= 6
    else
      toc.remove()
