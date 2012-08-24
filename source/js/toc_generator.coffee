$ ->
  $("article").each ->
    article = $(this)
    toc = $("#js-toc")
    toc_items = $("#js-toc-items")
    toc_heading_link = $("#js-toc-heading-link")

    article.find('h2').each ->
      content = $(this).html()
      name = content
      # name = content.replace(' ', '+')
      # name = encodeURI(content)
      anchor = "<a href=\"##{name}\">#{content}</a>";
      $(this).attr('id', name)
      $(this).html(anchor)
      toc_items.append("<li><a href=\"##{name}\">#{content}</a></li>")

    $("#js-toc").append(toc_items)
