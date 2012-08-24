$ ->
  $("article").each ->
    article = $(this)
    toc = $("#toc")
    toc_items = $("#toc-items")
    # toc_heading_link = $("#toc-heading-link")

    article.find('h2').each ->
      content = $(this).html()
      name = content
      # name = content.replace(' ', '+')
      # name = encodeURI(content)
      anchor = "<a href=\"##{name}\">#{content}</a>";
      $(this).attr('id', name)
      $(this).html(anchor)
      toc_items.append("<li><a href=\"##{name}\">#{content}</a></li>")

    $("#toc").append(toc_items)
