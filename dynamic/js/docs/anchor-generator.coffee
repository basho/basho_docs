$ ->
  $("article").each ->
    article = $(this)
    # We don't fetch 'h1's here because we don't want there to be any h1s save
    # for the automatically generated title.
    h2s = article.find('h2')
    h3s = article.find('h3')
    h4s = article.find('h4')
    h5s = article.find('h5')
    h6s = article.find('h6')

    # There's no real need for these to be five separate loops. I just wasn't
    # sure how to concat JQuery objects.
    h2s.each ->
      return if $(this).hasClass('title')
      content = $(this).html()
      name = content.replace(/\W+/g,'-')
      anchor = "<a href=\"##{name}\">#{content}</a>";
      $(this).attr('id', name)
      $(this).html(anchor)
    h3s.each ->
      return if $(this).hasClass('title')
      content = $(this).html()
      name = content.replace(/\W+/g,'-')
      anchor = "<a href=\"##{name}\">#{content}</a>";
      $(this).attr('id', name)
      $(this).html(anchor)
    h4s.each ->
      return if $(this).hasClass('title')
      content = $(this).html()
      name = content.replace(/\W+/g,'-')
      anchor = "<a href=\"##{name}\">#{content}</a>";
      $(this).attr('id', name)
      $(this).html(anchor)
    h5s.each ->
      return if $(this).hasClass('title')
      content = $(this).html()
      name = content.replace(/\W+/g,'-')
      anchor = "<a href=\"##{name}\">#{content}</a>";
      $(this).attr('id', name)
      $(this).html(anchor)
    h6s.each ->
      return if $(this).hasClass('title')
      content = $(this).html()
      name = content.replace(/\W+/g,'-')
      anchor = "<a href=\"##{name}\">#{content}</a>";
      $(this).attr('id', name)
      $(this).html(anchor)
