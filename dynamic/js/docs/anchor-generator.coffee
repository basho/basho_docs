$ ->
  $("article").each ->
    article = $(this)
    # We don't fetch 'h1's here because we don't want there to be any h1s save
    # for the automatically generated title.
    headers = article.find('h2')
      .add(article.find('h3'))
      .add(article.find('h4'))
      .add(article.find('h5'))
      .add(article.find('h6'))

    headers.each ->
      return if $(this).hasClass('title')
      content = $(this).html()
      name = content.replace(/\W+/g,'-')
      anchor = "<a href=\"##{name}\">#{content}</a>";
      $(this).html(anchor).attr('id', name)
