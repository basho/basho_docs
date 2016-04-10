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
      that = $(this)
      return if that.hasClass('title')
      content = that.html()
      name = that.text()
                 .replace(/[\W+_]/g,'-')
                 .replace(/-+$/g,'')
                 .replace(/^-+/g,'')
                 .replace(/--+/g,'-')
                 .toLowerCase()
      anchor = "<a href=\"##{name}\">#{content}</a>"
      that.html(anchor).attr('id', name)
