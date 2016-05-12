# Please note that this file must be run before toc-generator.coffee
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
      return if that.hasClass('title') # Early out for e.g. ToC titles.
      # Hugo will have already converted the header's text into and ID. Pull
      # that and use it to create an <a> element.
      id = that.attr('id')
      content = that.html()
      anchor = "<a href=\"##{id}\">#{content}</a>"
      that.html(anchor)
