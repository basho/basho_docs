$ ->
  unless toggleMenu?
    toggleMenu = (buttonID, menuID)->
      c = $(buttonID).attr('class')
      if c == 'selected'
        $(menuID).hide()
        $(buttonID).attr('class', 'unselected')
      else
        $(menuID).show()
        $(buttonID).attr('class', 'selected')

  $('#version-ddown-button').live 'click', ->
    toggleMenu('#version-ddown-button', '#version-list')
    #toggleSelected('#version-ddown-title')
    #toggleSelected('#version-ddown-arrow')

  # Mouse click on sub menu or account link
  $('#version-list, #version-ddown-button').live 'mouseup', -> false

  $(document).live 'mouseup', ->
    $('#version-list').hide()
    $('#version-ddown-button').attr('class', 'unselected')
