$ ->
  $("#riak-index article section header .command").click ->
    section = $(this).parents("section")
    content = section.find(".content")
    return false if content.is(":visible")
    $("#riak-index article section").each ()->
      $(this).find(".content").slideUp()
      $(this).find('.more').show()
    content.slideDown()
    section.find('.more').hide()
    false
