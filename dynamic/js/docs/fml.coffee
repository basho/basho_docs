$ ->
  if window.location.hash?
    details = $(window.location.hash)
    if details.length > 0
      details.find('details').attr('open', 'open')
      details.find('details').attr('style', 'display:block')
      details[0].scrollIntoView()
  $('section.qna').each ()->
    id = $(this).attr('id')
    $(this).find('details.question > summary').bind 'click', ()->
      window.location.hash = id
