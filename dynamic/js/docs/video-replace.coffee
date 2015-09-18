$ ->
  # hopefully temporary, until we swap out
  # the heavy use of this function
  $('.iframe-video').each ->
    self = $(this)
    self.after(
      $("<iframe>").attr(
        'src',
        self.attr('id')
      ).attr("width", "400").attr("height", "225")
    )
