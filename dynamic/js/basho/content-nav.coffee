###
Content Navigation Interaction
==============================
The Content Navigation element is pre-built in the HTML of each Hugo page, but
interactions with it are based entirely on JavaScript. On smaller widths, it's
also hidden by default.

This file includes all the logic necessary to hide and show the Content
Navigation element on smaller widths, and to hide and show sub-menus for any
size screen.
###

## JQuery .ready() Execution
## =========================
$ ->
  ## Batch + save lookups.
  side_nav            = $('.content-nav')
  content_well        = $('.content-well')
  content_nav_toggles = $('.js_toggle-content-nav')
  menu_items          = $('.content-menu__item')

  # The .content-nav is a `.hidden-sm-down` div which sets `display : none
  # !important`, so we need to remove that class to be able to show the nav at
  # all. Its styling will put it off-screen to the left, so it will still be
  # visually hidden.
  #TODO: Make sure screen readers don't pick up the visually hidden nav in the
  #      unopened state.
  side_nav.removeClass('hidden-sm-down')


  ## Wire up interactions

  # Content Navigation Display Toggle
  # ---------------------------------
  # When a `js_toggle-content-nav` element is clicked, toggle the salient
  # modifier classes on the `.content-nav` and `.content-well`.
  # We also want to prevent scrolling actions on the Content Well when the
  # Content Nav is covering the screen. We accomplish this by setting `position:
  # fixed` in `.content-well--immobile`, But this doesn't save the current
  # scroll state of the Content Well. We have to pull some tricks with `top:`
  # and `scrollHeight` to make it look good.
  content_nav_toggles.on('click',
    () ->
      # Capture state that'll let us futz with scrolling later.
      window_scroll = $(window).scrollTop()
      content_well_top = content_well.offset().top

      # Toggle display classes
      side_nav.toggleClass('content-nav--fullscreen')
      content_well.toggleClass('content-well--immobile')

      # Modify the Content Well's top or the Window's scroll to avoid a blink
      if content_well.hasClass('content-well--immobile')
        content_well.css('top', (-1 * window_scroll))
      else
        $(window).scrollTop((-1 * content_well_top))
        content_well.css('top', '')
  )

  # Menu Item Sub-Menu Toggles
  # --------------------------
  menu_items.on('click', '.content-menu__menu-toggle',
    () ->
      # This assumes that the first sibling of the toggle's parent is the
      # content-menu list we want to show/hide.
      #TODO: throw an error or something if the menu isn't actually a menu item
      menu = $(this.parentNode.nextElementSibling)

      # Cheat here a bit, and just use JQuery's `.slideUp()` and `.slideDown()`.
      if menu.hasClass('content-menu--open') then menu.slideUp('fast')
      else                                        menu.slideDown('fast')

      menu.toggleClass('content-menu--open')
      $(this).toggleClass('content-menu__menu-toggle--open')
  )

  return
