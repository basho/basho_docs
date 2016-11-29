###
Edge Faders; Interaction, and Helper Functions
==============================================

This file deals with the interaction and presentation of Edge Faders; whether or
not they are and visible and what happens when users interact with them.
These operations expect a relatively strict HTML format, outlined below;

  <div>                                                        # 1
    <div class="edge-fader edge-fader--left">                  # 2
      <span class="inline-block   edge-fader__arrow"></span>   # 3
    </div>
    <div class="edge-fader edge-fader--right">                 # 2
      <span class="inline-block   edge-fader__arrow"></span>   # 3
    </div>
    <div class="overflow   js_edge-fader--target"></div>       # 4
  </div

1. Edge Fader Wrapper
   This is the outer block element that wraps one set of Edge Faders (no more
   than one of each --left, --right, --top, & --bottom) and the block whose
   scrolling should be faded. This should be a fixed size element with
   non-static positioning.
2. Edge Faders
   These are absolutely positioned block elements that provide that fading
   gradient, and provide a place for the __arrow elements to be rendered.
3. Edge Fader Arrows
   These arrows are dynamically shown or hidden and active or inactive, based on
   window-size and scroll placement.
4. Edge Fader Target
   This is the element that actually scrolls, rendered underneath the gradient
   background of the Edge Faders. This element needs to have the
   '.js_edge-fader--target' applied to it.

Along with the JQuery event callbacks, this file defines the `window.EdgeFader`
object that exposes functions useful to other objects that wrap Edge Faders.
###


EdgeFader = window.EdgeFader = {}

## VerifyArrowState :: (Element) -> None
##            Where    Element instanceof $
##                     Element.hasClass('.js_edge-fader--target')
##                     Element.parent().children().hasClass('edge-fader')
EdgeFader.VerifyArrowState = ($target) ->
  max_left     = $target.maxScrollLeft()
  current_left = $target.scrollLeft()

  # Early out in case arrows are not (or, at least, should not be) shown.
  return if max_left == 0

  $wrapper = $target.parent()
  $left_arrow  = $wrapper.children('.edge-fader--left').children('.edge-fader__arrow')
  $right_arrow = $wrapper.children('.edge-fader--right').children('.edge-fader__arrow')

  # If current_left > 0, we are able to scroll left.
  # If current_left < max_left, we are able to scroll right.
  # The `+ 3` and `- 3` here are to add a little bit of padding to trigger the
  # `--inactive` state "early". Without those, floating point drift would make
  # hitting the extents hard-to-impossible.
  if current_left > 0 + 3
    $left_arrow.removeClass('edge-fader__arrow--inactive')
  else
    $left_arrow.addClass('edge-fader__arrow--inactive')

  if current_left < max_left - 3
    $right_arrow.removeClass('edge-fader__arrow--inactive')
  else
    $right_arrow.addClass('edge-fader__arrow--inactive')

  return

## showOrHideArrows :: (Element) -> None
##            Where    Element instanceof $
##                     Element.hasClass('.js_edge-fader--target')
##                     Element.parent().children().hasClass('edge-fader')
EdgeFader.showOrHideArrows = ($target) ->
  $wrapper = $target.parent()
  $left_arrow  = $wrapper.children('.edge-fader--left').children('.edge-fader__arrow')
  $right_arrow = $wrapper.children('.edge-fader--right').children('.edge-fader__arrow')

  # If max_left is > 0, we _may_ be able to scroll. If it is < 0, we _will not_
  # be able to scroll. Show or hide arrows accordingly.
  max_left = $target.maxScrollLeft()

  if max_left > 0
    $left_arrow.removeClass('edge-fader__arrow--invisible')
    $right_arrow.removeClass('edge-fader__arrow--invisible')
    # If we've gone from invisible to visible, we need to verify which arrow(s)
    # are currently scrollable.
    EdgeFader.VerifyArrowState($target)
  else
    $left_arrow.addClass('edge-fader__arrow--invisible')
    $right_arrow.addClass('edge-fader__arrow--invisible')

  return

## JQuery .ready() Execution
## =========================
$ ->

  # Target Scrolling
  # ----------------
  # When we scroll the Edge Fader's Target, we will want to enable / disable the
  # scroll arrows to suggest when a user has hit the last tab.
  # Throttle the event to 4 times a second so we don't overload anything.
  $('.edge-fader').parent().children('.js_edge-fader--target').on(
    'scroll.edge-fader-target',
    throttle(
      (event) -> ( EdgeFader.VerifyArrowState($(this)) ),
      250
    )
  )

  # Window Resizing
  # ---------------
  # When the window is resized (desktops changing size, mobile devices rotating)
  # there's a good chance that Edge Fader Targets will become obscured or be
  # revealed. In these cases, we should reevaluate whether Edge Fader Arrows
  # should be shown or hidden.
  # Throttle the event to 4 times a second so we don't overload anything.
  $(window).on('resize.edge-fader-target',
    throttle(
      (event) -> (
        $('.edge-fader').parent().children('.js_edge-fader--target').each(
          () -> EdgeFader.showOrHideArrows($(this))
        )
      ),
      250
    )
  )



  # Arrow Interactions
  # ------------------
  # When an arrow is clicked (or tapped), scroll the target element by 3/4 of
  # its current width.
  #TODO: 3/4 is an arbitrary and untested distance. Is it sufficient? Correct?
  $('.edge-fader__arrow').on('click.edge-fader-arrow'
    (event) -> (
      $arrow   = $(this)
      $wrapper = $arrow.parent().parent()
      $target  = $wrapper.children('.js_edge-fader--target')

      # Calculate the scroll difference and resulting target position.
      # NB. If we're in a `--left` edge fader, we should be scrolling left a
      # negative amount.
      pos = $wrapper.width() * 0.75;
      pos *= -1  if $arrow.parent().hasClass('edge-fader--left')
      pos += $target.scrollLeft()

      #NB. This will -- as intended -- trigger `scroll.edge-fader-target`.
      $target.animate({scrollLeft: pos}, 200);

      return
    )
  )

  return