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

## verifyArrowState :: (Element) -> None
##            Where    Element instanceof $
##                     Element.hasClass('.js_edge-fader--target')
##                     Element.parent().children().hasClass('edge-fader')
EdgeFader.verifyArrowState = ($target) ->
  max_left     = $target.maxScrollLeft()
  current_left = $target.scrollLeft()
  max_top      = $target.maxScrollTop()
  current_top  = $target.scrollTop()

  # Early out in case arrows are not (or, at least, should not be) shown.
  return if (max_left == 0 and max_top == 0)

  $wrapper = $target.parent()
  #NB. Any or all of the bellow _arrow selectors may come back empty. The calls
  # that use them are (or, at least, should be) built s.t. an empty list won't
  # cause JQuery to error.
  $left_arrow   = $wrapper.children('.edge-fader--left').children('.edge-fader__arrow')
  $right_arrow  = $wrapper.children('.edge-fader--right').children('.edge-fader__arrow')
  $top_arrow    = $wrapper.children('.edge-fader--top').children('.edge-fader__arrow')
  $bottom_arrow = $wrapper.children('.edge-fader--bottom').children('.edge-fader__arrow')

  # Check for arrow visibility (`max_* > 0`) and activity/inactivity.
  # The `+ 3`s and `- 3`s here are to add a little bit of padding to trigger the
  # `--inactive` state "early". Without those, floating point drift would make
  # hitting the extents hard-to-impossible.
  if max_left > 0 +3
    # If current_left > 0, we are able to scroll left.
    # If current_left < max_left, we are able to scroll right.
    if current_left > 0 + 3
      $left_arrow.removeClass('edge-fader__arrow--inactive')
    else
      $left_arrow.addClass('edge-fader__arrow--inactive')

    if current_left < max_left - 3
      $right_arrow.removeClass('edge-fader__arrow--inactive')
    else
      $right_arrow.addClass('edge-fader__arrow--inactive')

  if max_top > 0 +3
    # If current_top > 0, we are able to scroll up.
    # If current_top < max_top, we are able to scroll down.
    if current_top > 0 + 3
      $top_arrow.removeClass('edge-fader__arrow--inactive')
    else
      $top_arrow.addClass('edge-fader__arrow--inactive')

    if current_top < max_top - 3
      $bottom_arrow.removeClass('edge-fader__arrow--inactive')
    else
      $bottom_arrow.addClass('edge-fader__arrow--inactive')

  return

## showOrHideArrows :: (Element) -> None
##            Where    Element instanceof $
##                     Element.hasClass('.js_edge-fader--target')
##                     Element.parent().children().hasClass('edge-fader')
EdgeFader.showOrHideArrows = ($target) ->
  $wrapper = $target.parent()
  $left_arrow   = $wrapper.children('.edge-fader--left').children('.edge-fader__arrow')
  $right_arrow  = $wrapper.children('.edge-fader--right').children('.edge-fader__arrow')
  $top_arrow    = $wrapper.children('.edge-fader--top').children('.edge-fader__arrow')
  $bottom_arrow = $wrapper.children('.edge-fader--bottom').children('.edge-fader__arrow')

  # If max_left is > 0, we _may_ be able to scroll horizontally. If it is < 0,
  # we _will not_ be able to scroll horizontally.
  # Show or hide arrows accordingly.
  max_left = $target.maxScrollLeft()
  max_top  = $target.maxScrollTop()

  if max_left > 0
    $left_arrow.removeClass('edge-fader__arrow--invisible')
    $right_arrow.removeClass('edge-fader__arrow--invisible')
  else
    $left_arrow.addClass('edge-fader__arrow--invisible')
    $right_arrow.addClass('edge-fader__arrow--invisible')

  if max_top > 0
    $top_arrow.removeClass('edge-fader__arrow--invisible')
    $bottom_arrow.removeClass('edge-fader__arrow--invisible')
  else
    $top_arrow.addClass('edge-fader__arrow--invisible')
    $bottom_arrow.addClass('edge-fader__arrow--invisible')

  if max_left > 0 or max_top > 0
    # If we've gone from invisible to visible, we need to verify which arrow(s)
    # are currently scrollable.
    EdgeFader.verifyArrowState($target)

  return

## showArrows :: (Element) -> None
##      Where    Element instanceof $
##               Element.hasClass('.js_edge-fader--target')
##               Element.parent().children().hasClass('edge-fader')
EdgeFader.showArrows = ($target) ->
  $wrapper = $target.parent()
  $left_arrow   = $wrapper.children('.edge-fader--left').children('.edge-fader__arrow')
  $right_arrow  = $wrapper.children('.edge-fader--right').children('.edge-fader__arrow')
  $top_arrow    = $wrapper.children('.edge-fader--top').children('.edge-fader__arrow')
  $bottom_arrow = $wrapper.children('.edge-fader--bottom').children('.edge-fader__arrow')

  $left_arrow.removeClass('edge-fader__arrow--invisible')
  $right_arrow.removeClass('edge-fader__arrow--invisible')
  $top_arrow.removeClass('edge-fader__arrow--invisible')
  $bottom_arrow.removeClass('edge-fader__arrow--invisible')

## hideArrows :: (Element) -> None
##      Where    Element instanceof $
##               Element.hasClass('.js_edge-fader--target')
##               Element.parent().children().hasClass('edge-fader')
EdgeFader.hideArrows = ($target) ->
  $wrapper = $target.parent()
  $left_arrow   = $wrapper.children('.edge-fader--left').children('.edge-fader__arrow')
  $right_arrow  = $wrapper.children('.edge-fader--right').children('.edge-fader__arrow')
  $top_arrow    = $wrapper.children('.edge-fader--top').children('.edge-fader__arrow')
  $bottom_arrow = $wrapper.children('.edge-fader--bottom').children('.edge-fader__arrow')

  $left_arrow.addClass('edge-fader__arrow--invisible')
  $right_arrow.addClass('edge-fader__arrow--invisible')
  $top_arrow.addClass('edge-fader__arrow--invisible')
  $bottom_arrow.addClass('edge-fader__arrow--invisible')

## Obj.onClickCallback :: (Event) -> None
##               Where    Obj.hasClass('edge-fader__arrow')
# We're defining and exposing this method s.t. dynamically-created Edge Fader
# can (re)set their on('click') logic.
EdgeFader.onClickCallback = (event) ->
  $arrow   = $(this)
  $fader   = $arrow.parent()
  $wrapper = $fader.parent()
  $target  = $wrapper.children('.js_edge-fader--target')

  #NB. The $target.animate will trigger `scroll.edge-fader-target`. This is
  #    intended, and required to update the arrow active/inactive states.
  if      $fader.hasClass('edge-fader--left')
    new_pos = (-1 * $wrapper.width() * 0.75) + $target.scrollLeft()
    $target.animate({scrollLeft: new_pos}, 200);
  else if $fader.hasClass('edge-fader--right')
    new_pos = ( 1 * $wrapper.width() * 0.75) + $target.scrollLeft()
    $target.animate({scrollLeft: new_pos}, 200);
  else if $fader.hasClass('edge-fader--top')
    new_pos = (-1 * $wrapper.height() * 0.75) + $target.scrollTop()
    $target.animate({scrollTop: new_pos}, 200);
  else if $fader.hasClass('edge-fader--bottom')
    new_pos = ( 1 * $wrapper.height() * 0.75) + $target.scrollTop()
    $target.animate({scrollTop: new_pos}, 200);

  return

## JQuery .ready() Execution
## =========================
$ ->

  #NB. The selectors.coffee script dynamically generates a few Edge Faders, and
  #    so has to duplicate the below event watchers. If the below code changes,
  #    please be sure to also modify the last few calls in generateVersionLists.

  # Target Scrolling
  # ----------------
  # When we scroll the Edge Fader's Target, we will want to enable / disable the
  # scroll arrows to suggest when a user has hit the last tab.
  # Throttle the event to 4 times a second so we don't overload anything.
  $('.js_edge-fader--target').on('scroll.edge-fader-target',
    throttle(
      (event) -> ( EdgeFader.verifyArrowState($(this)) ),
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
  # When an arrow is clicked (or tapped), scroll the target element.
  $('.edge-fader__arrow').on('click.edge-fader-arrow',
    EdgeFader.onClickCallback
  )

  return