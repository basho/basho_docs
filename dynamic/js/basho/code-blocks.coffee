###
Code Blocks; Tabs and Titles, Generation and Interaction
========================================================

When a <pre><code> block is created using a fenced code block that's been given
an explicit language we will want to pass it though Highlight.js to give us
something to colorize.


When such a code block appears on its own, we will want to prepend a title to
that element to show what kind of file is being previewed.

For reference, Markdown will generate structures similar to the below,

  <pre><code class="language-python">
    . . .
  </code></pre>

We aim to build that into,

  <div class="code-block--titled">
    <span class="inline-block   code-block__title">
      Python
    </span>
    <pre class="code-block__code"><code class="language-python">
      . . .
    </code></pre>
  </div>


When multiple <pre><code> blocks appear next to one another, we want to collapse
the separate blocks into a single element with languages selectable via tabs.

For reference, Markdown will generate structures similar to the below,

  <pre><code class="language-ruby">
    . . .
  </code></pre>
  <pre><code class="language-java">
    . . .
  </code></pre>

We aim to build that into,

  <div class="code-block--tabbed">
    <div class="code-block__tab-set-wrapper">
      <div class="float-left    code-block__edge-fader--left "><span class="inline-block   code-block__edge-fader__arrow"></span></div>
      <div class="float-right   code-block__edge-fader--right"><span class="inline-block   code-block__edge-fader__arrow"></span></div>
      <ul class="overflow-x   code-block__tab-set">
        <li class="inline-block   code-block__tab code-block__tab--active">
          <a class="block" href="#code-block__language-java000" data-language="language-java">
            Java
          </a>
        </li>
        <li class="inline-block   code-block__tab">
          <a class="block" href="#code-block__language-python000" data-language="language-python">
            Python
          </a>
        </li>
      </ul>
    </div>
    <div class="code-block__code-set">
      <pre class="code-block__code" id="code-block__java000"><code class="language-java">
        . . .
      </code></pre>
      <pre class="code-block__code" id="code-block__python000"><code class="language-python hljs">
        . . .
      </code></pre>
    </div>
  </div>
###

#TODO: strict mode enables a pretty large number of runtime checks. We probably
#      want to turn it off when we deploy to prod.
#      Perhaps we can figure out some way to enable it when building debug?
'use strict'



language_transforms =
  'language-advancedconfig' : { display_name : 'advanced.config', highlight_as : 'language-erlang' }
  'language-appconfig'      : { display_name : 'app.config',      highlight_as : 'language-erlang' }
  'language-riakconf'       : { display_name : 'riak.conf',       highlight_as : 'language-matlab' }
  'language-riakcsconf'     : { display_name : 'riak-cs.conf',    highlight_as : 'language-matlab' }
  'language-stanchionconf'  : { display_name : 'stanchion.conf',  highlight_as : 'language-matlab' }
  'language-vmargs'         : { display_name : 'vm.args',         highlight_as : 'language-ini'    }
  'language-bash'           : { display_name : 'Shell',           highlight_as : '' }
  'language-curl'           : { display_name : 'CURL',            highlight_as : 'language-bash'   }
  'language-csharp'         : { display_name : 'C#',              highlight_as : '' }
  'language-erlang'         : { display_name : 'Erlang',          highlight_as : '' }
  'language-golang'         : { display_name : 'Go',              highlight_as : '' }
  'language-java'           : { display_name : 'Java',            highlight_as : '' }
  'language-javascript'     : { display_name : 'JS',              highlight_as : '' }
  'language-coffeescript'   : { display_name : 'Coffee',          highlight_as : '' }
  'language-json'           : { display_name : 'JSON',            highlight_as : '' }
  'language-php'            : { display_name : 'PHP',             highlight_as : '' }
  'language-protobuf'       : { display_name : 'Protobuf',        highlight_as : '' }
  'language-python'         : { display_name : 'Python',          highlight_as : '' }
  'language-ruby'           : { display_name : 'Ruby',            highlight_as : '' }
  'language-scala'          : { display_name : 'Scala',           highlight_as : '' }
  'language-sql'            : { display_name : 'SQL',             highlight_as : '' }
  'language-xml'            : { display_name : 'XML',             highlight_as : '' }


## getLanguage :: (Element) -> Str or None
##       Where    Element instanceof $
# Capture the language of a `<code class="language-*">` element. If no match is
# found (or if no class exists on the element), `undefined` will be returned.
getLanguage = (element) ->
  # Note that we use non-capturing groups to verify the language is wrapped by
  # whitespace / line terminators, and return only the first match found.
  element.attr('class')?.match(/(?:^|\s)(language-.+?)(?:\s|$)/)?[1]



## verifyArrowState :: (Element) -> None```
##            Where    Element instanceof $
##                     Element.hasClass('.code-block__tab-set-wrapper')
# Introspect in to a .code-block__tab-set-wrapper to,
# 1. Determine if scroll arrows should be present (if there are Tabs obscured on
#    one or both sides by the page width).
# 2. Make arrows visible if there are hidden Tabs.
# 3. Make arrows invisible if all Tabs are displayed.
verifyArrowState = (tab_set_wrapper) ->
  # Get all the lookups out of the way.
  left_edge_fader  = tab_set_wrapper.children('.code-block__edge-fader--left')
  right_edge_fader = tab_set_wrapper.children('.code-block__edge-fader--right')
  left_arrow       = left_edge_fader.children('.code-block__edge-fader__arrow')
  right_arrow      = right_edge_fader.children('.code-block__edge-fader__arrow')
  tab_set          = tab_set_wrapper.children('.code-block__tab-set')
  first_tab        = tab_set.children().first()
  last_tab         = tab_set.children().last()

  # Calculate the left and right extents of the visible Tab Set
  left_ext  = tab_set.offset().left + left_edge_fader.width()
  right_ext = tab_set.offset().left + left_edge_fader.width() + tab_set.width()

  # Determine if either side should be scrollable.
  # The `+ 3` and `- 3` here are to add a little bit of padding to trigger the
  # `--inactive` state. Without those, floating point drift would make hitting
  # the extents hard-to-impossible.
  left_is_scrollable  = (first_tab.offset().left + 3) <
                        left_ext
  right_is_scrollable = (last_tab.offset().left + last_tab.width() - 3) >
                        right_ext

  should_display_arrows = left_is_scrollable or right_is_scrollable

  # Branch based on whether we should or should not be displaying arrows.
  if should_display_arrows
    left_arrow.removeClass('code-block__edge-fader__arrow--invisible')
    right_arrow.removeClass('code-block__edge-fader__arrow--invisible')
  else
    left_arrow.addClass('code-block__edge-fader__arrow--invisible')
    right_arrow.addClass('code-block__edge-fader__arrow--invisible')

  if left_is_scrollable
    left_arrow.removeClass('code-block__edge-fader__arrow--inactive')
  else
    left_arrow.addClass('code-block__edge-fader__arrow--inactive')

  if right_is_scrollable
    right_arrow.removeClass('code-block__edge-fader__arrow--inactive')
  else
    right_arrow.addClass('code-block__edge-fader__arrow--inactive')



## Immediate DOM Manipulations
## ===========================

# Iterate over every <pre><code> element set, process (if present) any
# 'language-*' class, conditionally highlight the code element, and wrap the
# block in any modifier classes required.
$('pre > code').each(
  (index) ->
    code = $(this)
    pre  = code.parent()

    language = getLanguage(code)

    # If we found a language, process the string, then pass the block to hljs.
    if language
      # We rely on the `language` string not including any '.'s, in part because
      # we use the strings as an `id` later (and a '.' will break the CSS ID
      # selector) and in part because we just don't want to worry about '.'s.
      # Strip them and modify `language` and the class as necessary.
      if language.indexOf('.') != -1
        code.removeClass(language)
        language = language.replace(/\./g, '')
        code.addClass(language)

      # Trick Highlight.js to style using the language we want.
      # 1. If the specified language does not have an entry in the
      #    `language_transforms` object, don't highlight.
      # 2. If the specified language has a `language_transforms` entry that
      #    includes a `highlight_as` field, temporarily set the code's language
      #    class to `highligh_as` s.t. Highlight.js styles the correct language.
      # 3. If the specified language has a `language_transforms` entry that
      #    doesn't include a non-empty `highlight_as` field, just highlight.
      if language of language_transforms                                # 1
        highlight_as = language_transforms[language]?.highlight_as

        if highlight_as                                                 # 2
          code.removeClass(language)
          code.addClass(highlight_as)

        hljs.highlightBlock(code[0]);                                   # 2, 3

        if highlight_as                                                 # 2
          code.removeClass(highlight_as)
          code.addClass(language)


    # Conditionally mark the Code Block for extension with tabs or a title.
    # 1. If this <pre> is already a child of a .code-block__code-set,
    #    it has already been processed as part of a Tabbed Code Block, so skip.
    # 2. If this <pre> does not have an immediate <pre> sibling but it does have
    #    a 'language-*' class, it should be given a title.
    # 3. If this <pre> has one or more immediate siblings that are also <pre>
    #    elements, we should wrap all of them in Tabbed Code Block.
    return if pre.parent().hasClass('code-block__code-set')             # 1

    siblings = pre.nextUntil(':not(pre)')

    if language and siblings.length == 0                                # 2
      pre.wrap('<div class="code-block--titled">')

    # No `language` check here to more gracefully handle malformed tabbed sets.
    if siblings.length                                                  # 3
      pre.add(siblings).wrapAll(
        '<div class="code-block--tabbed"><div class="code-block__code-set">'
      )
)


# Iterate over all newly generated .code-block--titled elements and do what
# needs to be done to add a title.
# Note, we're guaranteed to be acting on exactly one <pre><code> set within each
# .code-block--titled, and a 'language-*' class must be present on the <code>.
$('.code-block--titled').each(
  (index) ->
    code_block = $(this)
    pre        = code_block.children('pre')
    code       = pre.children('code')
    language   = getLanguage(code)

    pre.addClass("code-block__code")

    # Fetch or build the presentation name. If one has not been explicitly
    # defined in the `language_transforms` object, strip 'language-' from the
    # language string and use what remains.
    display_name = language_transforms[language]?.display_name
    display_name = language.replace(/language-/, '') unless display_name

    $('<span class="inline-block   code-block__title">' + display_name +
      '</span>').prependTo(code_block)
)


# Iterate over all newly generated .code-block--tabbed elements and do what
# needs to be done to build the tab set. (Hint: it's a lot.)
# Note, we're guaranteed to have at least two <pre><code> element sets within
# each .code-block--tabbed, and we cannot rely on the <code> elements including
# a 'language-*' class.
$('.code-block--tabbed').each(
  (code_block_index) ->
    tabbed_code_block = $(this)
    code_block_set    = tabbed_code_block.children('.code-block__code-set')

    # Begin building the Tab Set Wrapper <div> that will encapsulate the list of
    # tabs, as well as the edge-fader overlays.
    # The Tab Set <ul> will be modified after creation, having a new tab added
    # per <pre><code> element, and so will be built separately and appended as
    # the last element of the wrapper.
    # After the tabs have all been created, we'll check if we should make the
    # edge-fader's arrows visible to signal / drive scrolling.
    arrow_str = '<span class="inline-block   code-block__edge-fader__arrow code-block__edge-fader__arrow--invisible"></span>'
    tab_set_wrapper = $('<div class="code-block__tab-set-wrapper">' +
        '<div class="float-left    code-block__edge-fader--left ">' + arrow_str + '</div>' +
        '<div class="float-right   code-block__edge-fader--right">' + arrow_str + '</div>' +
      '</div>')
    tab_set = $('<ul class="overflow-x   code-block__tab-set">')
    tab_set.appendTo(tab_set_wrapper)

    # Iterate over each <pre> element that is a child of this Code Block's set
    # of <pre><code> elements, modify the id of the <pre>, and add a
    # corresponding Tab <li> to the Tab Set.
    code_block_set.children('pre').each(
      (code_index) ->
        pre  = $(this)
        code = pre.children()

        # The `language` extracted from the <code class=*> will hopefully not be
        # `undefined`, but we can't guarantee that it will have a valid value.
        language = getLanguage(code)

        # Fetch or build the presentation name. If one has not been explicitly
        # defined, strip the 'language-' from the class name and use what's
        # left. If there is no language name, default to something ugly that we
        # will hopefully notice and fix.
        display_name = language_transforms[language]?.display_name
        display_name = language?.replace(/language-/, '') unless display_name
        display_name = "////" + padNumber(code_index, 2)  unless display_name

        # Conditionally setup a unnamed language string to be used for
        # identifiers. We would prefer this to simply be `language`, but if that
        # string is undefined, use the index of the code element within the
        # block to generate something.
        data_lang = language
        data_lang = "unnamed-lang" + padNumber(code_index,2) unless data_lang

        # Build a unique identifier for the code element using the defined
        # data_lang and the index of the Code Block within the page.
        code_id = "code-block__" + data_lang + padNumber(code_index, 3)

        # Modify the class/ID of the <pre> element.
        pre.addClass('code-block__code')
        pre.attr('id', code_id)

        # Build and append a Tab <li> to the Tab Set.
        tab_set.append('<li class="inline-block   code-block__tab">'  +
                         '<a class="block" '                          +
                              'href="#' + code_id + '" '              +
                              'data-language="' + data_lang + '">'    +
                            display_name                              +
                         '</a>'                                       +
                       '</li>')
    )

    # Pick out the first Tab and first Code element and mark them as active.
    #TODO: This logic is absolutely terrible. Make it better. Somehow. Please?
    tab_set.find('.code-block__tab').first()
           .addClass('code-block__tab--active')
    tabbed_code_block.find('.code-block__code').first()
                     .addClass('code-block__code--active')

    # At this point, the Tab Wrapper is fully built, and just needs to be
    # prepended as the first child of the current .code-block--tabbed.
    tabbed_code_block.prepend(tab_set_wrapper)

    # Do the thing with the arrows in the edge faders
    verifyArrowState(tab_set_wrapper)
)


## JQuery .ready() Execution
## =========================
$ ->

  ## Wire up interactions

  # Cache the lookups
  tabbed_code_blocks = $('.code-block--tabbed')
  tab_set_wrappers   = tabbed_code_blocks.children('.code-block__tab-set-wrapper')


  # Tab Set Resize
  # --------------
  # When the window is resized (desktops changing size, mobile devices rotating)
  # there's a good chance Code Blocks' Tabs will become obscured or be revealed.
  # On these resize event, re-do the arrow show/hide calculations.
  # Throttle the event to tripping 4 times a second so we don't overload.
  $(window).on('resize.code-block-resize',
    throttle(
      (event) -> (
        tab_set_wrappers.each( () -> verifyArrowState( $(this) ) )
      ),
      250
    )
  )


  # Arrow Interactions
  # ------------------
  # When an arrow is clicked (or tapped), scroll the Tab Set by 3/4 of the
  # current width of the Tab Set Wrapper.
  #TODO: 3/4 is an arbitrary and untested distance. Is it sufficient?
  $('.code-block__edge-fader__arrow').on('click.code-block-arrow'
    (event) -> (
      arrow           = $(this)
      tab_set_wrapper = arrow.closest('.code-block__tab-set-wrapper')
      tab_set         = tab_set_wrapper.children('.code-block__tab-set')

      # Calculate the scroll difference and resulting target position.
      # NB. If we're in a `--left` edge fader, we should be scrolling left a
      # negative amount.
      target = tab_set_wrapper.width() * 0.75;
      target *= -1  if arrow.parent().hasClass('code-block__edge-fader--left')
      target += tab_set.scrollLeft()

      $(tab_set).animate({scrollLeft: target}, 200);
    )
  )


  # Tab Scrolling
  # -------------
  # When we scroll the Tab Set we will want to enable / disable the scroll
  # arrows to suggest when a user has hit the last tab.
  #TODO: we're being super lazy about this right now, and just reusing the
  #      `verifyArrowState` logic. We should build a stripped-down version of
  #      that function and be more efficient with these calculations.
  $('.code-block__tab-set').on('scroll.code-block-tab-set',
    throttle(
      (event) -> (
        verifyArrowState( $(this).parent() )
      ),
      250
    )
  )



  # Tab Selection
  # -------------
  # When a Tab is clicked, we want all Code Blocks that contain a matching entry
  # to hide their current code, and reveal the selected language. All Code
  # Blocks without a matching entry will be ignored.
  $('.code-block__tab').on('click.code-block-tab', 'a',
    (event) ->
      event.preventDefault()

      event_anchor = $(this)
      language     = event_anchor.data('language')

      # Early out if we're in the active Tab.
      return if event_anchor.parent().hasClass('code-block__tab--active')

      # Capture the active element's top offset relative to the viewport. We'll
      # use this later to set the `$(window).scrollTop` s.t. the touched element
      # won't change Y position relative to the viewport after the page re-flow.
      # NB. This isn't a JQuery call; it's a straight-up DOM lookup.
      top_offset = this.getBoundingClientRect().top

      # Iterate over all Tabbed Code Blocks, and manipulate which Tabs and
      # Code elements are `--active`.
      for element in tabbed_code_blocks
        code_block = $(element)

        # Lookup only the anchors whose `data-language` match the event_anchor.
        # Early-out if the Code Block doesn't contain any matching elements.
        anchor = code_block.find('[data-language="' + language + '"]')
        continue if anchor.length == 0

        # Swap which Tab is active.
        code_block.find('.code-block__tab--active')
                  .removeClass('code-block__tab--active')
        anchor.parent()
              .addClass('code-block__tab--active')

        # Swap which Code element is active.
        code_block.find('.code-block__code--active')
                  .removeClass('code-block__code--active')
                  .hide()
        code_block.find(anchor.attr('href'))
                  .addClass('code-block__code--active')
                  .fadeIn()

      # Re-set the window's `scrollTop` w/o an animation to make it look like
      # the viewport hasn't moved in relation to the Code Block.
      $(window).scrollTop(event_anchor.offset().top - top_offset)

      return
  )

  return
