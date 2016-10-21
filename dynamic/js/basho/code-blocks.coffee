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
    <span class="inline-block code-block__title">
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


# ```get_code_language :: ($('code')) -> Str or None```
# Capture the language of a `<code class="language-*">` element. If no match is
# found (or if no class exists on the <code>), `undefined` will be returned.
# Note that the regex written is using non-capturing elements to verify that
# the language is wrapped by start-/end-of-line, or whitespace.
get_code_language = (code) ->
  code.attr('class')?.match(/(?:^|\s)(language-.+?)(?:\s|$)/)?[1]



## Immediate DOM Manipulations
## ===========================

## Iterate over every <pre><code> element set, conditionally highlight the
#  included code and append any modifier classes required.
$('pre > code').each(
  (index) ->
    code = $(this)
    pre  = code.parent()

    language = get_code_language(code)

    # If we found a language, pass the code block into highlight.js.
    if language
      # We rely on the `language` string to not include '.'s, in part because we
      # use the strings as an `id` later (and a '.' will break the CSS ID
      # selector) and in part because we just don't want to worry about '.'s.
      # Strip them and modify the class as necessary.
      if language.indexOf('.') != -1
        code.removeClass(language)
        language = language.replace(/\./g, '')
        code.addClass(language)

      #TODO: Stop using `hljf.highlightBlock` and manually call `hljs.highlight`
      #      so we can manually specify a language.
      hljs.highlightBlock(pre[0]);

    # Conditionally apply tabs or a title to the Code Block.
    # 1. If this <pre> is already a child of a .code-block__code-set,
    #    it has already been processed as part of a Tabbed Code Block, so skip.
    # 2. If this <pre> does not have an immediate <pre> sibling but it does have
    #    a 'language-*' class, it should be given a title.
    # 3. If this <pre> has one or more immediate siblings that are also <pre>
    #    elements, we should wrap all of them in Tabbed Code Block.

    return if pre.parent().hasClass('code-block__code-set')             # 1

    siblings = pre.nextUntil(':not(pre)')

    if language and not siblings.length                                 # 2
      pre.wrap('<div class="code-block--titled">')

    if siblings.length                                                  # 3
      pre.add(siblings).wrapAll(
        '<div class="code-block--tabbed"><div class="code-block__code-set">'
      )
)


## Iterate over all newly generated .code-block--titled elements, and finish
#  adding the necessary tags.

#  NB. We're guaranteed to be acting on exactly one <pre><code> set per `each`,
#  and a language will be included in the class of the the <code> element.
$('.code-block--titled').each(
  (index) ->
    code_block = $(this)
    pre        = code_block.children('pre')
    code       = pre.children('code')

    language = get_code_language(code)

    # Fetch or build the presentation name. If one has not been explicitly
    # defined, strip the 'language-' from the class name.
    display_name = language_transforms[language]?.display_name
    display_name = language?.replace(/language-/, '') unless display_name

    pre.addClass("code-block__code")

    title = $('<span class="inline-block   code-block__title">' + display_name +
              '</span>').prependTo(code_block)
)


## Iterate over all newly generated .code-block--tabbed elements, and finish
#  adding the necessary tags, classes, and IDs.
#  NB. We're guaranteed to have at least two <pre><code> element sets, and we
#  cannot rely on the code elements including a language in their classes.
$('.code-block--tabbed').each(
  (code_block_index) ->
    code_block = $(this)

    # Begin building the Tab Set Wrapper <div> that will encapsulate the list of
    # tabs and the overlaid edge-faders. The Tab Set <ul> will be modified after
    # creation, and so will be assigned separately and appended as the last
    # element of the wrapper. After the tabs have all been created, we'll check
    # if we should add arrows to signal / drive scrolling.
    #TODO: We should always add the edge faders, but the arrows should be
    #      conditional. If the width of the tab set requires scrolling, then we
    #      should include the arrows. Otherwise, we should leave them off.
    tab_set_wrapper = $('<div class="code-block__tab-set-wrapper">' +
        '<div class="float-left    code-block__edge-fader--left "><span class="inline-block   code-block__edge-fader__arrow code-block__edge-fader__arrow--inactive"></span></div>' +
        '<div class="float-right   code-block__edge-fader--right"><span class="inline-block   code-block__edge-fader__arrow"></span></div>' +
      '</div>')
    tab_set = $('<ul class="overflow-x   code-block__tab-set">')
    tab_set.appendTo(tab_set_wrapper)

    # Iterate over each <pre> element that is a child of this Code Block, modify
    # the id of the pre, and add a corresponding Tab <li> to the Tab Set.
    code_block.find('pre').each(
      (code_index) ->
        pre  = $(this)
        code = pre.children()

        # The `language` extracted from the <code class=*> should not be
        # undefined, but we can't guarantee that it will have a valid value.
        language = get_code_language(code)

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
        code_id = "code_block__" + data_lang + "__" + padNumber(code_index, 3)

        # Modify the class/ID of the <pre> element.
        pre.addClass('code-block__code')
        pre.attr('id', code_id)

        # Build and append a Tab <li> to the Tab Set.
        tab_set.append('<li class="inline-block   code-block__tab">' +
                         '<a class="block" href="#' + code_id + '" ' +
                            'data-language="' + data_lang + '">' +
                            display_name +
                         '</a>' +
                       '</li>')
    )

    # Pick out the first Tab and first Code element and mark them as active.
    #TODO: This logic is absolutely terrible. Make it better. Somehow. Please?
    tab_set.find('.code-block__tab').first()
           .addClass('code-block__tab--active')
    code_block.find('.code-block__code').first()
              .addClass('code-block__code--active')

    # At this point, the tabs wrapper is fully built, and just needs to be
    # prepended as the first child of the current .code-block--tabbed.
    code_block.prepend(tab_set_wrapper)
)


## JQuery .ready() Execution
## =========================
$ ->

  ## Wire up interactions

  # Tab Selection
  # -------------
  # When a Tab is clicked, we want all Code Blocks that contain a matching entry
  # to hide their current code, and reveal the selected language.

  # Cache the lookup of all Tabbed Code Blocks.
  tabbed_code_blocks = $('.code-block--tabbed')

  ## On Click Trigger for Tabs
  $('.code-block__tab').on('click.codeblock_tab', 'a',
    (event) ->
      event.preventDefault()

      event_anchor = $(this)
      language = event_anchor.data('language')

      # Early out if we're in the active Tab.
      return if event_anchor.parent().hasClass('code-block__tab--active')

      # Capture the active element's top offset relative to the viewport. We'll
      # use this later to set the `$(window).scrollTop` s.t. the touched element
      # won't change Y position relative to the viewport.
      #NB. This isn't a JQuery call; it's a straight-up DOM lookup.
      top_offset = this.getBoundingClientRect().top

      # Iterate over all Tabbed Code Blocks, and manipulate the which Tabs and
      # Code elements are active.
      for element in tabbed_code_blocks
        code_block = $(element)

        # Lookup only the anchors whose `data-language` match the event_anchor.
        # Early-out if the codeblock doesn't contain any matching elements.
        anchor = code_block.find('[data-language="' + language + '"]')
        continue if not anchor.length

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
