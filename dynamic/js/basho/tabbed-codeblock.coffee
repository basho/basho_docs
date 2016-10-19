###
Tabbed Code Blocks Generation and Interaction
=============================================

When multiple <pre><code> blocks appear next to one another, we want to collapse
the separate blocks into a single element with languages separated by tabs. This
file includes all the logic necessary to generate those collapsed elements, and
react to the tab-based selections.

For reference, Markdown will generate structures similar to the below

  <pre><code class="language-ruby">
    . . .
  </code></pre>
  <pre><code class="language-java">
    . . .
  </code></pre>

We aim to build that into,

  <div class="tabbed-codeblock">
    <div class="tabbed-codeblock__tab-set-wrapper">
      <div class="float-left    tabbed-codeblock__edge-fader--left "><span class="inline-block   tabbed-codeblock__edge-fader__arrow"></span></div>
      <div class="float-right   tabbed-codeblock__edge-fader--right"><span class="inline-block   tabbed-codeblock__edge-fader__arrow"></span></div>
      <ul class="overflow-x   tabbed-codeblock__tab-set">
        <li class="inline-block   tabbed-codeblock__tab tabbed-codeblock__tab--active">
          <a class="block" href="#codeblock__java000"   data-language="language-python">
            Java
          </a>
        </li>
        <li class="inline-block   tabbed-codeblock__tab">
          <a class="block" href="#codeblock__python000" data-language="language-java">
            Python
          </a>
        </li>
      </ul>
    </div>
    <div class="tabbed-codeblock__code-segments">
      <pre class="tabbed-codeblock__segment" id="codeblock__java000"><code class="language-java">
        . . .
      </code></pre>
      <pre class="tabbed-codeblock__segment" id="codeblock__python000"><code class="language-python hljs">
        . . .
      </code></pre>
    </div>
  </div>
###

#TODO: strict mode enables a pretty large number of runtime checks. We probably
#      want to turn it off when we deploy to prod.
#      Perhaps we can figure out some way to enable it when building debug?
'use strict'



## JQuery .ready() Execution
## =========================
$ ->
  ## Inject tabbed-codeblock structure

  # Iterate over all <pre> blocks, and wrap those that are consecutive in
  # .tabbed-codeblock and .tabbed-codeblock__contents divs. We do this before
  # any other modification to allow us to track the number of such elements and
  # uniquely identify individual tabs.
  $('pre').each(
    () ->
      pre = $(this)

      # If the prev() element is a <pre> we will have already processed this
      # block and its siblings. If the next() element is not a <pre>, we have no
      # need to wrap the block in tabs. In either case; skip.
      return if pre.prev().is('pre')
      return if not pre.next().is('pre')

      pre.add(pre.nextUntil(':not(pre)')).wrapAll(
        '<div class="tabbed-codeblock"><div class="tabbed-codeblock__code-segments">')

      return
  )

  # At this point, each touched set will be in the form
  #
  #   <div class="tabbed-codeblock">
  #     <div class="tabbed-codeblock__code-segments">
  #       <pre><code class="language-*">
  #         . . .
  #       </pre></code>
  #     </div>
  #   </div>


  # Iterate over every .tabbed-codeblock that was just created, build the tabs
  # set and uniquely name each segment.
  $('.tabbed-codeblock').each(
    (index) ->
      codeblock = $(this)

      # Begin building the Tabs element and the wrapper that will encapsulate
      # the list of tabs, and the overlaid edge-faders. The `tab_set` <ul> will
      # be modified below, and so will be created separately and appended as the
      # last element of the wrapper.
      #TODO: We should always add the edge faders, but the arrows should be
      #      conditional. If the width of the tab set requires scrolling, then
      #      we should include the arrows. Otherwise, we should leave them off.
      tabs_wrapper = $('<div class="tabbed-codeblock__tab-set-wrapper">' +
          '<div class="float-left    tabbed-codeblock__edge-fader--left "><span class="inline-block   tabbed-codeblock__edge-fader__arrow"></span></div>' +
          '<div class="float-right   tabbed-codeblock__edge-fader--right"><span class="inline-block   tabbed-codeblock__edge-fader__arrow"></span></div>' +
        '</div>')
      tab_set = $('<ul class="overflow-x   tabbed-codeblock__tab-set">')
      tabs_wrapper.append(tab_set)

      # Iterate over each <pre> element in the code-segments of the current
      # codeblock, modify the id of the pre, and add a corresponding tab <li> to
      # the tab set.
      codeblock.children('.tabbed-codeblock__code-segments')
               .children('pre')
               .each(
        () ->
          pre = $(this)

          # Capture the language of the `<pre><code class="language-*">`
          # element. I'm using a regex here because I'm kinda dumb. Note that
          # it's using non-capturing elements to verify that the language is
          # wrapped by start-/end-of-line, or whitespace
          language = pre.children('code')
                        .attr('class')
                        .match(/(?:^|\s)language-(.+?)(?:\s|$)/)[1]

          # Unique ID for the block. ex; "tabbed-codeblock__ruby004"
          segment_id = "tabbed-codeblock__" + language + padNumber(index,3)

          pre.addClass('tabbed-codeblock__segment')
          pre.attr('id', segment_id)

          # Build a tab <li> in the form,
          #
          #   <li class="inline-block   tabbed-codeblock__tab">
          #     <a class="block" href="#codeblock__java004" data-language="language-java">
          #       Java
          #     </a>
          #   </li>
          tab_set.append('<li class="inline-block   tabbed-codeblock__tab">' +
                           '<a class="block" href="#' + segment_id + '" ' +
                              'data-language="' + language + '">' +
                             language +
                           '</a>' +
                         '</li>')
      )

      # Absolutely terrible 'activate the first tab' logic.
      #TODO: Make this better. Somehow.
      tab_set.find('.tabbed-codeblock__tab').first()
             .addClass('tabbed-codeblock__tab--active')
      codeblock.find('.tabbed-codeblock__segment').first()
               .addClass('tabbed-codeblock__segment--active')

      # At this point, the tabs wrapper is fully built, and just needs to be
      # prepended as the first child of the current .tabbed-codeblock.
      codeblock.prepend(tabs_wrapper)

      return
  )

  return