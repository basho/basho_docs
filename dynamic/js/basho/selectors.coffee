###
Selector Generation and Interaction
===================================
Requires sem-ver.coffee

This file defines a series of calls and callbacks that apply to the generation
of and interaction with .selector(s), .selector-pane(s) and related elements.

Currently we're only implementing a .selector--version and
.selector-pane--versions, but this logic should be written in a more general
form to allow us to expand on the components in the future, if need be.

The root-level .selector and .selector-pane elements will be included in the
Hugo compilation, but the lists displayed will not. We don't want every
generated HTML file within a project/version to be modified every time a new
version of that project is released, so we've chosen to upload the version
information as a JSON to our server and dynamically generate the version lists
at load-time.

Interaction is a mater of expanding the pane when the .selector__btn is clicked
(or touched), and collapsing it when anything else is clicked on.
###


#TODO: strict mode enables a pretty large number of runtime checks. We probably
#      want to turn it off when we deploy to prod.
#      Perhaps we can figure out some way to enable it when building debug?
'use strict'


## contentOfMeta :: (Str) -> Str or Undefined
# Fetch the content of a <meta> tag of the given name. If no tag of the given
# name exists, `undefined` is returned.
#TODO: Probably good to move this to a general utilities file
contentOfMeta = (name) ->
  return $('meta[name='+name+']').attr('content')


## generateVersionLists :: () -> None
# Build the Version Selector list dynamically, using fetched JSON and metadata
# exposed by Hugo through <meta> tags.
# There's no need to fetch the JSON or build this DOM element before the site is
# rendered and interactable, so we're going to wrap the DOM manipulation inside
# the `$.getJSON` callback _that's inside_ a `$.ready()` callback.
# Double indirection, baby.
#TODO: Consider marking the selectors as inactive until the list is built.
#      This assumes it will take a substantial amount of time (1s or so) for the
#      element to be created. It should probably also only go invalid when JS is
#      enabled, and stay orange if we're never going to fetch the JSON...
generateVersionLists = () ->
  # Pull project/pathing information from the meta tags set up by Hugo.
  project               =  contentOfMeta("project")               # ex; riak_kv
  current_version       =  contentOfMeta("version")               # ex; 2.1.4
  project_relative_path =  contentOfMeta("project_relative_path") # ex; installing/

  # The version_history <meta> tags will only exist if the front matter of the
  # given content .md page contains them, so these may be `undefined`.
  meta_version_hisotry_in        = contentOfMeta("version_history_in")
  meta_version_history_locations = contentOfMeta("version_history_locations")
  version_range      = undefined
  versions_locations = []

  if meta_version_hisotry_in
    version_range = SemVer.parseRange(meta_version_hisotry_in)

  if meta_version_history_locations
    locations_json = JSON.parse(meta_version_history_locations)
    versions_locations = for [range, path] in locations_json
                         [SemVer.parseRange(range), path]

  # Fetch the Project Descriptions from the server, and do all the heavy lifting
  # inside the `success` callback.
  if project then $.getJSON('/data/project_descriptions.json',
    (data) ->
      project_data = data[project]

      project_path = project_data.path            # ex; /riak/kv
      latest_rel   = project_data.latest          # ex; 2.1.4
      lts_version  = project_data.lts             # ex; 2.0.7
      archived_url = project_data['archived_url'] # undefined, or a complete URL

      # Aggregator for the resulting HTML. To be added into the
      # div.selector-pane--versions
      version_selector_list_html = ''

      # Loop over each release set.
      for release_set, set_index in project_data.releases.reverse()

        # Open a new release list.
        # NB. Because we have to use inline-blocks for each list, whitespace
        #     in between them (newlines, tabs, spaces) will translate into a
        #     space character rendered between each list. This is **super
        #     annoying** for a lot of reasons. To prevent that whitespace from
        #     being added we must avoid adding whitespace between pretty much
        #     every element.
        # NB. We're setting a descending z-index per selector-list__sizing-box
        #     to ensure scrollbars are always intractable. Without this explicit
        #     z-index, the scrollbar of a __sizing-box may be partially covered
        #     by the padding of a selector-list immediately to its right.
        version_selector_list_html += '<div class="inline-block   selector-list__sizing-box" style="z-index:'+(100-set_index)+';">'

        arrow_str = '<span class="inline-block   edge-fader__arrow edge-fader__arrow--invisible"></span>'
        version_selector_list_html += '<div class="edge-fader edge-fader--top ">' + arrow_str + '</div>'
        version_selector_list_html += '<div class="edge-fader edge-fader--bottom ">' + arrow_str + '</div>'

        version_selector_list_html += '<div class="overflow-y   selector-list__scroll-box  js_edge-fader--target">'

        version_selector_list_html += '<ul class="selector-list">'

        # List depth is used for setting color. Our CSS only has colors from 1
        # to 6, so cap anything deeper.
        list_depth = Math.min(6, (set_index+1))

        for release_version, index in release_set.reverse()
          release_sem_ver  = SemVer.parse(release_version)
          in_version_range = not version_range or
                             SemVer.isInRange(release_sem_ver, version_range)

          # Start aggregating class modifiers that will be `.join("\n")`d
          # together to form the .selector-list__element's complete class once
          # all modifiers have been added.
          class_list = ["selector-list__element"]

          # If this page didn't exist for the given version of the project, set
          # it's "depth" to `disabled`, rather than a numerical depth.
          if in_version_range
            class_list.push("selector-list__element--"+list_depth)
          else
            class_list.push("selector-list__element--disabled")

          # If a this is the first version in a set, mark it as `open`. If it is
          # the last, mark it as `closing`.
          # NB. A single element can open and close a list.
          if index == 0
            class_list.push("selector-list__element--opening")
          if index == (release_set.length - 1)
            class_list.push("selector-list__element--closing")

          #TODO: We're not acting on these tags, yet. They should also probably
          #      apply to a version set, rather than a specific version.
          if release_version == lts_version
            class_list.push("selector-list__element--lts")

          #TODO: We're not acting on these tags, yet. They should also probably
          #      apply to a version set, rather than a specific version.
          if release_version == current_version
            class_list.push("selector-list__element--current")

          # Build out the list element's anchor based on metadata available.
          # If the list element is --disabled or --current it should not include
          # an active link, so skip giving them an href.
          anchor_tag = '<a class="block">'
          # Otherwise, give the versions_locations overrides a chance to direct
          # this release_version to a different version-relative url.
          if in_version_range and (release_version != current_version)
            # If none of the ranges match (or if there are no ranges), default
            # to the current page's project/version-relative url.
            relative_path = project_relative_path
            for [range, url] in versions_locations
              if SemVer.isInRange(release_sem_ver, range)
                relative_path = url
                break
            anchor = project_path+"/"+release_version+"/"+relative_path
            anchor_tag = '<a class="block" href="'+anchor+'">'

          # Build the full list element and add it to the html aggregator.
          #TODO: Consider importing a sprintf library.
          #      Because JS doesn't ship w/ that functionality? For... reasons?
          #NB. See above note re: whitespace.
          version_selector_list_html +=
              '<li class="'+class_list.join("\n")+'">'+
                anchor_tag+release_version+'</a>'+
              '</li>'

        # Close out the release set.
        #NB. See above note re: whitespace.
        version_selector_list_html += '</ul></div></div>'

      # After all listed versions have been added, check if this project has the
      # optional `archived_url`, and conditionally add a special "set" with only
      # a link out to the archived content.
      if archived_url
        class_list = ["selector-list__element",
                      "selector-list__element--archived",
                      "selector-list__element--opening",
                      "selector-list__element--closing"]

        # We can skip the Edge Fader here, b/c we know there's only ever going
        # to be one "Older" element.
        #NB. See above note re: whitespace.
        version_selector_list_html += '<div class="inline-block   selector-list__sizing-box">'
        version_selector_list_html += '<div class="overflow-y   selector-list__scroll-box">'

        version_selector_list_html += '<ul class="selector-list">\n'
        version_selector_list_html +=
          '<li class="'+class_list.join("\n")+'"><a class="block" href="'+archived_url+'">older</a></li>'
        version_selector_list_html += '</ul></div></div>'


      # What we've all been waiting for; make the DOM modification.
      $('.selector-pane--versions').html(version_selector_list_html)


      # With the lists added to the DOM, we can capture the height of the
      # tallest one, and set the height of the selector-pane's __shadow-box
      # and __sizing-box.
      # NB. This calculation needs to be redone every time breakpoints (sm to
      # md, md to lg) are hit, but we're going to cheat and add it into a
      # general window.on(resize) call.
      $version_pane         = $('.selector-pane--versions')
      $version_pane__shadow = $version_pane.parent()
      $version_pane__sizing = $version_pane__shadow.parent()

      tallest_list = Math.max.apply(
          Math,
          $version_pane.find('.selector-list').map(() -> $(this).outerHeight())
        )
      $version_pane__shadow.css('height', tallest_list + (2).rem())
      $version_pane__sizing.css('height', tallest_list + (2).rem())


      ## HACK:
      #  Because these dynamic elements were added after the JQuery.ready()
      #  clause that defied 'scroll' and 'click' event listeners on all other
      #  Edge Faders, we need to re-do that effort for the new elements.

      $('.selector-pane--versions').find('.js_edge-fader--target').on(
        'scroll.selector-fader-target',
        throttle(
          (event) -> ( EdgeFader.verifyArrowState($(this)) ),
          250
        )
      )

      $('.edge-fader__arrow').on('click.selector-fader-arrow',
        EdgeFader.onClickCallback
      )

    ) # *end getJSON callback*



## JQuery .ready() Execution
## =========================
$ ->
  ## Build the version list
  generateVersionLists();


  ## Wire up interactions

  # Save shared lookups s.t. we don't have to constantly query the DOM.
  $content_nav          = $('.content-nav')
  $content_nav__primary = $('.content-nav__primary')
  $version_selector     = $('.selector--version')
  $version_pane         = $('.selector-pane--versions')
  $version_pane__shadow = $version_pane.parent()
  $version_pane__sizing = $version_pane__shadow.parent()

  # When we open the .selector-pane--versions we're going to want to give each
  # .selector-list inside it a chance to show its Edge Fader arrows. We don't
  # want this to happen until the version pane is fully open (500ms), and we
  # don't want someone mashing the Version Selector button to see a bunch of
  # weird artifacts from this logic being run mid-transition.
  # The answer; a debounced function that wraps the .selector-list show-or-hide
  # logic, and only runs it when the .selector-pane--versions is not a child of
  # a --hidden pane.
  # Simple, right?
  debouced_show_or_hide_selector_list_arrows = debounce(
    () -> (
      if $version_pane.closest('.selector-pane__sizing-box--hidden').length == 0
        $version_pane.find('.selector-list__scroll-box').each(
          (index) -> EdgeFader.showOrHideArrows($(this))
        )
    ),
    500)

  # When the selector button is clicked, toggle the button's open/closed state,
  # and manually set the box's hight / max-height of the .selector-pane based on
  # the current state of the pane's contents, and the .content-nav__primary.
  $version_selector.on('click.toggle_selector', '.selector__btn',
    (event) ->
      $selector = $(this).parent()
      $selector.toggleClass('selector--open')
      $version_pane__sizing.toggleClass('selector-pane__sizing-box--hidden')

      if $selector.hasClass('selector--open')
        # Set the max-height on the __shadow-box and __sizing-box, with the
        # intent of leaving it on the __shadow-box, and resetting it to 0 on the
        # __sizing-box when the pane is closed.
        $version_pane__shadow.css('max-height', $content_nav__primary.outerHeight() + (0.25).rem())
        $version_pane__sizing.css('max-height', $content_nav__primary.outerHeight() + (0.25).rem())

        # If the selector has just opened, we may immediately test the
        # .selector-pane--versions for horizontal scrollablity. We need to wait
        # the 500ms for the pane to scale to its full height before we test each
        # .selector-list__scroll-box inside the page for vertical scrollablitly.
        EdgeFader.showOrHideArrows($version_pane)
        debouced_show_or_hide_selector_list_arrows()
      else
        $version_pane__sizing.css('max-height', '')

        EdgeFader.hideArrows($version_pane)
        $version_pane.find('.selector-list__scroll-box').each(
          (index) -> EdgeFader.hideArrows($(this))
        )
  )

  # On window resizes, recalculate the height of the version pane's
  # __shadow-box and __sizing-box and, of the selector--version is open,
  # recalculate max-height values as well.
  # This is pretty much a copy / paste from the last few lines of
  # generateVersionLists() and from the above click.toggle_selector logic, so if
  # these bits of code don't line up, please be concerned.
  $(window).on('resize.toggled_selector_reize',
    debounce(
      (event) -> (
        tallest_list = Math.max.apply(
            Math,
            $version_pane.find('.selector-list').map(() -> $(this).outerHeight())
          )
        $version_pane__shadow.css('height', tallest_list + (2).rem())
        $version_pane__sizing.css('height', tallest_list + (2).rem())

        if $version_selector.hasClass('selector--open')
          # Set the max-height on the __shadow-box and __sizing-box, with the
          # intent of leaving it on the __shadow-box, and resetting it to 0 on the
          # __sizing-box when the pane is closed.
          $version_pane__shadow.css('max-height', $content_nav__primary.outerHeight() + (0.25).rem())
          $version_pane__sizing.css('max-height', $content_nav__primary.outerHeight() + (0.25).rem())

          # If the selector has just opened, we may immediately test the
          # .selector-pane--versions for horizontal scrollablity. We need to wait
          # the 500ms for the pane to scale to its full height before we test each
          # .selector-list__scroll-box inside the page for vertical scrollablitly.
          EdgeFader.showOrHideArrows($version_pane)
          debouced_show_or_hide_selector_list_arrows()

        true
      ),
      250
    )
  )

  # Whenever interaction occurs outside of a .selector-pane, close all panes,
  # and hide all Edge Faders.
  $(document).on('click.selector_close',
    (event) ->
      # Early outs for actions inside the .selector-pane, or repeated clicks of
      # the .selector_btn.
      return true if $(event.target).closest('.selector-pane__sizing-box').length > 0
      return true if $(event.target).hasClass('selector__btn')

      $('.selector--open').removeClass('selector--open')
      $version_pane__sizing.addClass('selector-pane__sizing-box--hidden')

      $version_pane__sizing.css('max-height', '')

      EdgeFader.hideArrows($version_pane)
      $version_pane.find('.selector-list__scroll-box').each(
        (index) -> EdgeFader.hideArrows($(this))
      )
  )
