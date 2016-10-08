###
Version Selector Generation and Interaction
===========================================
Requires sem-ver.coffee

This file defines a series of calls and callbacks that apply to the generation
of and interaction with the .version-selector element.

The root-level .version-selector div will be generated as part of the Hugo
compilation, but the version list will not. We don't want every generated HTML
file within a project/version to be modified every time a new version of that
project is released, so we've chosen to upload the version information as a JSON
to our server and dynamically generate the version lists at load-time.

Interaction is a simple mater of expanding (or simply showing) the list when the
.version-selector div is clicked, and collapsing it (or hiding it) when anything
else is clicked on.
###

#TODO: strict mode enables a pretty large number of runtime checks. We probably
#      want to turn it off when we deploy to prod.
#      Perhaps we can figure out some way to enable it when building debug?
'use strict'


# ```contentOfMeta :: (Str) -> Str or Undefined```
# Fetch the content of a <meta> tag of the given name. If no tag of the given
# name exists, `undefined` is returned.
#TODO: Probably good to move this to a general utilities file
contentOfMeta = (name) ->
  return $('meta[name='+name+']').attr('content')


# ```generateVersionLists :: () ->```
# Build the Version Selector list dynamically, using fetched JSON and metadata
# exposed by Hugo through <meta> tags.
# There's no need to fetch the JSON or build this DOM element before the site is
# rendered and interactable, so we're going to wrap the manipulation inside the
# `.getJSON` callback _that's inside_ a `.ready()` callback. Double indirection,
# baby.
#TODO: Consider mark the version-selector as inactive until the list is built.
#      This assumes it will take a substantial amount of time (1s or so) for the
#      element to be created. It should probably also only go invalid when JS is
#      enabled, and stay orange if we're never going to fetch the JSON...
generateVersionLists = () ->
  # Pull project/pathing information from the meta tags set up by Hugo
  project              =  contentOfMeta("project")              # ex; riak_kv
  current_version      =  contentOfMeta("version")              # ex; 2.1.4
  project_relative_url =  contentOfMeta("project_relative_url") # ex; installing/

  # The version_history <meta> tags will only exist if the front matter of the
  # given content .md page contains them, so these may be `undefined`.
  meta_version_hisotry_in = contentOfMeta("version_history_in")
  version_range = undefined
  meta_version_history_locations = contentOfMeta("version_history_locations")
  versions_locations = []

  if meta_version_hisotry_in
    version_range = SemVer.parseRange(meta_version_hisotry_in)

  if meta_version_history_locations
    locations_json = JSON.parse(meta_version_history_locations)
    versions_locations = for [range, path] in locations_json
                         [SemVer.parseRange(range), path]

  # Fetch the Project Descriptions from the server, and do all the heavy lifting
  # inside the `success` callback.
  $.getJSON('/data/project_descriptions.json',
    (data) ->
      version_selector_list_html = "" # Aggregator for the resulting HTML.

      project_data = data[project]

      project_path = project_data.path            # ex; /riak/kv
      latest_rel   = project_data.latest          # ex; 2.1.4
      lts_version  = project_data.lts             # ex; 2.0.7
      archived_url = project_data['archived_url'] # undefined, or a complete URL

      for release_set, set_index in project_data.releases.reverse()
        # List depth is used for setting color. Our CSS only has colors from 1
        # to 6, so cap anything deeper.
        list_depth = Math.min(6, (set_index+1))

        # We're want to act on the last element of the release set in the below
        # loop. I can't think of a better way to do that this.
        last_index = release_set.length - 1

        for release_version, index in release_set.reverse()
          release_sem_ver = SemVer.parse(release_version)

          # Record if this release_version is within the version_range
          in_version_range = not version_range or
                             SemVer.isInRange(release_sem_ver, version_range)

          # Start aggregating class modifiers that will be `.join("\n")`d
          # together once all modifier have been added.
          class_list = ["version-selector__list-element"]

          if in_version_range
            class_list.push("version-selector__list-element--"+list_depth)
          else
            class_list.push("version-selector__list-element--disabled")

          if index == 0
            class_list.push("version-selector__list-element--top")

          if index == last_index
            class_list.push("version-selector__list-element--bottom")

          if release_version == lts_version
            class_list.push("version-selector__list-element--lts")

          if release_version == current_version
            class_list.push("version-selector__list-element--current")

          # The class list is complete.
          # Build out the list contents and anchor based on metadata available.

          # If the list element is --disabled or --current it should not include
          # an active link, so skip giving them an href.
          anchor_tag = ""
          if (not in_version_range) or (release_version == current_version)
            anchor_tag = '<a>'
          else
            # Give the versions_locations overrides a change to direct this
            # release_version to a different project/version-relative url.
            # If none of the ranges match (or if there are no ranges), default
            # to the current page's project/version-relative url.
            relative_url = project_relative_url
            for [range, url] in versions_locations
              if SemVer.isInRange(release_sem_ver, range)
                relative_url = url
                break

            anchor = project_path+"/"+release_version+"/"+relative_url
            anchor_tag = '<a href="'+anchor+'">'

          # Build the full list element and add it to the
          # `version_selector_list_html` aggregator.
          #TODO: Consider importing a sprintf library.
          #      Because JS doesn't ship w/ that functionality? For... reasons?
          version_selector_list_html +=
              '<li class="'+class_list.join("\n")+'">'+
                anchor_tag+release_version+'</a>'+
              '</li>\n'

      # If this project has the optional `archived_url`, add a special "set"
      # with only a link out to the archived content.
      if archived_url
        class_list = ["version-selector__list-element",
                      "version-selector__list-element--6",
                      "version-selector__list-element--top",
                      "version-selector__list-element--bottom"]
        version_selector_list_html +=
          '<li class="'+class_list.join("\n")+'"><a href="'+archived_url+'">older</a></li>\n'

      # What we've all been waiting for; make the DOM modification.
      $(".version-selector__list").html(version_selector_list_html)
    )



## JQuery .ready() Execution
## =========================
$ ->
  # Build the version list
  generateVersionLists();

  # Wire up interactions
  version_badge = $('.version-selector__badge')
  version_list  = $('.version-selector__list')

  # When the badge is clicked, the list should toggle between shown and hidden.
  version_badge.on('click',
    () ->
      if 0 < version_badge.attr('class').search(".version-selector__badge--selected")
        version_list.hide()
        version_badge.removeClass(".version-selector__badge--selected")
      else
        version_list.show()
        version_badge.addClass(".version-selector__badge--selected")
      # Consume the event to prevent propagation.
      false
  )

  # When child elements of the list are clicked, nothing should occur. Just
  # return `true` to allow the event to bubble up.
  version_list.on('click', '.version-selector__list-element', () -> true )

  # When the current or a disabled list element is clicked nothing should
  # happen, so return `false` to consume the event.
  version_list.on('click', '.version-selector__list-element--disabled', () -> false )
  version_list.on('click', '.version-selector__list-element--current',  () -> false )

  # When anything else in the document is clicked, we should hide the list.
  $(document).on('click',
    () ->
      if 0 < version_badge.attr('class').search(".version-selector__badge--selected")
        version_list.hide()
        version_badge.removeClass(".version-selector__badge--selected")
  )
