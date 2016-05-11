## Front Matter

So the front matter that starts off every content file is very important. And it's very important to make sure it's right. There are future plans to build a front-matter-checker that will be included in the `rake build:hugo` task, so we can nip issues in the bud, but for now please see below for a discussion of the front matter we're expecting and how we're going to be using it.

```
# We're using YAML for the front matter, so we start it and end it with `---`
---
title: "Page Title"         # Required: Will be rendered as the top <h1> title.

title_supertext: "Title"    # Optional: Smaller supertext that will appear above the title.

description: "Descriptions" # Required (by Hugo). Page Description.
                            #           Treat is as something you'd expect to see as title text when
                            #           hovering you mouse over menus items.
project: "riak_s2"          # Required: Project Designation that this page is associated with.
project_version: "2.1.0"    # Required: Project Version that this page is associated with.

cononical_link: "URL"       # Required: The URL that should appear in the the page's
                            #           `<head><link rel="canonical" href=".." />` element

menu:                       # Required: Menus are automatically constructed using these mappings
  riak_s2-2.1.0:            # Required: The name of the menu this page will be added to.
                            #           Must be equal to `project + project_version`.
    name: "Using Riak S2"   # Required: (Reduced) name of the page.
                            #           This text will be rendered as the menu item's text.
    identifier: "using"     # Required: Must be string unique _to the menu_.
                            #           Every menu can have a `using` ID, but each menu _must not_
                            #           have duplicate entries. These errors will only be spotted by
                            #           Hugo during a build.
    weight: 200             # Required: Determines the order of menu entries at the given level.
                            #           The given level is either depth-0, when no `parent` is set,
                            #           or all pages whose `parent` identifier matches.
                            #           Order is ascending, which means the lowest weight will be at
                            #           the top, and the highest will sink to the bottom.
    parent: "identifier"    # Optional: Should be set to the identifier of the parent page, if one
                            #           exists. Left empty if the page should be a top level menu.
    pre-icon: font-icon     # Optional: Defines the Docs Font Icon that will appear to the left of
                            #           the menu Name. Will be ignored if a `parent` is set.

toc: true                   # Optional: Whether or not to render th Table of Contents.
commercial_offering: false  # Optional: Whether or not to render the Enterprise Content flag.
aliases:                    # Optional: A list of pages that should redirect to this page.
    - /riak/2.1.3/com...    #           A URL that should redirect here.
    - /riak/latest/co...    #           Another URL that should redirect here.
---
```

There is a separate layout for pages dedicated to presenting downloads links. There are a few additional elements that must be included, **in addition** to the above.

```
layout: downloads           # Required: Triggers using the layout/_default/downloads.html template,
                            #           rather than the layout/_default/single.html template.
listed_projects:            # Required: List of maps that defines the packages that will be
                            #           displayed. We can't assume from the project path what to use
                            #           because of e.g. Riak CS's associations with specific
                            #           Stanchion and Riak-CS-Control package version that match
                            #           neither the project nor the version of the page doing the
                            #           displaying.
  - project: "riak_cs"      # Required: Project Designation.
    version: "2.1.1"        # Required: Project Version.
    title: "Riak CS"        # Required: Rendered title text.
  - project: "Stanchion"    # Required: Another Project Designation.
    version: "1.0.12"       # Required: A different Project Version.
    title: "Something Cool" # Required: Fancy title text to render.
```

## Problematic Markdown (and Workarounds)

#### Markdown Tables
<!-- 
Added 2016/05/10 after Hipchat conversation between LR and CV - This workaround will ideally
be removed if/when the Hugo MD renderer addresses this precedence issue.  -cv
-->
**Problem**: Inserting an element with a `|` character can cause a table to render incorrectly.  
**Solution**: Replace the `|` with the HTML entity `&#124;`.  
**Note**: This workaround will not work when the `|` is inside of backticks. In that case, use the Divides character ("∣"—U+2223) in place of those instances of the pipe character.

 