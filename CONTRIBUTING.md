## Front Matter

The front matter that starts off every content file is incredibly important. The
metadata described by front matter sections is used by Hugo at build time and
JavaScript at load time to correctly format and generate our site. If there are
syntactic or semantic errors in any page's front matter, it may negatively
impact the look of the entire site.

This section will detail (with perhaps painful verbosity) the format and
function of front matter elements.

There are future plans to build a front-matter-linter that would be included in
the `rake hugo` task so we can nip certain issues in the bud. We don't have that
yet, though, and we're not sure when that might happen. For now, please be
careful when writing new pages or modifying the front matter/location of
existing ones.

#### Primary Content Pages

This is the default layout for pages. Anything that is not the splash page
(which already uses a special layout), a downloads page (which requires an
expanded set of configurations; see below), or a page in the community section
(which requires a reduced set of these configurations; see below), is probably
a primary content page.

<!-- TODO: Link to a description of the directory layout, which should talk
           about how Hugo layouts are selected. -->

```
# We're using YAML for the front matter, so we need to indicate to Hugo the
# start and end with `---`
---
title_supertext: "Super-text title"            # 1.ii  -- Optional
title: "Page Title"                            # 1.i
description: "Page description"                # 2
project: "riak_kv"                             # 3
project_version: "2.1.4"                       # 4
menu:                                          # 5
  riak_kv-2.1.4:                               # 5.i
    name: "Abbreviated Title"                  # 5.ii
    identifier: "identifier"                   # 5.iii
    weight: 309                                # 5.iv
    parent: "parent_identifier"                # 5.v   -- Conditional
    pre: font-icon                             # 5.vi  -- Conditional
toc: true                                      # 6     -- Optional
commercial_offering: false                     # 7     -- Optional
version_history:                               # 8     -- Optional
  in: "1.3.0-2.99.99"                          # 8.i   -- Optional
  locations:                                   # 8.ii  -- Optional
    - ["<1.5.0",      "using/. . ."]           #       -- Optional
    - ["1.5.2-1.9.9", "using/. . ."]           #       -- Optional
    - ["2.0.0+"     , "using/. . ."]           #       -- Optional
aliases:                                       # 9     -- Optional
    - /riakts/1.3.1/. . .                      #       -- Optional
canonical_link: "https://docs.basho.com/. . ." # 10    -- Conditional
---
```

1. The Page Title is the title of the page, in the context of both the
    &lt;head>&lt;title> element and as rendered on the page.

    The rendered title will be placed above the primary content (above the text
    that makes up the markdown document) as the only &lt;h1> element on the
    page. If the Title Super-Text (1.b, `title_supertext`) is present, it will
    be rendered above the Page Title, using a slightly reduced font.

2. The Description should be a quick, single-sentence summary of the page.

    It will be used in a `<meta name="description">` tag for rich content
    summaries and potentially as the title text that appears when hovering over
    menu item elements.

3. The Project Designation that this page is associated with.

    In the case of pages located under riak/kv/ and riak/ts, the designation
    will use underscores, ex. `riak_kv`, and `riak_ts`.

4. The Project Version that this page is associated with.

5. "Hugo has a simple yet powerful menu system that permits content to be
    placed in menus with a good degree of control without a lot of work."

    https://gohugo.io/extras/menus

    Our implementation of Hugo menus requires adherence to a few additional
    conventions detailed below. For general information, please see the
    documentation linked above

    1.  The name of the menu for the page.

        This name __must be__ `"${project}-${project_version}"`. The content
        navigation generation logic assumes this to be true and will not be able
        to correctly render our side bar if this is not the case.

    2.  Abbreviated name of the page.

        This text will be rendered as the page title in the content navigation
        menu, and so length should be considered.

    3.  Unique page identifier.

        Hugo menus use these `identifier`s internally, and will assume that each
        `identifier` is unique _to the menu_. Every version of the `riak_ts`
        project can include the `using` identifier -- as each version builds its
        own menu -- but each menu **must not** include duplicate entries.

        If duplicate entries are present in a menu, errors will be thrown at
        Hugo build time.

    4.  The ordering weight of the page, relative to pages sharing a `parent`.

        Order is ascending, which means the lowest (lightest) weight will be at
        the top, and the highest (heaviest) will sink to the bottom. In case of
        ties, alphabetical sorting of page `identifier` is used.

        **Note**: Reordering of pages should only happen when new pages are
        added to a given section. If you feel the ordering of a section could be
        improved, please reach out to the Docs team by opening an issue (not a
        PR, please) in this repository.

    5.  The `identifier` of the the parent the page (if one exits).

        If the page is a top-level element, the `parent` field should be
        excluded, and a `pre` should be defined instead.

    6.  The name of the font-icon that should be rendered as part of the menu
        item's title

        If the page is not a top-level element -- if it has a `parent` set --
        this field should be excluded, as it will be ignored.

        <!-- TODO: Build and link to a reference page that shows off all icons -->

6. Whether or not a Table of Contents should be rendered.

    Defaults to `true`.

7. Whether or not the 'Enterprise Only Content' flag should be rendered.

    Defaults to `false`.

8. The `version_history` element is optional, but should be included on all
    pages that do not exist, or do not exist in the same location, in every
    version of a given project.

    1.  The `version_history.in` Version Range should be included for pages that
        do not exist in every version.

        For pages that were added after the first published version of a
        project, a range similar to `>=1.2.0` or `1.2.0+` could be used. For
        pages that were removed at a certain version, a range similar to
        `<3.0.0` or `2.99.99-` (note that the `-` is inclusive, and equivalent
        to `<=`) could be used. An explicit range can be used for pages that
        were added late, and then removed, ex;

        ```
        version_history:
          in: "1.2.0-2.99.99"
        ```

        For pages that apply to non-contiguous versions, specify `in` as an array:

        ```
        version_history:
          in: 
            - "1.2.0-2.99.99"
            - "3.2.0+"
        ```

        > For more information regarding Version Ranges, please see the
        > `ParseRange` function in js/tools/sem_ver.coffee.

    2.  If a page is moved on disk between versions, the
        `version_history.locations` element should be used.

        The `locations` element should be a list of tuples (list with length
        2), each tuple being made up of a Version Range followed by the project
        /version-relative path to the page for the given Range. It will be
        assumed that any valid versions the `locations` list does not cover will
        follow the same path as the page being rendered.

        **PLEASE BE AWARE; the first range that matches the given version will
        be used, and all following locations will not be checked.** If the first
        version listed masks all others (ex; `<=99.99.99`), its path will be the
        only path used.

        (For more information regarding Version Ranges, please see the
        `ParseRange` function in js/tools/sem_ver.coffee.)

        **NOTE**: _Every_ version of the page should include a version of the
        `locations` list.

        > For example, the Riak TS page `using/arithmetic-operations.md` is
        > being moved in version 1.4.0 to `querying/arithmetic-operations`. The
        > older version of the page should be,
        >
        > ```
        > version_history
        >   locations:
        >     - [">=1.4.0", "querying/arithmetic-operations"]
        > ```
        >
        > and the newer version of the page should be,
        >
        > ```
        > version_history
        >   locations:
        >     - ["<1.4.0", "using/arithmetic-operations"]
        > ```
        >
        > Alternatively, both versions could be,
        >
        > ```
        > version_history
        >   locations:
        >     - [">=1.4.0", "querying/arithmetic-operations"]
        >     - ["<1.4.0",  "using/arithmetic-operations"]
        > ```

9. The canonical URI for the page

    If this element is excluded, the canonical URI will default to
    `${project_path}/latest/${project_relative_path}`. **If this is
    not what the canonical link should be, this element is required**.

    This is especially important to look out for when moving pages between
    versions. **The most recent location of any given page should be its
    canonical link.**

10. "Redirects can be handled easily with aliases in Hugo."

    https://gohugo.io/extras/aliases/

    For every alias given, Hugo will automatically generate an HTML document
    whose primary task is to trigger a &lt;meta http-equiv="refresh"> to
    redirect users to more recent content.

    We primarily leverage aliases to redirect from archived content to more
    current pages.

#### Downloads Pages

For Downloads pages -- those that present automatically generated links to
product packages -- there are a few front matter elements that must be included
**in addition** to the elements detailed above.

```
---
. . .
layout: downloads           # 1
listed_projects:            # 2
  - project: "riak_cs"      # 2.i
    version: "2.1.1"        # 2.ii
    title: "Riak CS"        # 2.iii
  - project: "Stanchion"
    version: "1.0.12"
    title: "Something Cool"
---
```

11. Triggers the use of the `layout/_default/downloads.html` layout, rather than
    the default `layout/_default/single.html` template.

12. The `listed_projects` element is a list of maps that define the packages
    that will be displayed, and how they will be presented. We can't assume from
    the project path and version what to present because of e.g. Riak CS's
    associations with specific Stanchion and Riak-CS-Control versions that match
    neither the project path nor version of the page doing the displaying.

    1.  The Project Designation of the packages to present.

    2.  The version of the given project to present.

    3.  The rendered title of the given project.

        This does not need to match any other data, and as such should be
        descriptive, rather than technical.

#### Community Pages

Pages in the Community section of content are not versioned in the same way as
primary content pages, and thus must include a reduced set of front matter
elements.

Please see the above sections for descriptions of un-annotated elements.

```
---
title_supertext: "Super-text title"
title: "Page Title"
description: "Page description"
menu:
  community:                                   # 1
    name: "Abbreviated Title"
    identifier: "identifier"
    weight: 309
    parent: "parent_identifier"
    pre: font-icon
toc: true
commercial_offering: false
aliases:
    - /riakts/1.3.1/. . .
canonical_link: "https://docs.basho.com/. . ." # 2 -- OPTIONAL
---
```

1. The `menu` map of a Community page should be treated similarly to the `menu`
    of a primary content page, save for menu name. Because there is no
    `project_version` in the Community front matter, the name of the menu **must
    be** `community`.

2. Default canonical links for Community pages are slightly different from and
    simpler than primary content pages; `community/${project_relative_path}`.
    Again, this element is entirely optional.

## Problematic Markdown (and Workarounds)

#### Markdown Tables

<!--
Added 2016/05/10 after Hipchat conversation between LR and CV - This workaround will ideally
be removed if/when the Hugo MD renderer addresses this precedence issue.  -cv
-->
**Problem**: Inserting an element with a `|` character can cause a table to render incorrectly.
**Solution**: Replace the `|` with the HTML entity `&#124;`.
**Note**: This workaround will not work when the `|` is inside of backticks. In
that case, use the Divides character ("∣"—U+2223) in place of those instances of the pipe character.
