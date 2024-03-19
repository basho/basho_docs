JavaScript and Friends
======================

This directory should contain all of the code necessary to run JS code on the
docs site, including vendor libraries, home-grown tools, and site-wide / per-
page scripts.

This code is designed to be compiled using the [Sprockets][sprockets] tool set.
Vendor code should be included in an uncompressed form to aid in debugging ---
the Sprockets compile will minify it and our distribution network will compress
it. Our code can be written in either JavaScript or CoffeeScript, though Coffee
is preferred for its consistency and because each .coffee file is wrapped in an
Immediately Invoked Function Expression. If you write straight JS, I ask that
you wrap each file in an IIFE for consistency's and safety's sake.

For details on how the build system is invoked, see
[rake_libs/compile_js.rb][rake_libs/compile_js.rb].

The .js file(s) in this directory should contain little to no actual code; they
should leverage Sprocket's [asset bundles][sprockets_assets] to aggregate
related code into a single file that can then be minified, compressed, and
served.

For now, we're only generating the main.js file that contains all of the code
(dependencies included) required to perform DOM manipulations and site
interactivity. This file *must* be included immediately before the closing
</body> tag, as it is designed to edit elements before the DOM is initially
rendered.

[sprockets]: https://github.com/rails/sprockets
[sprockets_assets]: https://github.com/rails/sprockets#managing-and-bundling-dependencies

Files and Stuff
---------------

    dynamic/js
    ├── main.js -- This file *must* be included at the bottom of the <body> HTML
    │              element. It will include all of the JS required for DOM
    │              modifications and page interactions.
    ├── vendor
    │   ├── modernizr-3.1.1.js
    │   ├── jquery-2.2.4.js   -- JQuery because JQuery. Version 2.2.4 because
    │   │                        Bootstrap requires < version 3.0.0.
    │   ├── bootstrap-v4...js -- Bootstrap because Bootstrap.
    │   │                        FIXME: When Bootstrap v4 is released, we should
    │   │                        upgrade this package (and the CSS)
    │   ├── tether-1.1.0.js -- Required by Bootstrap.
    │   └── highlight-9.3.0.pack.js
    └── basho
        ├── tools
        │   ├── *.coffee -- Tooling that can be separated from per-component
        │   │               generation or interaction. Should generally add an
        │   │               Object to the `window` object that can then be used
        │   └               when and where necessary.
        │
        └── *.coffee -- Per-component generation and/or interaction logic.
