# Style, Baby

We're using SCSS as our source-to-source language because it is a strict super-
set of CSS, but why would you ever want to write CSS anymore?

We're using a modified [Sass 7-1 Pattern] for organization. All of the .scss
files at the root of this subdirectory will be converted to .css files as part
of our [SCSS compilation and post-processing] build phase. All other files
should be Sass Partials (named with a leading '_', e.g. `_base.scss`), and
included in these root-files.

Each of these subdirectories will include an additional README that should
include information about what kinds of styles should be include in each
section.

[Sass 7-1 Pattern]: http://sass-guidelin.es/#the-7-1-pattern
[SCSS compilation and post-processing]: https://github.com/basho/basho_docs/blob/master/rake_libs/compile_css.rb