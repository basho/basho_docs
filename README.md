
[basho docs]: http://docs.basho.com/
[task list]: https://github.com/basho/private_basho_docs/issues/11
[middleman]: https://middlemanapp.com/
[rvm]: https://rvm.io/

# Basho's Documentation Generation

This repository contains all the bits and pieces, large and small required to render and deploy
### http://docs.basho.com/

This is a Work In Progress! It's the current (and continually growing) crop of our [continuing transition work][task list] from a Middleman based system to a Hugo based system.
Please let us know if you'd like to help out!


## Building The HTML Locally

1. **Install [Hugo][hugo]**
    Check out [Hugo's Installing][installing hugo] page.

1. **Clone the Repository**
    `git clone https://github.com/basho/private_basho_docs.git`
    `cd private_basho_docs`
    `git checkout hugo/testing`

1. **Run Hugo**
    `hugo server`

1. **Play**
    http://localhost:1313

[hugo]: http://gohugo.io/
[installing hugo]: http://gohugo.io/overview/installing/
[homebrew]: http://brew.sh/

#### No Really, _Go_ Play
<sub>That was a pun. Did you catch it? "Go"? The Go language? That Hugo uses? I'm bad at puns.</sub>

At this point, any changes you make to the markdown files in the `content/` directory will be automatically detected and rendered live in your local browser. Change some stuff! Have fun!

If you want to modify the [content templates][hugo content templates] that define how each pages' HTML is generated, modifying the [Go Templates][hugo go template primer] in `layouts/_default/` and the [partial templates][hugo partial templates] in `layouts/partials/` will also be automatically detected and rendered live in your browser.

**TODO**: Drop a link to the list of shortcodes we (don't currently) have.

[hugo content templates]: https://gohugo.io/templates/content/
[hugo go template primer]: https://gohugo.io/templates/go-templates/
[hugo partial templates]: https://gohugo.io/templates/partials/
[hugo shortcodes]: https://gohugo.io/extras/shortcodes/


## Modifying the `.js` and `.css` Files

If you want to mess with the scripts and CSS that this site uses, it's not _quite_ as easy as modifying the HTML.

The scripts and CSS files used to render Hugo content are expected to live in the `static/` directory. We use a lot of [Coffee Script][coffee] and [Sass][sass] for our scripting and styling needs, and we convert those files to `.js` and `.css` as a pre-render step. Because we're clever, we put those `.coffee` and `.scss` files into the `dynamic/` directory. <sub>(we're not really that clever. We try, though.)</sub>

To convert the Coffee and Sass into `.js` and `.css` files, you'll need to...

1. **Install [RVM][rvm]**
    or equivalent.
    <sub>You might need to restart your shell to get the `rvm` command to be recognized.</sub>

1. **Install Ruby version 2.2.3**
    `rvm install ruby-2.2.3`
    or the current version specified in the .ruby-version and Gemfile files.
    <sub>If it doesn't match, please [open a new issue][new issue] and let us know this README isn't up to date!</sub>

1. **Install [Bundler] With The ruby-2.2.3 Gem**
    `gem install bundler`

1. **Install the Rest of the Dependencies**
    `bundle install`

1. **Use [Rake] to do Everything Else**
    Like rebuild a copy of everything that should live in `static/`.
    `rake build`

    Or output a more debug-friendly version of everything?
    `rake build:debug`

    Or maybe you want any changes you make to `.coffee` and `.scss` files to be automatically detected, and rendered live in your browser?
    `rake watch`

    For a list of some of the useful commands, just run,
    `rake`

[coffee]: coffeescript.org
[sass]: http://sass-lang.com/
[rvm]: https://rvm.io/
[bundler]: http://bundler.io/
[rake]: http://docs.seattlerb.org/rake/


## Would You Like to Contribute?

Awesome! <sub>(We're assuming you said yes. Because you're reading this. And you're _awesome_.)</sub>

This repository operates just like any other open source repo, and only thrives through the efforts of everyone who contributes to it. If you see something wrong, something that could be improved, or something that's simply missing please don't hesitate to...

* **Open Up a [New Issue]**
    and let us know what you think should change.

* **[Find the File] You Want to Change**
    and use GitHub's online editor to open a Pull Request right here.

* **[Fork] This Repository**
    so you can make (and see) your changes locally.

Don't forget to check out our [Contributing Guidelines][contributing] so you can read up on all our weird little quirks, like how we [don't want you to use `<h1>` headers][contributing_headers].

**TODO**: The above links all point to private_basho_docs and hugo/testing. Once (or before) we're ready to cut these over, we should modify the links s.t. they point to the right place.

[new issue]: https://github.com/basho/private_basho_docs/issues/new
[find the file]: https://github.com/basho/private_basho_docs/find/hugo/testing
[fork]: https://github.com/basho/private_basho_docs/#fork-destination-box
[contributing]: https://github.com/404
[contributing_headers]: https://github.com/404
