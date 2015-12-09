
[basho docs]: http://docs.basho.com/
[task list]: https://github.com/basho/private_basho_docs/issues/11
[middleman]: https://middlemanapp.com/
[hugo]: http://gohugo.io/
[install hugo]: http://gohugo.io/overview/installing/
[rvm]: https://rvm.io/

# hugo/testing

The primary branch for the on-going [Basho Docs][basho docs] conversion from [Middleman][middleman] to [Hugo][hugo].

[Task List][task list]

## Getting Started

* `git clone https://github.com/basho/private_basho_docs.git`
* Switch to `hugo/testing` branch
* [Install Hugo 0.15][install hugo] (`brew install hugo`)
* [Install RVM][rvm] & Ruby v2.2.3
* `gem install bundler`
* `bundle install`
* `rake watch` (to compile assets)
* `hugo server`
