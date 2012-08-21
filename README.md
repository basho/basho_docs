# Riak Docs 2.0

*It's been a long time a-comin', baby...*

## Develoment Mode

Easy:

```
gem install bundler
bundle install
bundle exec middleman server
```

## Testing deploy mode

To try out the thin server in the way production functions, first build the static files:

```
bundle exec middleman build
```

Then you can run the thin server locally:

```
bundle exec thin start -p 3000
```

To have MiddleMan auto-build on each save, we use the Watchr gem. It will auto-build on each save in the source directory:

```
bundle exec watchr ./Watchrfile
```

## Deploy to Heroku

The docs are pushed to heroku, just so we have something to look at and share.

**Keep it secret. Keep it safe.**
