# Riak Docs 2.0

*It's been a long time a-comin', baby...*

## Develoment Mode

Easy:

```
gem install bundler
bundle install
bundle exec middleman server
```

Optionally, run with specifying a version of the docs to generate

```
RIAK_VERSION=1.2.0 bundle exec middleman server
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

## Deploy to S3

To deploy you must specify a riak version

```
RIAK_VERSION=1.2.0 middleman build
```

**Keep it secret. Keep it safe.**
