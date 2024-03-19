#!/usr/bin/env sh

# Based on https://github.com/jojomi/docker-hugo/blob/0.25.1/run.sh

RAKE_DEBUG="${RAKE_DEBUG:=false}"
RAKE_GEN_DOWNS="${RAKE_GEN_DOWNS:=false}"
RAKE_GEN_PROJECTS="${RAKE_GEN_PROJECTS:=true}"
WATCH="${HUGO_WATCH:=false}"
SLEEP="${HUGO_REFRESH_TIME:=-1}"
HUGO_PORT="${HUGO_PORT:=1314}"

echo "RAKE_DEBUG:"        $RAKE_DEBUG
echo "RAKE_GEN_DOWNS:"    $RAKE_GEN_DOWNS
echo "RAKE_GEN_PROJECTS:" $RAKE_GEN_PROJECTS
echo "HUGO_WATCH:"        $WATCH
echo "HUGO_REFRESH_TIME:" $HUGO_REFRESH_TIME
echo "HUGO_THEME:"        $HUGO_THEME
echo "HUGO_BASEURL:"      $HUGO_BASEURL
echo "HUGO_PORT:"         $HUGO_PORT
echo "ARGS"               $@

bundle update --bundler
bundle install

if [[ "$RAKE_GEN_DOWNS" != "false" ]]; then
    rake metadata:generate_downloads
fi

HUGO=/usr/local/sbin/hugo
echo "Hugo path: $HUGO"

while [ true ]
do
    if [[ "$RAKE_DEBUG" != "false" ]]; then
        rake build:debug
    else
        rake build
    fi
    if [[ "$RAKE_GEN_PROJECTS" != "false" ]]; then
        rake metadata:generate_projects
    fi

    if [[ $HUGO_WATCH != 'false' ]]; then
	    echo "Watching..."
        # add for logging: --log --logFile "/output/log.txt" --verboseLog
        rake watch
        $HUGO server --watch=true --source="/src" --theme="$HUGO_THEME" --destination="/output" --baseURL="$HUGO_BASEURL" --bind="0.0.0.0" "$@" -p $HUGO_PORT || exit 1
    else
	    echo "Building one time..."
        $HUGO --source="/src" --theme="$HUGO_THEME" --destination="/output" --baseURL="$HUGO_BASEURL" "$@" || exit 1
    fi

    if [[ $HUGO_REFRESH_TIME == -1 ]]; then
        exit 0
    fi
    echo "Sleeping for $HUGO_REFRESH_TIME seconds..."
    sleep $SLEEP
done

