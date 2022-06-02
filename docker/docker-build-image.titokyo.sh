#!/bin/bash

if test -f ./docker/Dockerfile; then
    cd ./docker
fi

docker build -t titokyo/riak_docs_generator --no-cache .
