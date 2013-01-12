---
title: Backend API
project: riak
version: 1.0.0+
document: appendix
toc: true
keywords: [api, backends]
---

As of the 1.0 release, Riak's storage API has been overhauled and
uniformly applied to all of the
[[supported backends|Storage-Backends]]. This page presents the
details of the storage backend API in the form of
[Erlang type specifications](http://www.erlang.org/doc/reference_manual/typespec.html)
(specs). Specs are used by
[dialyzer](http://www.erlang.org/doc/man/dialyzer.html), an Erlang
static analysis tool. It is recommended to copy these specs into any
custom backend modules and use them as a guide for development to
avoid errors and ensure full compatibility with Riak. Also included
below is the function export list that can be pasted directly into a
custom storage backend module.

```erlang
%% Riak Storage Backend API
-export([api_version/0,
         start/2,
         stop/1,
         get/3,
         put/5,
         delete/4,
         drop/1,
         fold_buckets/4,
         fold_keys/4,
         fold_objects/4,
         is_empty/1,
         status/1,
         callback/3]).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Return the major version of the
%% current API and a capabilities list.
%% The current valid capabilities are async_fold
%% and indexes.
-spec api_version() -> {integer(), [atom()]}.

%% @doc Start the backend
-spec start(integer(), config()) -> {ok, state()} | {error, term()}.

%% @doc Stop the backend
-spec stop(state()) -> ok.

%% @doc Retrieve an object from the backend
-spec get(riak_object:bucket(), riak_object:key(), state()) ->
                 {ok, any(), state()} |
                 {ok, not_found, state()} |
                 {error, term(), state()}.

%% @doc Insert an object into the backend.
-type index_spec() :: {add, Index, SecondaryKey} | {remove, Index, SecondaryKey}.
-spec put(riak_object:bucket(), riak_object:key(), [index_spec()], binary(), state()) ->
                 {ok, state()} |
                 {error, term(), state()}.

%% @doc Delete an object from the backend
-spec delete(riak_object:bucket(), riak_object:key(), [index_spec()], state()) ->
                    {ok, state()} |
                    {error, term(), state()}.

%% @doc Fold over all the buckets
-spec fold_buckets(riak_kv_backend:fold_buckets_fun(),
                   any(),
                   [],
                   state()) -> {ok, any()} | {async, fun()}.

%% @doc Fold over all the keys for one or all buckets.
-spec fold_keys(riak_kv_backend:fold_keys_fun(),
                any(),
                [{atom(), term()}],
                state()) -> {ok, term()} | {async, fun()}.

%% @doc Fold over all the objects for one or all buckets.
-spec fold_objects(riak_kv_backend:fold_objects_fun(),
                   any(),
                   [{atom(), term()}],
                   state()) -> {ok, any()} | {async, fun()}.

%% @doc Delete all objects from this backend
%% and return a fresh reference.
-spec drop(state()) -> {ok, state()} | {error, term(), state()}.

%% @doc Returns true if this backend contains any
%% non-tombstone values; otherwise returns false.
-spec is_empty(state()) -> boolean() | {error, term()}.

%% @doc Get the status information for this backend
-spec status(state()) -> [{atom(), term()}].

%% @doc Register an asynchronous callback
-spec callback(reference(), any(), state()) -> {ok, state()}.
```

