#!/usr/bin/env escript
%% -*- erlang -*-
main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Lines = tl(re:split(Data, "\r?\n", [{return, binary},trim])),
    lists:foreach(fun(L) -> LS = re:split(L, ","), format_and_insert(LS) end, Lines).

format_and_insert(Line) ->
    JSON = io_lib:format("{\"Date\":\"~s\",\"Open\":~s,\"High\":~s,\"Low\":~s,\"Close\":~s,\"Volume\":~s,\"Adj. Close\":~s}", Line),
    Command = io_lib:format("curl -X PUT http://127.0.0.1:8091/riak/goog/~s -d '~s' -H 'content-type: application/json'", [hd(Line),JSON]),
    io:format("Inserting: ~s~n", [hd(Line)]),
    os:cmd(Command).
