-module(pbstream).

-export([load/1, keys/0, bucket/0, map/3, pb_link/0, pb_link/2]).

-define(QUERY, [{map, {modfun, riak_kv_mapreduce, map_object_value}, <<"filter_notfound">>, true}]).

load(HowMany) ->
    lists:foreach(fun(Index) ->
        BIndex = list_to_binary(integer_to_list(Index)),
        RObj   = riakc_obj:new(<<"examples">>, <<"key",BIndex/binary>>, <<"Value ",BIndex/binary>>),
        riakc_pb_socket:put(pb_link(), RObj)
    end, lists:seq(1, HowMany)).

keys() ->
    Inputs = [{<<"examples">>, <<"key1">>},
              {<<"examples">>, <<"key2">>},
              {<<"examples">>, <<"key3">>}],
    riakc_pb_socket:mapred_stream(pb_link(), Inputs, ?QUERY, self()),
    loop().

bucket() ->
    riakc_pb_socket:mapred_bucket_stream(pb_link(), <<"examples">>, ?QUERY, self(), 60000),
    loop().

pb_link() ->
    pb_link("127.0.0.1", 8087).

pb_link(Host, Port) ->
    case get(pb_link) of
        undefined ->
            put(pb_link, riakc_pb_socket:start_link(Host, Port)),
            pb_link(Host, Port);
        {ok, Client} ->
            Client;
        Error ->
            io:format("Error linking to pb socket ~p~n", [Error]),
            {pb_link_error, Error}
    end.

%% @private

loop() ->
    receive
        {_ReqId, done} ->
            ok;
        {_ReqId, {mapred,_Phase,Results}} ->
            io:format("Streaming Results: ~p~n", [Results]),
            loop();
        {_ReqId, {error, Reason}} ->
            io:format("Something bad happened! ~p~n", [Reason])
    end.
