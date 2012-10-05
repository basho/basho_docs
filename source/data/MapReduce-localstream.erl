-module(localstream).

-export([load/1, keys/0, bucket/0, map/3]).

-define(QUERY, [{map, {modfun, localstream, map}, none, true}]).

load(HowMany) ->
    {ok, Client} = riak:local_client(),
    lists:foreach(fun(Index) ->
        BIndex = list_to_binary(integer_to_list(Index)),
        RObj   = riak_object:new(<<"examples">>, <<"key",BIndex/binary>>, <<"Value ",BIndex/binary>>),
        Client:put(RObj)
    end, lists:seq(1, HowMany)).

keys() ->
    {ok, Client} = riak:local_client(),
    {ok, {_ReqPid, FlowPid}} = Client:mapred_stream(?QUERY, self()),
    %% more than 100 inputs should be added in multiple calls to %luke_flow:add_inputs
    luke_flow:add_inputs(FlowPid, [{<<"examples">>, <<"key1">>},
                                   {<<"examples">>, <<"key2">>},
                                   {<<"examples">>, <<"key3">>}]),
    luke_flow:finish_inputs(FlowPid),
    loop().


bucket() ->
    {ok, Client} = riak:local_client(),
    Client:mapred_bucket_stream(<<"examples">>, ?QUERY, self()),
    loop().

%% note: riak_object:get_value will error if Value has siblings
map(Value, _KeyData, _Arg) ->
    [iolist_to_binary([riak_object:bucket(Value),
                       "/",
                       riak_object:key(Value),
                       " => ",
                       riak_object:get_value(Value)])].


%% @private

loop() ->
    receive
        {flow_results,_,done} ->
            ok;
        {flow_error,_,Error} ->
            io:format("Something bad happend! (~p~n)", [Error]);
        {flow_results,_,_,Results} ->
            io:format("Streaming Results: ~p~n", [Results]),
            loop()
    end.
