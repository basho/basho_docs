---
title: "Taste of Riak: Object Modeling with Erlang"
project: riak
version: 1.3.1+
document: tutorials
toc: true
audience: beginner
keywords: [developers, client, 2i, search, erlang, modeling]
---

####Getting Started with the Models

To get started, let's create the records that we'll be using. 

```erlang
-define(USER_BUCKET, <<"Users">>).
-define(MSG_BUCKET, <<"Msgs">>).
-define(TIMELINE_BUCKET, <<"Timelines">>).
-define(INBOX, "Inbox").
-define(SENT, "Sent").

-record(user, {user_name, full_name, email}).

-record(msg, {sender, recipient, created, text}).

-record(timeline, {owner, msg_type, msgs}).
```

To use these records to store data, we will first have to create a user. Then, when a user creates a message, we will append that message to one or more timelines. If it's a private message, we'll append it to the Recipient's `Inbox` timeline and the User's own `Sent` timeline. If it's a group message, we'll append it to the Group's timeline, as well as to the User's `Sent` timeline.  

#### Buckets and Keys Revisited

Now that we've worked out how we will differentiate data in the system, let's figure out our bucket and key names.

The bucket names are straightforward. We can use `Users`, `Msgs`, and `Timelines`. The key names, however, are a little more tricky. In past examples we've used sequential integers, but this presents a problem: we would need a secondary service to hand out these IDs. This service could easily be a future bottleneck in the system, so let's use a natural key. Natural keys are a great fit for key/value systems because both humans and computers can easily construct them when needed, and most of the time they can be made unique enough for a KV store.


| Bucket | Key Pattern | Example Key 
|:-------|:------------|:-----------
| `Users` | `<user_name>` | `joeuser`
| `Msgs` | `<username>_<datetime>` | `joeuser_2014-03-06T02:05:13.223556Z`
| `Timelines` | `<username>_<type>_<date>` | `joeuser_Sent_2014-03-06Z`<br /> `marketing_group_Inbox_2014-03-06Z` |

For the `Users` bucket, we can be certain that we will want each username to be unique, so let's use the `username` as the key.  For the `Msgs` bucket, let's use a combination of the username and the posting datetime in an [ISO 8601 Long](http://en.wikipedia.org/wiki/ISO_8601) format. This combination gives us the pattern `<username>_<datetime>`, which produces keys like `joeuser_2014-03-05T23:20:28Z`.

Now for `Timelines`, we need to differentiate between `Inbox` and `Sent` timelines, so we can simply add that type into the key name. We will also want to partition each collection object into some time period, that way the object doesn't grow too large (see note below).

For `Timelines`, let's use the pattern `<username>_<type>_<date>` for users, and `<groupname>_Inbox_<date>` for groups, which will look like `joeuser_Sent_2014-03-06Z` or `marketing_group_Inbox_2014-03-05Z`, respectively.


<div class="note">
<div class="title">Note</div>
Riak performs best with objects under 1-2MB. Objects larger than that can hurt performance, especially many siblings are being created. We will cover siblings, sibling resolution, and sibling explosions in the next chapter.
</div>

#### Keeping our story straight with repositories

Now that we've figured out our object model, let's write some repositories to help create and work with these objects in Riak:

```erlang
%% User Operations

save_user(ClientPid, User) -> 
    RUser = riakc_obj:new(?USER_BUCKET, 
                          list_to_binary(User#user.user_name), 
                          User),
    riakc_pb_socket:put(ClientPid, RUser).

get_user(ClientPid, UserName) -> 
    {ok, RUser} = riakc_pb_socket:get(ClientPid, 
                                      ?USER_BUCKET, 
                                      list_to_binary(UserName)),
    binary_to_term(riakc_obj:get_value(RUser)).

%% Msg Operations

create_msg(Sender, Recipient, Text) ->
    #msg{sender=Sender,
         recipient=Recipient,
         created=get_current_iso_timestamp(),
         text = Text}.

%% Timeline Operations

post_msg(ClientPid, Msg) -> 
     %% Save the cannonical copy
    SavedMsg = save_msg(ClientPid, Msg),
    MsgKey = binary_to_list(riakc_obj:key(SavedMsg)),

    %% Post to sender's Sent timeline
    add_to_timeline(ClientPid, Msg, sent, MsgKey),

    %% Post to recipient's Inbox timeline
    add_to_timeline(ClientPid, Msg, inbox, MsgKey),
    ok.

get_timeline(ClientPid, Owner, MsgType, Date) -> 
    TimelineKey = generate_key(Owner, MsgType, Date),
    {ok, RTimeline} = riakc_pb_socket:get(ClientPid, 
                                          ?TIMELINE_BUCKET, 
                                          list_to_binary(TimelineKey)),
    binary_to_term(riakc_obj:get_value(RTimeline)).

%% Private Helper Functions

get_msg(ClientPid, MsgKey) -> 
    {ok, RMsg} = riakc_pb_socket:get(ClientPid, 
                                     ?MSG_BUCKET, 
                                     MsgKey),
    binary_to_term(riakc_obj:get_value(RMsg)).


save_msg(ClientPid, Msg) -> 
    MsgKey = Msg#msg.sender ++ "_" ++ Msg#msg.created,
    ExistingMsg = riakc_pb_socket:get(ClientPid, 
                                      ?MSG_BUCKET, 
                                      list_to_binary(MsgKey)),
    SavedMsg = case ExistingMsg of
        {error, notfound} ->
            NewMsg = riakc_obj:new(?MSG_BUCKET, list_to_binary(MsgKey), Msg),
            {ok, NewSaved} = riakc_pb_socket:put(ClientPid, 
                                                 NewMsg, 
                                                 [if_none_match, return_body]),
            NewSaved;
        {ok, Existing} -> Existing
    end,
    SavedMsg.

add_to_timeline(ClientPid, Msg, MsgType, MsgKey) ->
    TimelineKey = generate_key_from_msg(Msg, MsgType),
    ExistingTimeline = riakc_pb_socket:get(ClientPid, 
                                           ?TIMELINE_BUCKET, 
                                           list_to_binary(TimelineKey)),
    UpdatedTimeline = case ExistingTimeline of
        {error, notfound} ->
            create_new_timeline(Msg, MsgType, MsgKey, TimelineKey);
        {ok, Existing} -> 
            add_to_existing_timeline(Existing, MsgKey)
    end,

    {ok, SavedTimeline} = riakc_pb_socket:put(ClientPid, 
                                              UpdatedTimeline, 
                                              [return_body]),
    SavedTimeline.

create_new_timeline(Msg, MsgType, MsgKey, TimelineKey) ->
    Owner = get_owner(Msg, MsgType),
    Timeline = #timeline{owner=Owner,
                         msg_type=MsgType,
                         msgs=[MsgKey]},
    riakc_obj:new(?TIMELINE_BUCKET, list_to_binary(TimelineKey), Timeline).

add_to_existing_timeline(ExistingRiakObj, MsgKey) ->
    ExistingTimeline = binary_to_term(riakc_obj:get_value(ExistingRiakObj)),
    ExistingMsgList = ExistingTimeline#timeline.msgs,
    UpdatedTimeline = ExistingTimeline#timeline{msgs=[MsgKey|ExistingMsgList]},
    riakc_obj:update_value(ExistingRiakObj, UpdatedTimeline).

get_owner(Msg, inbox) ->  Msg#msg.recipient;
get_owner(Msg, sent) ->  Msg#msg.sender.

generate_key_from_msg(Msg, MsgType) ->
    Owner = get_owner(Msg, MsgType),
    generate_key(Owner, MsgType, Msg#msg.created).

generate_key(Owner, MsgType, Date) when is_tuple(Date) ->
    DateString = get_iso_datestamp_from_date(Date),
    generate_key(Owner, MsgType, DateString);

generate_key(Owner, MsgType, Datetimestamp) ->
    DateString = get_iso_datestamp_from_iso_timestamp(Datetimestamp),
    MsgTypeString = case MsgType of
        inbox -> ?INBOX;
        sent -> ?SENT
    end,
    Owner ++ "_" ++ MsgTypeString ++ "_" ++ DateString.

get_current_iso_timestamp() ->
    {_,_,MicroSec} = DateTime = erlang:now(),
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time(DateTime),
    lists:flatten(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~6..0B",
            [Year, Month, Day, Hour, Min, Sec, MicroSec])).

get_iso_datestamp_from_date(Date) ->
    {Year,Month,Day} = Date,
    lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day])).

get_iso_datestamp_from_iso_timestamp(CreatedString) ->
    {Date, _} = lists:split(10,CreatedString),
    Date.
```

Finally, let's test them:

```erlang
c(msgy).
rr(msgy).

%% Setup our repositories
{ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 10017).

%% Create and save users
Joe = #user{user_name="joeuser",
            full_name="Joe User",
            email="joe.user@basho.com"}.

Marleen = #user{user_name="marleenmgr",
                full_name="Marleen Manager",
                email="marleen.manager@basho.com"}.

msgy:save_user(Pid, Joe).
msgy:save_user(Pid, Marleen).

%% Create new Msg, post to timelines
Msg = msgy:create_msg(Marleen#user.user_name, Joe#user.user_name, "Welcome to the company!").
msgy:post_msg(Pid, Msg).


%% Get Joe's inbox for today, get first message
{TodaysDate,_} = calendar:now_to_universal_time(erlang:now()).
JoesInboxToday = msgy:get_timeline(Pid, Joe#user.user_name, inbox, TodaysDate).

JoesFirstMessage = msgy:get_msg(Pid, hd(JoesInboxToday#timeline.msgs)).

io:format("From: ~s~nMsg : ~s~n~n", [JoesFirstMessage#msg.sender, JoesFirstMessage#msg.text]).
```

As you can see, the repository pattern helps us with a few things:

 - It helps us to see if an object exists before creating a new one
 - It keeps our buckets and key names consistent
 - It provides us with a consistent interface to work with.

While this set of repositories solves many of our problems, it is very minimal and doesn't cover all the edge cases. For instance, what happens if two different people try to create a user with the same username?  

We can also easily "compute" key names now, but how do we quickly look up the last 10 messages a user sent? Many of these answers will be application dependent. If your application shows the last 10 messages in reverse order, for example, you may want to store that set of data in another collection object to make lookup faster. There are drawbacks to every solution, but we recommend seeking out the key/value-based solution first, as it will likely be the quickest.

So to recap, in this chapter we learned:

 - How to choose bucket names
 - How to choose natural keys based on how we want to partition our data.

In the next chapter, we will learn about siblings, how to deal with them, and some other edge cases to check for while working with Riak.
