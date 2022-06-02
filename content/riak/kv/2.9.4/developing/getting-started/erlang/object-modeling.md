---
title_supertext: "Getting Started:"
title: "Object Modeling with Erlang"
description: ""
project: "riak_kv"
project_version: 2.9.4
menu:
  riak_kv-2.9.4:
    name: "Object Modeling"
    identifier: "getting_started_erlang_object"
    weight: 102
    parent: "getting_started_erlang"
toc: true
aliases:
  - /riak/2.9.4/dev/taste-of-riak/object-modeling-erlang
  - /riak/kv/2.9.4/dev/taste-of-riak/object-modeling-erlang
---

To get started, let's create the records that we'll be using.

{{% note title="Code Download" %}}
You can also download the code for this chapter at
[Github](https://github.com/basho/taste-of-riak/tree/am-dem-erlang-modules/erlang/Ch03-Msgy-Schema).

The Github version includes Erlang type specifications which have been omitted
here for brevity.
{{% /note %}}


```erlang
%% msgy.hrl

-define(USER_BUCKET, <<"Users">>).
-define(MSG_BUCKET, <<"Msgs">>).
-define(TIMELINE_BUCKET, <<"Timelines">>).
-define(INBOX, "Inbox").
-define(SENT, "Sent").

-record(user, {user_name, full_name, email}).

-record(msg, {sender, recipient, created, text}).

-record(timeline, {owner, msg_type, msgs}).
```

We'll be using the bucket `Users` to store our data. We won't be [using bucket types]({{<baseurl>}}riak/kv/2.9.4/developing/usage/bucket-types) here, so we don't need to specify one.

To use these records to store data, we will first have to create a user
record. Then, when a user creates a message, we will append that message
to one or more timelines. If it's a private message, we'll append it to
the Recipient's `Inbox` timeline and to the User's own `Sent` timeline.
If it's a group message, we'll append it to the Group's timeline, as
well as to the User's `Sent` timeline.

#### Buckets and keys revisited

Now that we've worked out how we will differentiate data in the system,
let's figure out our bucket and key names.

The bucket names are straightforward. We can use `Users`, `Msgs`, and
`Timelines`. The key names, however, are a little more tricky. In past
examples we've used sequential integers, but this presents a problem: we
would need a secondary service to hand out these IDs. This service could
easily be a future bottleneck in the system, so let's use a natural key.
Natural keys are a great fit for key/value systems because both humans
and computers can easily construct them when needed, and most of the
time they can be made unique enough for a KV store.


Bucket | Key Pattern | Example Key
:------|:------------|:-----------
`Users` | `<user_name>` | `joeuser`
`Msgs` | `<username>_<datetime>` | `joeuser_2014-03-06T02:05:13.223556Z`
`Timelines` | `<username>_<type>_<date>` | `joeuser_Sent_2014-03-06Z`<br /> `marketing_group_Inbox_2014-03-06Z` |

For the `Users` bucket, we can be certain that we will want each
username to be unique, so let's use the `username` as the key.  For the
`Msgs` bucket, let's use a combination of the username and the posting
datetime in an [ISO 8601 Long](http://en.wikipedia.org/wiki/ISO_8601)
format. This combination gives us the pattern `<username>_<datetime>`,
which produces keys like `joeuser_2014-03-05T23:20:28Z`.

Now for `Timelines`, we need to differentiate between `Inbox` and `Sent`
timelines, so we can simply add that type into the key name. We will
also want to partition each collection object into some time period,
that way the object doesn't grow too large (see note below).

For `Timelines`, let's use the pattern `<username>_<type>_<date>` for
users, and `<groupname>_Inbox_<date>` for groups, which will look like
`joeuser_Sent_2014-03-06Z` or `marketing_group_Inbox_2014-03-05Z`,
respectively.

{{% note title="Note" %}}
Riak performs best with objects under 1-2 MB. Objects larger than that can
hurt performance, especially if many siblings are being created. We will cover
siblings, sibling resolution, and sibling explosions in the next chapter.
{{% /note %}}

#### Keeping our story straight with repositories

Now that we've figured out our object model, let's write some modules to
act as repositories that will help us create and work with these records
in Riak:

```erlang
%% user_repository.erl

-module(user_repository).
-export([save_user/2,
         get_user/2]).
-include("msgy.hrl").

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
```

<br />

```erlang
%% msg_repository.erl

-module(msg_repository).
-export([create_msg/3,
         get_msg/2]).
-include("msgy.hrl").

-spec create_msg(user_name(), user_name(), text()) -> msg().
create_msg(Sender, Recipient, Text) ->
    #msg{sender=Sender,
         recipient=Recipient,
         created=get_current_iso_timestamp(),
         text = Text}.

-spec get_msg(pid(), riakc_obj:key()) -> msg().
get_msg(ClientPid, MsgKey) ->
    {ok, RMsg} = riakc_pb_socket:get(ClientPid,
                                     ?MSG_BUCKET,
                                     MsgKey),
    binary_to_term(riakc_obj:get_value(RMsg)).

%% @private
-spec get_current_iso_timestamp() -> datetimestamp().
get_current_iso_timestamp() ->
    {_,_,MicroSec} = DateTime = erlang:now(),
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time(DateTime),
    lists:flatten(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~6..0B",
            [Year, Month, Day, Hour, Min, Sec, MicroSec])).

```

<br />

```erlang
%% timeline_repository.erl

-module(timeline_repository).
-export([post_msg/2,
         get_timeline/4]).
-include("msgy.hrl").

post_msg(ClientPid, Msg) ->
     %% Save the canonical copy
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

%% --------------------------------------------------------------------

%% @private
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

%% @private
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

%% @private
create_new_timeline(Msg, MsgType, MsgKey, TimelineKey) ->
    Owner = get_owner(Msg, MsgType),
    Timeline = #timeline{owner=Owner,
                         msg_type=MsgType,
                         msgs=[MsgKey]},
    riakc_obj:new(?TIMELINE_BUCKET, list_to_binary(TimelineKey), Timeline).

%% @private
add_to_existing_timeline(ExistingRiakObj, MsgKey) ->
    ExistingTimeline = binary_to_term(riakc_obj:get_value(ExistingRiakObj)),
    ExistingMsgList = ExistingTimeline#timeline.msgs,
    UpdatedTimeline = ExistingTimeline#timeline{msgs=[MsgKey|ExistingMsgList]},
    riakc_obj:update_value(ExistingRiakObj, UpdatedTimeline).

%% @private
get_owner(Msg, inbox) ->  Msg#msg.recipient;
get_owner(Msg, sent) ->  Msg#msg.sender.

%% @private
generate_key_from_msg(Msg, MsgType) ->
    Owner = get_owner(Msg, MsgType),
    generate_key(Owner, MsgType, Msg#msg.created).

%% @private
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

%% @private
get_iso_datestamp_from_date(Date) ->
    {Year,Month,Day} = Date,
    lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day])).

%% @private
get_iso_datestamp_from_iso_timestamp(CreatedString) ->
    {Date, _} = lists:split(10,CreatedString),
    Date.

```

Finally, let's test them:

```erlang
%% msgy.erl

-module(msgy).
-export([main/0]).
-include("msgy.hrl").

main() ->
  %% Setup our repositories
  {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 10017),

  %% Create and save users
  Joe = #user{user_name="joeuser",
              full_name="Joe User",
              email="joe.user@basho.com"},

  Marleen = #user{user_name="marleenmgr",
                  full_name="Marleen Manager",
                  email="marleen.manager@basho.com"},

  user_repository:save_user(Pid, Joe),
  user_repository:save_user(Pid, Marleen),

  %% Create new Msg, post to timelines
  Msg = msg_repository:create_msg(Marleen#user.user_name, Joe#user.user_name, "Welcome to the company!"),
  timeline_repository:post_msg(Pid, Msg),


  %% Get Joe's inbox for today, get first message
  {TodaysDate,_} = calendar:now_to_universal_time(erlang:now()),
  JoesInboxToday = timeline_repository:get_timeline(Pid, Joe#user.user_name, inbox, TodaysDate),

  JoesFirstMessage = msg_repository:get_msg(Pid, hd(JoesInboxToday#timeline.msgs)),

  io:format("From: ~s~nMsg : ~s~n~n", [JoesFirstMessage#msg.sender, JoesFirstMessage#msg.text]),
  ok.
```

As you can see, the repository pattern helps us with a few things:

* It helps us to see if an object exists before creating a new one
* It keeps our buckets and key names consistent
* It provides us with a consistent interface to work with.

While this set of repositories solves many of our problems, it is very
minimal and doesn't cover all the edge cases. For instance, what happens
if two different people try to create a user with the same username?

We can also easily "compute" key names now, but how do we quickly look
up the last 10 messages a user sent? Many of these answers will be
application dependent. If your application shows the last 10 messages in
reverse order, for example, you may want to store that set of data in
another collection object to make lookup faster. There are drawbacks to
every solution, but we recommend seeking out the key/value-based
solution first, as it will likely be the quickest.

So to recap, in this chapter we learned:

* How to choose bucket names
* How to choose natural keys based on how we want to partition our data.


