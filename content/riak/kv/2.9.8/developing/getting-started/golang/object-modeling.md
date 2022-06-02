---
title_supertext: "Getting Started:"
title: "Object Modeling with Go"
description: ""
project: "riak_kv"
project_version: 2.9.8
menu:
  riak_kv-2.9.8:
    name: "Object Modeling"
    identifier: "getting_started_go_object"
    weight: 102
    parent: "getting_started_go"
toc: true
aliases:
  - /riak/2.9.8/dev/taste-of-riak/object-modeling-golang
  - /riak/kv/2.9.8/dev/taste-of-riak/object-modeling-golang
---

{{% note title="Code Download" %}}
You can download the code for this chapter at
[Github](https://github.com/basho/taste-of-riak/tree/master/go/ch03/models).
{{% /note %}}

To get started, let's create the models that we'll be using:

```model.go
package models

type Model interface {
  GetId() string
  SetId(id string)
}

type modelImpl struct {
  id string
}

func (m *modelImpl) SetId(id string) {
  m.id = id
}
```

Our user model:

```user.go
package models

type User struct {
  modelImpl
  UserName string
  FullName string
  Email    string
}

func NewUser(userName, fullName, email string) *User {
  u := &User{
    UserName: userName,
    FullName: fullName,
    Email:    email,
  }
  u.SetId(userName)
  return u
}

func (u *User) GetId() string {
  return u.UserName
}
```

And our message model:

```msg.go
package models

import (
  "fmt"
  "time"

  util "github.com/basho/taste-of-riak/go/util"
)

type Msg struct {
  modelImpl
  Sender    string
  Recipient string
  Text      string
  Created   time.Time
}

func NewMsg(sender, recipient, text string) *Msg {
  m := &Msg{
    Sender:    sender,
    Recipient: recipient,
    Text:      text,
    Created:   time.Now(),
  }
  m.SetId(m.GetId())
  return m
}

func (m *Msg) GetId() string {
  return fmt.Sprintf("%s_%v", m.Sender, util.Iso8601(m.Created))
}
```

Our timeline model:

```timeline.go
package models

type Timeline struct {
  modelImpl
  MsgKeys []string
}

type TimelineType byte

const (
  TimelineType_INBOX TimelineType = iota
  TimelineType_SENT
)

func NewTimeline(id string) *Timeline {
  t := &Timeline{}
  t.id = id
  return t
}

func (t *Timeline) AddMsg(msgKey string) {
  t.MsgKeys = append(t.MsgKeys, msgKey)
}

func (t *Timeline) GetId() string {
  return t.id
}
````

We'll be using the bucket `Users` to store our data. We won't be [using bucket types]({{<baseurl>}}riak/kv/2.9.8/developing/usage/bucket-types) here, so we don't need to specify one.

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
`Timelines`. The key names, however, are a little trickier. In past
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
users and `<groupname>_Inbox_<date>` for groups, which will look like
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

```repository.go
package repositories

import (
  "encoding/json"
  "errors"

  riak "github.com/basho/riak-go-client"
  models "github.com/basho/taste-of-riak/go/ch03/models"
)

var ErrUnexpectedSiblings = errors.New("Unexpected siblings in response!")

type Repository interface {
  Get(key string, notFoundOk bool) (models.Model, error)
  Save(models.Model) (models.Model, error)
  getBucketName() string
  getModel() models.Model
  getClient() *riak.Client
}

type repositoryImpl struct {
  client *riak.Client
}

func (ri *repositoryImpl) getClient() *riak.Client {
  return ri.client
}

func get(r Repository, key string, notFoundOk bool) (models.Model, error) {
  client := r.getClient()
  bucket := r.getBucketName()
  cmd, err := riak.NewFetchValueCommandBuilder().
    WithBucket(bucket).
    WithKey(key).
    WithNotFoundOk(notFoundOk).
    Build()
  if err != nil {
    return nil, err
  }
  if err = client.Execute(cmd); err != nil {
    return nil, err
  }

  fcmd := cmd.(*riak.FetchValueCommand)

  if notFoundOk && len(fcmd.Response.Values) == 0 {
    return nil, nil
  }

  if len(fcmd.Response.Values) > 1 {
    // Siblings present that need resolution
    // Here we'll just return an unexpected error
    return nil, ErrUnexpectedSiblings
  } else {
    return buildModel(r.getModel(), fcmd.Response.Values[0])
  }
}

func save(r Repository, m models.Model) (models.Model, error) {
  client := r.getClient()
  bucket := r.getBucketName()
  key := m.GetId()

  cmd, err := riak.NewFetchValueCommandBuilder().
    WithBucket(bucket).
    WithKey(key).
    WithNotFoundOk(true).
    Build()
  if err != nil {
    return nil, err
  }
  if err = client.Execute(cmd); err != nil {
    return nil, err
  }

  modelJson, err := json.Marshal(m)
  if err != nil {
    return nil, err
  }

  var objToInsertOrUpdate *riak.Object
  fcmd := cmd.(*riak.FetchValueCommand)
  if len(fcmd.Response.Values) > 1 {
    // Siblings present that need resolution
    // Here we'll just assume the first sibling is the "correct" one
    // with which to update with the new Model data
    // A conflict resolver can also be part of the options to fetchValue above
    objToInsertOrUpdate = fcmd.Response.Values[0]
    objToInsertOrUpdate.Value = modelJson
  } else {
    objToInsertOrUpdate = &riak.Object{
      Bucket:      bucket,
      Key:         key,
      ContentType: "application/json",
      Charset:     "utf8",
      Value:       modelJson,
    }
  }

  cmd, err = riak.NewStoreValueCommandBuilder().
    WithContent(objToInsertOrUpdate).
    WithReturnBody(true).
    Build()
  if err != nil {
    return nil, err
  }
  if err = client.Execute(cmd); err != nil {
    return nil, err
  }

  scmd := cmd.(*riak.StoreValueCommand)
  if len(scmd.Response.Values) > 1 {
    return nil, ErrUnexpectedSiblings
  }
  obj := scmd.Response.Values[0]
  return buildModel(r.getModel(), obj)
}

func buildModel(m models.Model, obj *riak.Object) (models.Model, error) {
  err := json.Unmarshal(obj.Value, m)
  m.SetId(obj.Key)
  return m, err
}
```

<br/>

```user-repository.go
package repositories

import (
  riak "github.com/basho/riak-go-client"
  models "github.com/basho/taste-of-riak/go/ch03/models"
)

type UserRepository struct {
  repositoryImpl
}

func NewUserRepository(c *riak.Client) *UserRepository {
  r := &UserRepository{}
  r.client = c
  return r
}

func (u *UserRepository) Get(key string, notFoundOk bool) (models.Model, error) {
  return get(u, key, notFoundOk)
}

func (u *UserRepository) Save(m models.Model) (models.Model, error) {
  return save(u, m)
}

func (u *UserRepository) getBucketName() string {
  return "Users"
}

func (u *UserRepository) getModel() models.Model {
  return &models.User{}
}
```

<br/>

```msg-repository.go
package repositories

import (
  riak "github.com/basho/riak-go-client"
  models "github.com/basho/taste-of-riak/go/ch03/models"
)

type MsgRepository struct {
  repositoryImpl
}

func NewMsgRepository(c *riak.Client) *MsgRepository {
  m := &MsgRepository{}
  m.client = c
  return m
}

func (m *MsgRepository) Get(key string, notFoundOk bool) (models.Model, error) {
  return get(m, key, notFoundOk)
}

func (m *MsgRepository) Save(model models.Model) (models.Model, error) {
  return save(m, model)
}

func (m *MsgRepository) getBucketName() string {
  return "Msgs"
}

func (m *MsgRepository) getModel() models.Model {
  return &models.Msg{}
}
```

<br/>

```timeline-repository.go
package repositories

import (
  riak "github.com/basho/riak-go-client"
  models "github.com/basho/taste-of-riak/go/ch03/models"
)

type TimelineRepository struct {
  repositoryImpl
}

func NewTimelineRepository(c *riak.Client) *TimelineRepository {
  t := &TimelineRepository{}
  t.client = c
  return t
}

func (t *TimelineRepository) Get(key string, notFoundOk bool) (models.Model, error) {
  return get(t, key, notFoundOk)
}

func (t *TimelineRepository) Save(m models.Model) (models.Model, error) {
  return save(t, m)
}

func (t *TimelineRepository) getBucketName() string {
  return "Timelines"
}

func (t *TimelineRepository) getModel() models.Model {
  return &models.Timeline{}
}
```

Finally, let's test them:

```golang
package main

import (
  "time"

  mgrs "github.com/basho/taste-of-riak/go/ch03/managers"
  models "github.com/basho/taste-of-riak/go/ch03/models"
  repos "github.com/basho/taste-of-riak/go/ch03/repositories"

  riak "github.com/basho/riak-go-client"
  util "github.com/basho/taste-of-riak/go/util"
)

func main() {
  var err error

  // un-comment-out to enable debug logging
  // riak.EnableDebugLogging = true

  util.Log.Println("Starting Client")

  o := &riak.NewClientOptions{
    RemoteAddresses: util.GetRiakAddresses(),
  }

  var client *riak.Client
  client, err = riak.NewClient(o)
  if err != nil {
    util.ErrExit(err)
  }

  defer func() {
    if err := client.Stop(); err != nil {
      util.ErrExit(err)
    }
  }()

  userRepo := repos.NewUserRepository(client)
  msgRepo := repos.NewMsgRepository(client)
  timelineRepo := repos.NewTimelineRepository(client)
  timelineMgr := mgrs.NewTimelineManager(timelineRepo, msgRepo)

  util.Log.Println("Creating and saving users")

  marleen := models.NewUser("marleenmgr", "Marleen Manager", "marleen.manager@basho.com")
  joe := models.NewUser("joeuser", "Joe User", "joe.user@basho.com")

  var m models.Model
  m, err = userRepo.Save(marleen)
  if err != nil {
    util.ErrExit(err)
  }
  marleen = m.(*models.User)

  m, err = userRepo.Save(joe)
  if err != nil {
    util.ErrExit(err)
  }
  joe = m.(*models.User)

  util.Log.Println("Posting message")

  msg := models.NewMsg(marleen.UserName, joe.UserName, "Welcome to the company!")
  if terr := timelineMgr.PostMsg(msg); terr != nil {
    util.ErrExit(terr)
  }

  util.Log.Println("Getting Joe's inbox for today")

  // Get Joe's inbox for today, get first message
  now := time.Now()
  joe_tl, terr := timelineMgr.GetTimeline(joe.UserName, models.TimelineType_INBOX, now)
  if terr != nil {
    util.ErrExit(terr)
  }

  for _, msgKey := range joe_tl.MsgKeys {
    m, merr := msgRepo.Get(msgKey, false)
    if merr != nil {
      util.ErrExit(merr)
    }
    tl_msg := m.(*models.Msg)
    util.Log.Println("From: ", tl_msg.Sender)
    util.Log.Println("Msg: ", tl_msg.Text)
  }
}
```

As you can see, the repository pattern helps us with a few things:

* It helps us to see if an object exists before creating a new one.
* It keeps our buckets and key names consistent.
* It provides us with a consistent interface to work with.

While this set of repositories solves many of our problems, it is very
minimal and doesn't cover all the edge cases. For instance, what happens
if two different people try to create a user with the same username?

Also, we can easily compute key names now, but how do we quickly look
up the last 10 messages a user sent? Many of these answers will be
application-dependent. If your application shows the last 10 messages in
reverse order, for example, you may want to store that set of data in
another collection object to make lookup faster. There are drawbacks to
every solution, but we recommend seeking out the key/value-based
solution first, as it will likely be the quickest.

So to recap, in this chapter we learned:

* How to choose bucket names.
* How to choose natural keys based on how we want to partition our data.






