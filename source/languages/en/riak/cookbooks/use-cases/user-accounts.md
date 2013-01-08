---
title: User Accounts
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [use-cases]
---

## Simple Case

User accounts are pretty straight-forward -- the usual practice is to store JSON objects in a 'users' bucket. As far as what to use for a key value, usual app-specific considerations apply. For example, if your application involves user logins, the simplest and most read-efficient way is to use the login username as the object key. Get the username off the login, perform a GET on the user account object and go. There are several drawbacks, however - what if they'll want to change their username or email, later? The most common solution is - use a unique UUID type key for the user, and store their username or email as a Secondary Index, for efficient lookup.


## Complex Case

For simple retrieval of a specific account, a user id (plus perhaps a secondary index on a username/email) is enough. If you foresee the need to make queries on additional user attributes (creation time, user type, region), plan ahead and either set up additional Secondary Indexes, or consider using Riak Search to index the JSON contents of the user account.

## Community Examples

<table class="links">
  <tr>
    <td><a href="https://player.vimeo.com/video/47535803" target="_blank" title="Riak at Braintree"><img class="vid_img"src="http://b.vimeocdn.com/ts/329/711/329711886_640.jpg"/></a>
    </td>
    <td><a href="https://player.vimeo.com/video/47535803" target="_blank" title="Riak at Braintree">Riak at Braintree</a>
    <br>
    Ben Mills, a developer at Braintree, discusses how their backend team came to find and begin to integrate Riak into their production environment. They also cover their model and repository framework for Ruby, Curator. Check out more details and slides on the <a href="http://basho.com/blog/technical/2012/08/14/riak-at-braintree/" target="_blank">Riak blog.</a>
    </td>	    
  </tr>
</table>

