---
title: Session Storage
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [use-cases]
moved: {
  '1.4.0-': '/cookbooks/use-cases/serving-ads'
}
---

## Simple Case

Riak was originally created to serve as a highly scalable session store.   This is an ideal use case for Riak, which is a fundamentally a key value store.  Since user/session IDs are usually stored in cookies or otherwise known at lookup time, Riak is able to serve these requests with predictably low latency.  Riak's content-type agnosticism also imposes no restrictions on the value, so session data can be encoded in many ways and can evolve without administrative changes to schemas.


## Complex Case

Riak has other features that enable more complex session storage use cases.  The bitcask storage backend supports automatic expiry of keys, which frees application developers from implementing manual session expiry.  Riak's MapReduce system can also be used to perform analysis on large bodies of session data, for example to compute the average number of active users.  If sessions must be retrieved using multiple keys (e.g. a UUID or email address), Secondary Indexes (2I) provides an easy solution.

## Community Examples

<table class="links">
    <tr>
        <td><a href="https://player.vimeo.com/video/42744689" target="_blank" title="Scaling Riak at Kiip">
           <img src="http://b.vimeocdn.com/ts/296/624/296624215_960.jpg"/>
         </a></td>
        <td><a href="https://player.vimeo.com/video/42744689" target="_blank" title="Riak at OpenX">Scaling Riak at Kiip</a>
        <br>
    In this talk, recorded at the May 2012 San Francisco Riak Meetup, Armon Dadgar and Mitchell Hashimoto of Kiip give an overview of how and why they are using Riak in production, and the road they took to get there. One of the first subsystems they switched over to Riak was Sessions. You can also read the blog post and catch the slides <a href="http://basho.com/blog/technical/2012/05/25/Scaling-Riak-At-Kiip/" class="riak" target="_blank">here.</a>
        </td>
    </tr>
</table>
