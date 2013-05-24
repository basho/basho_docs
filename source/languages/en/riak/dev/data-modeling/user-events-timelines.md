---
title: User Subscriptions/Events/Timelines
project: riak
version: 1.2.0+
document: cookbook
toc: true
audience: intermediate
keywords: [use-cases]
---

*Typically one-to-many and many-to-many relationships*

## Simple Case

Sometimes you may want to do more complex or specific kinds of modeling user data. A common example would be storing data for assembling a social network timeline. To create a user timeline, you could make "timeline" a bucket in Riak, and make keys a unique user ID. You would store timeline information as a value - a list of status update IDs which could then be used to retrieve the full information from another bucket, or perhaps containing the full status update. If you want to store additional data - such as a timestamp, category or list properties - turn the list into an array of hashes containing this additional information. Note that in Riak, you cannot append information to an object, so to add events in the timeline, you would have to read the full object, add the new values to the hash, and write it back. 

## Community Examples

<table class="links">
	<tr>
	    <td><a href="http://player.vimeo.com/video/21598799" target="_blank" title="Riak at Yammer">
		   <img src="http://b.vimeocdn.com/ts/139/033/139033664_640.jpg"/>
		 </a></td>
	    <td><a href="http://player.vimeo.com/video/21598799" target="_blank" title="Riak at Yammer">Riak at Yammer</a>
		<br>
	This video was recorded at the March 2012 San Francisco Riak Meetup and is worth every minute of your time. Coda Hale and Ryan Kennedy of Yammer give an excellent and in depth look into how they built “Streamie”, user notifications, why Riak was the right choice, and the lessons learned in the process. Read more and get the slides in the Riak blog <a href="http://basho.com/blog/technical/2011/03/28/Riak-and-Scala-at-Yammer/" target="_blank">here.</a>
		</td>	    
	</tr>

	<tr>
	    <td><a href="http://player.vimeo.com/video/44498491" target="_blank" title="Riak at Voxer">
		   <img src="http://b.vimeocdn.com/ts/309/154/309154350_960.jpg"/>
		 </a></td>
	    <td><a href="http://player.vimeo.com/video/44498491" target="_blank" title="Riak at Voxer">Riak at Voxer</a>
		<br>
	The team at Voxer has long relied on Riak as their primary data store for various production services. They have put Riak through its paces and have served as one of our more exciting customers and use cases: Riak was in place when they shot to the top of the App Store at the end of 2011. We also love them because they open-sourced their Node.js client. Read more and get the slides in the Riak blog <a href="http://basho.com/blog/technical/2012/06/27/Riak-at-Voxer/" target="_blank">here.</a>
		</td>	    
	</tr>
</table>