---
title: Production Deployments
project: riak
version: 0.10.0+
document: appendix
toc: true
keywords: [community, resources]
---

Comcast, Yammer, Voxer, Boeing, BestBuy, Joyent, Kiip, GitHub, and the Danish Government are just a few of the thousands of [[startups and enterprises that have deployed Riak|http://basho.com/company/production-users/]].

This page is a collection of videos, slides, papers, and other media detailing various Riak deployments.

## Videos

<table class="vid_table">

	<tr>
        <td class="vid_td"><a href="http://player.vimeo.com/video/44498491" target="_blank" title="Riak in Production at Voxer">
		   <img class="vid_img"src="http://b.vimeocdn.com/ts/309/154/309154350_200.jpg"/>
		 </a></td>
        <td class="vid_td"><a href="http://player.vimeo.com/video/44498491" target="_blank" title="Riak in Production at Voxer">Riak in Production at Voxer</a>  
	Voxer relies on Riak as their primary database for various services. They have more than 50 machines dedicated to Riak to support their huge growth and user base, and this has made for an adventure in scaling.
		</td>	    
	</tr>
	<tr>
		<td class="vid_td"><a href="http://player.vimeo.com/video/42744689" target="_blank" title="Scaling Riak at Kiip">
		   <img class="vid_img"src="http://b.vimeocdn.com/ts/296/624/296624215_200.jpg"/>
		 </a></td>
        <td class="vid_td"><a href="http://player.vimeo.com/video/42744689" target="_blank" title="Scaling Riak at Kiip">Scaling Riak at Kiip</a>
			In this talk, recorded at the May San Francisco Riak Meetup, Armon Dadgar and Mitchell Hashimoto of Kiip give an overview of how and why they are using Riak in production, and the road they took to get there.

			You can also get the [[slides|https://speakerdeck.com/u/mitchellh/p/day-at-kiip]].
		</td>
	</tr>	
	<tr>
        <td class="vid_td"><a href="http://player.vimeo.com/video/35905739" target="_blank" title="Riak at Posterous">
		   <img class="vid_img"src="http://b.vimeocdn.com/ts/245/866/245866678_200.jpg"/>
		 </a></td>
        <td class="vid_td"><a href="http://player.vimeo.com/video/35905739" target="_blank" title="Riak at Posterous">Riak at Posterous</a>
	Julio Capote, Backend Engineer at Posterous, talks about how their move to Riak to serve as a post cache, and how they went about selecting it over other database like MySQL, MongoDB, and Redis.
		</td>	    
	</tr>		

	<tr>
        <td class="vid_td"><a href="http://player.vimeo.com/video/27208328" target="_blank" title="Riak at Formspring">
		   <img class="vid_img"src="http://b.vimeocdn.com/ts/180/470/180470095_200.jpg"/>
		 </a></td>
        <td class="vid_td"><a href="http://player.vimeo.com/video/27208328" target="_blank" title="Riak at Formspring">Riak at Formspring</a>
In this talk, Tim Bart of Formspring gives an overview of how and why they are using Riak as the data storage layer for two new features they are rolling out. Tim starts with an overview of what Formpring is all about and then goes into the design of their new features and what they've learned (both good and bad) when working with Riak at scale.
		</td>	    
	</tr>
	<tr>
        <td class="vid_td"><a href="http://player.vimeo.com/video/21598799" target="_blank" title="Riak and Scala at Yammer">
		   <img class="vid_img"src="http://b.vimeocdn.com/ts/139/033/139033664_200.jpg"/>
		 </a></td>
        <td class="vid_td"><a href="http://player.vimeo.com/video/21598799" target="_blank" title="Riak and Scala at Yammer">Riak and Scala at Yammer</a>
In this talk Coda Hale and Ryan Kennedy give an overview of "Streamie," the Riak-backed notifications service they recently built and put into production at Yammer.
		</td>	    
	</tr>		
	<tr>
        <td class="vid_td"><a href="http://player.vimeo.com/video/13508289" target="_blank" title="Riak in Production - Lexer">
		   <img class="vid_img"src="http://b.vimeocdn.com/ts/776/587/77658784_200.jpg"/>
		 </a></td>
        <td class="vid_td"><a href="http://player.vimeo.com/video/13508289" target="_blank" title="Riak in Production - Lexer">Riak in Production - Lexer</a>  
In this short clip, Basho's Community Manager Mark Phillips speaks with Andrew Harvey, a developer for Lexer, and gets a few details on how and why they are using Riak in their applications.
		</td>	    
	</tr>	
    <tr>
        <td class="vid_td"><a href="http://www.infoq.com/presentations/Case-Study-Riak-on-Drugs" target="_blank" title="Riak on Drugs (and the Other Way Around)">
		   <img class="vid_img"src="http://basho.com/images/riak-on-drugs.jpg"/>
		 </a></td>
        <td class="vid_td"><a href="http://www.infoq.com/presentations/Case-Study-Riak-on-Drugs" target="_blank" title="Riak on Drugs (and the Other Way Around)">Riak on Drugs (and the Other Way Around)</a>  
Oct. 11, 2011 Kresten Krab Thorup discusses a MySQL project that was moved to Riak for high availability, scalability and to run off multiple data centers, sharing the experiences, pitfalls and lessons learned. 
		</td>	    
	</tr>
	<tr>
        <td class="vid_td"><a href="http://player.vimeo.com/video/37930578" target="_blank" title="Instant-ish Real Service Architecture">
		   <img class="vid_img"src="http://b.vimeocdn.com/ts/260/760/260760293_200.jpg"/>
		 </a></td>
        <td class="vid_td"><a href="http://player.vimeo.com/video/37930578" target="_blank" title="Instant-ish Real Service Architecture">Instant-ish Real Service Architecture</a>
	Filmed at BashoChats on February 28, 2012.

		Ted Nyman, Lead Engineer at Simple, talks about the RESTful service-oriented JVM-backed architecture they've built using new libraries and frameworks like Dropwizard which allow for the development of sophisticated services with surprising little development time.
		</td>	    
	</tr>
</table>

## Slide Decks

This is a sample of the slide decks used in presentations given by Riak Core Developers and Developer Advocates, and members of the Riak Community at conferences, meetups, and various other events worldwide. *(If you have a Slide Deck to add, please fork the [Riak Docs Repo on GitHub](https://github.com/basho/basho_docs) and do so.)*

* [[Riak at Posterous|http://www.slideshare.net/capotej/riak-at-posterous]] - Posterous recently deployed Riak to serve as their content cache. In this talk, Julio Capote will cover why the engineering team chose Riak for the use case. He'll also share some details on the old post cache and its problems, what solutions they evaluated, and how they settled on Riak.
* [[Scaling with Riak at Showyou|http://www.slideshare.net/jmuellerleile/scaling-with-riak-at-showyou]] - A presentation on how Showyou uses the Riak datastore at Showyou.com, as well as work we've been doing on a custom Riak backend for search and analytics.
* [[Riak perf wins|http://www.slideshare.net/flakenstein/riak-perf-wins]] - How the team at Clipboard.com got more than 100x better search performance with some simple changes to riak search..
* [[Riak Use Cases : Dissecting The Solutions To Hard Problems|http://www.slideshare.net/argv0/riak-use-cases-dissecting-the-solutions-to-hard-problems]] - Basho Chief Architect Andy Gross discusses how Riak can be used in difficult environments.
* [[Scaling at Showyou: Operations|http://www.slideshare.net/aphyr_/scaling-at-showyou]] - Architecture/operations slides from the Scaling at Showyou talk. 
* [[Riak a successful failure|http://www.slideshare.net/GiltTech/riak-a-successful-failure-11512791]] - Riak in production: Portrait of a successful failure.


## Publications

The following papers give case studies of Riak in production.

* [[Auric - Riak Powers Payment Card Industry Solution|http://media.basho.com/pdf/Auric-Case-Study.pdf]]
* [[Linkfluence - Linkfluence Drives Social Media Insight with Riak|http://media.basho.com/pdf/Linkfluence-Case-Study-v2-1.pdf]]
* [[Trifork - Basho and Trifork Power Critical Medical Data System for Danish Citizens|http://media.basho.com/pdf/Trifork-Case-Study.pdf]]
* [[Voxer - Voxer Drives Meteoric Growth Powered by Riak|http://media.basho.com/pdf/Voxer-Case-Study.pdf]]
* [[Yammer - Yammer Powers User Notifications with Riak|http://media.basho.com/pdf/Yammer-Case-Study-v2-1.pdf]]

