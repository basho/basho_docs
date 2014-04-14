---
title: Developing with Riak
project: riak
version: 0.10.0+
toc: true
document: appendix
audience: intermediate
keywords: [community, resources]
moved: {
  '1.4.0-': '/references/appendices/community/Developing-with-Riak'
}
---

This page is a collection of videos, slides, papers, example applications, and other media discussing developing with Riak (general guidelines, using client libraries, etc.).

_If you have a video to add, please fork the [Riak Docs Repo on GitHub](https://github.com/basho/basho_docs) and do so._

## Videos

<table class="vid_table">
	<tr>
	    <td class="vid_td">
	    	<a href="http://player.vimeo.com/video/21099379" target="_blank" title="Riak with node.js Webinar"><img class="vid_img"src="http://b.vimeocdn.com/ts/135/477/135477978_200.jpg"/></a>
	    </td>
	    <td class="vid_td">
	    	<a href="http://player.vimeo.com/video/21099379" target="_blank" title="Riak with node.js Webinar">Riak with node.js Webinar</a><br />Learn how to use Riak from NodeJS applications.
		</td>	    
	</tr>
	<tr>
	    <td class="vid_td">
	    	<a href="http://player.vimeo.com/video/13924929" target="_blank" title="Riak with Rails"><img class="vid_img"src="http://b.vimeocdn.com/ts/807/889/80788943_640.jpg"/></a>
	    </td>
	    <td class="vid_td">
	    	<a href="http://player.vimeo.com/video/13924929" target="_blank" title="Riak with Rails">Riak with Rails</a><br />Using Riak with Ruby on Rails.
		</td>
	</tr>

	<tr>
	    <td class="vid_td">
	    	<a href="http://player.vimeo.com/video/18713269" target="_blank" title="Riak Overview and Schema Design"><img class="vid_img"src="http://b.vimeocdn.com/ts/117/890/117890563_200.jpg"/></a>
	    </td>
	    <td class="vid_td">
	    	<a href="http://player.vimeo.com/video/18713269" target="_blank" title="Riak Overview and Schema Design">Riak Overview and Schema Design</a><br />Basho Hackers Jon Meredith and Dave "Dizzy" Smith give an extensive overview of what Riak is and how you should approach schema design when developing with Riak. (Special thanks to the team at Pivotal Labs Boulder for making this happen.) 
		</td>	    
	</tr>		
	<tr>
	    <td class="vid_td">
	    	<a href="http://player.vimeo.com/video/13554436" target="_blank" title="MapReduce querying in Riak"><img class="vid_img"src="http://b.vimeocdn.com/ts/779/892/77989230_640.jpg"/></a>
	    </td>
	    <td class="vid_td">
	    	<a href="http://player.vimeo.com/video/13554436" target="_blank" title="MapReduce querying in Riak">MapReduce querying in Riak</a><br />Basics and best practices for MapReduce querying in Riak
		</td>
	</tr>
</table>

## Example Application

To showcase the power of indexing in Riak, we created a [Zombie Sample App](http://zombies.samples.basho.com/) that's run on Riak. This app has one million “Zombielepsy” victims loaded into Riak and lets the user locate them using ZIP code as the index value. It supports both Term-Based Inverted Indexes and [[secondary indexes|using secondary indexes]]. In addition to better understanding indexing in Riak, users can:

* Create a Zombie Sighting Report System so that the concentration of live zombies in an area can be quickly determined based on the count and last report date.
* Add a crowd-sourced Inanimate Zombie Reporting System so that members of the non-zombie population can report inanimate zombies.
* Add a correlation feature, utilizing Graph CRDTs, so we can find our way back to Patient Zero.

More information on the app can be found [here](http://basho.com/indexing-the-zombie-apocalypse-with-riak/).

## Slide Decks

This is a sample of the slide decks used in presentations given by Riak Core Developers and Developer Advocates, and members of the Riak Community at conferences, meetups, and various other events worldwide.

_If you have a Slide Deck to add, please fork the [Riak Docs Repo on GitHub](https://github.com/basho/basho_docs) and do so._

* [riak-js: Javascript Turtles All the Way Down](http://www.slideshare.net/seancribbs/riakjs-javascript-turtles-all-the-way-down) --- riak-js is an awesome client driver for the Riak distributed datastore.
* [Building Distributed Systems With Riak and Riak Core](http://www.slideshare.net/argv0/riak-coredevnation) --- My talk from DevNationSF 2010 
* [Configure a Riak Cluster](http://www.slideshare.net/mbbx6spp/link-walking-with-riak) --- Describe how to create, manage and traverse links in Riak KV. 
* [Link Walking with Riak](http://www.slideshare.net/seancribbs/riakjs-javascript-turtles-all-the-way-down) --- riak-js is an awesome client driver for the Riak distributed datastore.
* [Riak with NodeJS](http://www.slideshare.net/seancribbs/riak-with-nodejs) --- Learn how to use Riak with NodeJS applications.
