This directory contains code to analyze an actual month or so of Twitter data, namely gardenhose -- their streaming API representing a statistically significant fraction of Twitter.  The dataset is from October-November of 2009 and contains about 100 million tweets.

From those tweets we gathered a communication graph of those _repliers_ who address each other in public with @mentions and carry out conversations that way -- a preferred way to exchange URLs and quick updates and opinions online now.

In that graph, some people get much more replies than others, making us wonder, what drives their influence?  For that purpose, we introduce a notion of "karmic" _Social Capital_ which rewards replying to those who ask you, getting replies from those whom you asked, talking more with those with whom you talk a lot already, etc.

For each user @Alice who tweeted "@Bob: hey!" (or anything) on November 20, 2009, we'll have an edge

	Alice 	Bob	2009-11-20
	
in our `dreps` graph, for "daily replies," and also an edge

	Bob	Alice	2009-11-20
	
in our `dments` graph, for daily mentions.  

The graphs are stored in TokyoCabinet as adjacency lists.  For instance, if Alice also tweeted as

	Alice	Mandy	2009-11-20
	Alice	Mandy	2009-11-20
	Alice	Zack	2009-11-25
	
then the resulting node and its adjacency list in `dreps` will look as follows:

	Alice -> {"2009-11-20":{"Bob":1,"Mandy":2}, "2009-11-25":{"Zack":1}}
	
Note that for each day, we count how many such directed tweets exist, mapping user names to their respective counts.

I implemented this world originally in Clojure, shown here as `socrun.clj`, and then translated into Haskell with minor changes.  The Clojure file represents a part of the system called interactively from the _repl_, where the `dreps` and `dments` graphs are already loaded, and the result is computed by calling `socrun` from a `(def sgraph ...)`.  The Haskell reads the graphs first and then computes the social capital, writing its history for each person, the `dcaps`, to a file with `Data.Binary`, compressed.

Clojure goes through all 35 days in about 35 minutes, staying within a 32 GB JVM with compressed references.  Initially, the SocRun version without any `seq` statements added ran through day 30, reaching about 57 GB, and just stayed there for a day without much swapping.  Adding a maxDays parameter allowed to try smaller day run, which caused stack overflow, suggesting increasing the stack size.  Throwing in a quick `-K5G` after an `-A5G` which have already sped up things 1000x allowed to run through for 10 days fairly quickly.  While not getting orders of magnitude advantage over Clojure, I still saw significant speedup, at least about 2x.  However, 20 days still ended up sitting there doing nothing.

There's a profiling run in sc10days.prof.
