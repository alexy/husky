Twitter Graph in Haskell
========================

Haskell-Cafe patrons: head straight for [the Haskell vs Clojure making-it-just-work and then -better challenge!](husky/Haskell-vs-Clojure-Twitter.md)

Here we analyze Twitter communication graph.  We load it from TokyoCabinet, where it's represented as pairs

	node -> json-adjacency-list
	
We convert JSON to a Map, and the whole graph is a map of maps, similar to my Clojure representation -- in fact, the cabinet is made by Clojure, with `Jackson`.  Originally we use an even faster representation with Google Protocol Buffers, but have to extend the Haskell's with the `(map)` extension for repeated map entries on the wire.  

Once we build the graph in memory, we dump it with `Data.Binary` and then load that back in.

The program was sped up greatly with `+RTS -A10G`, otherwise was crawling for hours.  Now the read is done in seconds, and a full run with dump in 11 minutes.  Clojure does simple JSON read in an hour at least, but protobufs in 3 minutes.  We'll speed up Jackson read with explicit field by field map-building though.