Twitter Graph in Haskell
========================

Here we analyze Twitter communication graph.  We load it from TokyoCabinet, where it's represented as pairs

	node -> json-adjacency-list
	
We convert JSON to a Map, and the whole graph is a map of maps.  Once we build the graph in memory, we dump it with `Data.Binary` and then load that back in.


GHC trunk
---------

As of June 2010, this code has elicited a numeric overflow bug in GC under GHC 6.12.1.  Simon Marlow valiantly fought and quashed it, renting a 68 GB instance on Amazon EC2, the largest then available.  (Microsoft Research needs more memory!)  I got the nightly binary build for amd64, the 6.13.20100625, but it depended on `libtinfo`.

JSONb 1.0.0.1

ghc, ghc-pkg, cabal precedence


Building
--------

The program was sped up greatly with `+RTS -A5G`, otherwise was crawling for hours.

	ghc --make -O2 -rtsopts sc.hs
	
@c_wraith recommends building with cabal.  In fact building under GHC head shows why -- e.g. binary-0.5.0.2 was hidden, so I'd had to either add a flag to ghc make as:

	ghc --make -O2 -rtsopts sc.hs -package binary-0.5.0.2
	
or unhide it with

	ghc-pkg expose

Data Preparation -- Interning
-----------------------------	

	
Running
-------


	time ./sc data/sample/dreps100K.bin.zip data/sample/dments100K.bin.zip +RTS -A5G -H2G -K1G -M60G
	
I've added the `-A` and `-K` sizes which did the job; perhaps lower levels would work with the full data, and surely would with the samples.  The samples are 100,000 users each, vs. 3.5 million for the originals (to be uploaded at a later date).

Acknowledgements
----------------

This is the 3rd time I've taken up Haskell, and it worked.  I thank the great `#haskell` channel on Freenode.  @dafis provided a bangified version of `SocRun.hs`, to which I added a couple more bangs.  I wonder which ones are unnecessary?
