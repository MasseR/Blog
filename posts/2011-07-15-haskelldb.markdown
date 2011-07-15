---
title: Example on using HaskellDB
tags: database, haskell
---

### HaskellDB

A while ago we had a database course in the university. We decided to do it
with Haskell, and found out about
[HaskellDB](http://hackage.haskell.org/package/haskelldb), which is a
high-level database wrapper based on relational algebra. Therefore we felt it
as a best choice for showing how functional languages can handle database
queries. I had beforehand done some testing with HDBC, which is a database
wrapper influenced by Perl, and felt that using long query strings within
Haskell was, if nothing else, ugly.

We felt that HaskellDB would provide us a nice declarative interface and type
safety. Little did we know about the journey it would take us into, and the
perils that waited us.

The first hurdle we faced, was that HaskellDB was badly documented. I felt that
I needed some high-level overview of the library before I could dive in into
the API documentation, but sadly there was practically no documentation like
that. Two papers from the original creator was found from
[here](http://haskelldb.sourceforge.net/haskelldb.pdf) and
[here](www.cs.chalmers.se/~bringert/publ/haskelldb/haskelldb-db-2005.pdf).
First document was from 2004 and the second from 2005, but the library hasn't
been updated that much since, and the papers were still valid. They
complemented each other, but even then they both focused on queries, even
though HaskellDB required the layout to be defined. I found no examples of how
to define the DBLayout, until one lucky search which found a `Setup.hs` example
from [hpaste](http://hpaste.org). Unfortunately, I no longer can find the
paste, if it even exists.

In this post, I will write an example (two actually) on how to set up HaskellDB
so that you can run your queries. After that I will write some pros and cons I
found doing the assignment. I am by no means an expert in HaskellDB, but I felt
that I should document the journey I went through, so that the people after me
might find it easier.

### The example
