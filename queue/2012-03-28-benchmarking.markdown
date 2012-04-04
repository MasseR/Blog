---
title: Benchmarking some common data structures
tags: haskell
---

After my
[query on stack
overflow](http://stackoverflow.com/questions/9772098/building-a-histogram-with-haskell-many-times-slower-than-with-python/9774668)
about why my code that builds a histogram is magnitudes slower than a simple
python script, I decided to benchmark some data structures mentioned in the
answers and what I had thought of using myself. What really spurred me to do
the benchmarks was that when I usually go to stack overflow with questions
about speed and optimization, I don't get recommendations for other data
structures, but instead on subtle coding differences that might make a huge
difference. The results also make me worried about how suitable Haskell is for
real world tasks, or at least tasks that require a map and is still fast.

For those who are interested, the accepted answer got the time down to 30
seconds. After stack overflow, it's still not as fast as python, although
closer. Also as a side notice, I noticed when doing the benchmarks that
increasing the allocation size, helps immensely with Map too. I haven't tested,
but probably with just `-A500M` would have gotten the time down to around 40-50
seconds.

## Tools and packages

For the benchmarks I am using [criterion benchmarking
suite](http://hackage.haskell.org/package/criterion-0.6.0.1).  It's a great
tool from Bryan O'Sullivan, that handles all the nitty gritty details of
statistics, deviations and much more.

As the control, I have Map from
[containers](http://hackage.haskell.org/package/containers-0.4.2.1). The
package comes with haskell-platform, so it's always readily available. It is a
balanced tree of some sort, I haven't looked into details so much. As a tree,
it should have lookup speed of O(log n). Same goes with inserting data.

Someone suggested a trie in the answers, so for that I chose
(bytestring-trie)[http://hackage.haskell.org/package/bytestring-trie] which is
a radix tree based trie. Tries are interesting data structures. For the
'normal' trie, the maximum height of the tree, is the maximum word length in
the set. This property allows them to be almost as fast as hash tables. As they
can be written immutable style, I had high hopes for them. Wikipedia mentions
that the speed for radix tree is O(m) where m is the longest word in the set.

Next I have the winner of the contest,
[hashtables](http://hackage.haskell.org/package/hashtables-1.0.1.2). A fast
mutable hashtable implementation. Speed for hash tables are usually mentioned
as O(1), but there is always the hashing part which is closer to O(m) where m
is the length of the string being hashed. On top of that there are collisions,
where we might do a linear scan. And with Haskell, there is also the problem of
arrays. Haskell doesn't have similar primitive arrays as imperative languages,
and they too have a little bit of overhead.

And for the last one I have HashMap from
[unordered-containers](http://hackage.haskell.org/package/unordered-containers-0.2.1.0).
There have been a couple of posts lately that unordered-containers have gotten
faster, so I wanted to see how they compare.

## Code

~~~{.haskell}

{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as C
import qualified Data.HashTable.IO as H
import qualified Data.HashMap.Strict as UH
import qualified Data.Map as M
import qualified Data.Trie as T
import Data.List (foldl')
import Control.Monad
import Criterion.Main

type HashMap = H.BasicHashTable C.ByteString Int
type UHashMap = UH.HashMap C.ByteString Int
type Map = M.Map C.ByteString Int
type TrieMap = T.Trie Int

hashHistogram :: [C.ByteString] -> IO HashMap
hashHistogram w = do
  m <- H.new :: IO HashMap
  mapM_ (go m) w
  return m
  where
    go :: HashMap -> C.ByteString -> IO ()
    go m word = do
      c <- H.lookup m word
      case c of
           Nothing -> H.insert m word 1
           Just n -> H.insert m word (n+1)

uhashHistogram :: [C.ByteString] -> UHashMap
uhashHistogram w = foldl' (\m k -> UH.insertWith (+) k 1 m) UH.empty w

mapHistogram :: [C.ByteString] -> Map
mapHistogram w = foldl' (\m k -> M.insertWith' (+) k 1 m) M.empty w

trieHistogram :: [C.ByteString] -> TrieMap
trieHistogram w = foldl' (\t k -> T.alterBy (\_ _ -> Just . maybe 1 (+1)) k 1 t) T.empty w

main :: IO ()
main = do
  w <- C.words `fmap` C.readFile "pg64.txt" -- Two books from gutenberg, totaling ~140k words
  length w `seq` return ()
  let ns = tail $ [0,1000..100000]
  defaultMain [
    bgroup "histogram" $
         [bench ("Map/" ++ show n) $ whnf mapHistogram $ take n w | n <- ns]
      ++ [bench ("TrieMap/" ++ show n) $ whnf trieHistogram $ take n w | n <- ns]
      ++ [bench ("unordered-containers HashMap/" ++ show n) $ whnf uhashHistogram $ take n w | n <- ns]
      ++ [bench ("HashMap/" ++ show n) $ whnfIO (hashHistogram $ take n w) | n <- ns]
    ]

~~~
