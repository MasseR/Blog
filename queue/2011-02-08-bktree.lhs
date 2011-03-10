---
title: BK-trees
tags: haskell, algorithms, text, trees
---
> {-# LANGUAGE BangPatterns #-}
> import Levenshtein
> import Data.Time.Clock(diffUTCTime,getCurrentTime)
> import Data.IntMap (IntMap)
> import qualified Data.IntMap as M
> import qualified Data.Vector as V
> import Data.List (foldl')
> import Data.Char(toLower)
> data Tree = Tree Text (IntMap Tree) deriving Show


> singleton :: Text -> Tree
> singleton !s = Tree s M.empty

> insert :: Text -> Tree -> Tree
> insert !s (Tree !root !m) = Tree root $ M.insertWith recurse dist newnode m
>   where recurse !_ !old = insert s old
> 	!dist = levenshtein s root
> 	!newnode = Tree s M.empty

> fromList :: [Text] -> Tree
> fromList !(x:xs) = foldl' (flip insert) (singleton x) xs


> query :: String -> Int -> Tree -> [(Int, String)]
> query s n !(Tree !root !m) =
>   let range x = x <= d+n && x >= d-n
>       !d = levenshtein (V.fromList s) root
>       !e = M.elems $ M.filterWithKey (\k _ -> range k) m
>       !next = concatMap (query s n) e
>   in if d <= n then (d, V.toList root) : next else next


> bench ::  Tree -> String -> IO ()
> bench tree x = do
>   s <- getCurrentTime
>   mapM_ print $ query x 1 tree
>   e <- getCurrentTime
>   putStrLn $ "Search took " ++ show (diffUTCTime e s)

> main ::  IO ()
> main = do
>   --dict <- readFile "/usr/share/dict/words" >>= return . map V.fromList . lines . map toLower
>   dict <- (map V.fromList . lines . map toLower) `fmap` readFile "/usr/share/dict/words"
>   let tree = fromList dict
>   forever tree
>   where forever tree = do
> 	  putStrLn "Enter word:"
> 	  l <- getLine
> 	  bench tree l
> 	  forever tree
