---
title: Fuzzy string matching with tries
tags: haskell, algorithms, trees, tries, text
---
> {-# LANGUAGE BangPatterns, OverloadedStrings #-}
> import Data.List
> import qualified Data.Vector as V
> import Data.Vector (Vector, (!))
> import System.Environment
> import Data.Time.Clock
> import Data.Map (Map)
> import qualified Data.Map as M
> import Data.Text (Text)
> import qualified Data.Text as T
> import qualified Data.Text.IO as TI
> type Children a = Map a (Trie a)
> data Trie a = Root !(Children a) | Leaf Bool !(Node a) !(Children a) deriving Show
> data Node a = Node !a deriving (Show, Eq, Ord)
> children (Root !xs) = xs
> children (Leaf _ _ !xs) = xs
> setChildren (Root _) !xs = Root xs
> setChildren (Leaf !f !n _) !xs = Leaf f n xs

> trieInsert :: Text -> Trie Char -> Trie Char
> trieInsert !xs !t | T.length xs > 1 =
>   let !x = T.head xs
>       !xs' = T.tail xs
>       !newNode = Leaf False (Node x) M.empty
>       !c = children t
>       append _ old@(Leaf !f !n !zs) = trieInsert xs' old
>   in setChildren t $ M.insertWith' (append) x (trieInsert xs' newNode) c
> 		| not $ T.null xs =
>   let !x = T.head xs
>       !newNode = Leaf True (Node x) M.empty
>       !c = children t
>       finish _ (Leaf _ !n !xs) = Leaf True n xs
>   in setChildren t $ M.insertWith (finish) x (newNode) c
> 		| otherwise = t
> fromList :: [Text] -> Trie Char
> fromList !xs = foldl' (flip trieInsert) (Root M.empty) xs
> isFinal (Leaf !f _ _) = f
> value (Leaf _ (Node !n) _) = n

> search xs dist !t = {-# SCC "search" #-}
>   let !w = V.fromList xs
>       !fr = V.generate (V.length w + 1) id
>   in filter (not . T.null) $ concatMap (search' w fr dist "") $ M.elems $ children t

> search' :: Vector Char -> Vector Int -> Int -> String -> Trie Char -> [Text]
> search' !w !prvr !dist !xs !t = {-# SCC "search'" #-}
>   let curRow = V.generate (V.length w + 1) fn
>       fn 0 = (prvr ! 0) + 1
>       fn !i = ic i `seq` dc i `seq` rc i `seq` minimum [ic i, dc i, rc i]
>       prev !i = i-1
>       ic !i = {-# SCC "ic" #-} curRow ! prev i + 1
>       dc !i = {-# SCC "dc" #-} prvr ! i + 1
>       pr !i = {-# SCC "pr" #-} prvr ! prev i
>       rc !i = {-# SCC "rc" #-} if x == (w ! prev i) then pr i else pr i + 1
>       !x = {-# SCC "x" #-} value t
>       !xs' = {-# SCC "xs'" #-} xs ++ [x]
>       !lev = {-# SCC "lev" #-} V.last curRow
>       !c = M.elems $ children t
>       !next = {-# SCC "next" #-} (concatMap (search' w curRow dist xs') c)
>   in if isFinal t && lev <= dist then (T.pack xs') : next else next

> testWords :: [Text]
> testWords = ["the", "they're", "them", "those", "that", "him", "he", "his", "hers", "her", "car", "cat", "catacomb", "catacombs", "zyxon", "amd", "intel", "a", "at", "party", "pregnant", ""]

> bench :: Trie Char -> Text -> IO ()
> bench !tree !x = do
>   s <- getCurrentTime
>   TI.putStrLn $ T.unlines $ search (T.unpack x) 1 tree
>   e <- getCurrentTime
>   TI.putStrLn $ "Search took " `T.append` (T.pack $ show $ diffUTCTime e s)

> main = do
>   dict <- TI.readFile "/usr/share/dict/words"
>   --mapM_ (TI.putStrLn) $ T.lines dict
>   let tree = fromList $ T.lines dict
>   TI.putStrLn "creating database"
>   --mapM (bench tree) ["gooble"]
>   forever tree
>   where forever tree = do
> 	  TI.putStrLn "Enter word:"
> 	  l <- TI.getLine
> 	  bench tree l
> 	  forever tree
