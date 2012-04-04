---
title: Lunchtime coding: Text generator
tags: haskell, snippet, code
---

> module Main where

> import qualified Data.HashMap.Lazy as M
> import Data.HashMap.Lazy (HashMap)
> import qualified Data.Text.Lazy as T
> import Data.Text.Lazy (Text)
> import qualified Data.Text.Lazy.IO as TI
> import Data.List (foldl', sortBy)
> import System.Random
> import System.Environment

> type Prefix = [Text]
> type Model = HashMap Prefix (HashMap Text Int)
> type Corpus = Text

> empty :: Model
> empty = M.empty

> separate :: Int -> Corpus -> [[Text]]
> separate n corpus = let split = T.words corpus
>                     in separate' split []
>   where
>     separate' [] acc = dropWhile (\x -> length x /= n) acc
>     separate' split acc = separate' (tail split) (take n split : acc)

> train :: Int -> Model -> Corpus -> Model
> train n model = foldl' updatePrefix model . separate n
>   where
>     updatePrefix m xs = let prefix = take (n-1) xs
>                             word = last xs
>                         -- Unfortunately there is no insertWithKey like Data.Map
>                         in M.insertWith (updateCounts word) prefix (M.singleton word 1) m
>     updateCounts :: Text -> HashMap Text Int -> HashMap Text Int -> HashMap Text Int
>     updateCounts key _ = M.insertWith (+) key 1

> generate :: (RandomGen g) => Int -> g -> Model -> Text
> generate n g model = let (start, g') = randomR (0, M.size model - 1) g
>                          start' = M.keys model !! start
>                      in T.unwords $ reverse $ generate' (n - length start') start' g' start'
>   where
>     generate' 0 _ _ acc = acc
>     generate' c context g acc = case M.lookup context model of
>                                      Nothing -> acc
>                                      Just submodel -> let total = M.foldl' (+) 0 submodel
>                                                           words = sortBy (\a b -> snd b `compare` snd b) $ M.toList submodel
>                                                           (skip, g') = randomR (0, total - 1) g
>                                                           nextword = consume skip words
>                                                           phrase = context ++ [nextword]
>                                                           context' = tail phrase
>                                                      in generate' (c-1) context' g' (nextword : acc)
>     consume c ((x, c'):xs) | c - c' <= 0 = x
>                           | otherwise = consume (c - c') xs

> main :: IO ()
> main = do
>   [file, n1', n2'] <- getArgs
>   let n1 = read n1'
>   let n2 = read n2'
>   text <- fmap T.toLower $ TI.readFile file
>   let model = train n1 empty text
>   g <- getStdGen
>   TI.putStrLn $ generate n2 g model
