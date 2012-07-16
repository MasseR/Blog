---
title: Lunchtime coding: Text generator
tags: haskell, snippet, code
---

A while back we had a course about natural language processing, or the basics
of it. During one class we were discussing
[n-grams](http://en.wikipedia.org/wiki/N-gram) which is a naive probabilistic
language model. It assumes that the nth word is dependent only on the $n-1$
previous words. I'm using the word 'word' here, but it can as well be
characters or any other item.

N-grams are extremely versatile. I've seen them used to predict whether a word
belongs a dictionary, predict whether there is a mistype error, language
identification, speech recognition etc.

Today however we're looking for fun times, we're going to use n-grams to
generate random text. The 'n' in n-grams means the amount of words, we consider
relevant for the probability. The higher the 'n', the better the text, but it
lessens the variation, so we need to find a proper 'n' by trial and error.

Enough chit-chat, let's get to the code, shall we? Let's get the imports down
first.

> module Main where

> import qualified Data.HashMap.Lazy as M
> import Data.HashMap.Lazy (HashMap)
> import qualified Data.Text.Lazy as T
> import Data.Text.Lazy (Text)
> import qualified Data.Text.Lazy.IO as TI
> import Data.List (foldl', sortBy)
> import System.Random
> import System.Environment

How do we model the previous state (n-1 words)? Because we need to find the 'n'
by trial and error, it's not good to hard-code it into data types, so let's
just assume that the previous state is a list of words. For the language model
we can assume that a list of words, the prefix, is followed by a single word
(the nth word). But notice, that the prefix can be followed by any number of
different words, with different probabilities, so let's record the different
words and their frequency.

> type Prefix = [Text]
> type Model = HashMap Prefix (HashMap Text Int)
> type Corpus = Text

> empty :: Model
> empty = M.empty

From our corpus, we need to separate the prefixes and suffixes. Our `separate`
function does just this. It takes an 'n' and puts the list of words into a
list, so we get a list of list of words.

> separate :: Int -> Corpus -> [[Text]]
> separate n corpus = let split = T.words corpus
>                     in separate' split []
>   where
>     separate' [] acc = dropWhile (\x -> length x /= n) acc
>     separate' split acc = separate' (tail split) (take n split : acc)

As for the training, we take our list of lists, and for each list of words, we
take $n-1$ words, and insert it as a key to our model. For the $n$ word, we
create another map with the default value of $1$, but if it exists, we
increment it by one.

> train :: Int -> Model -> Corpus -> Model
> train n model = foldl' updatePrefix model . separate n
>   where
>     updatePrefix m xs = let prefix = take (n-1) xs
>                             word = last xs
>                         -- Unfortunately there is no insertWithKey like Data.Map
>                         in M.insertWith (updateCounts word) prefix (M.singleton word 1) m
>     updateCounts :: Text -> HashMap Text Int -> HashMap Text Int -> HashMap Text Int
>     updateCounts key _ = M.insertWith (+) key 1

As for the generator, the basic idea is to first choose a prefix by random, and
then choose the preceding word by probability. This n word combination is then
used to create the next prefix, by chugging away the first word, and combining
the rest of the old prefix and the suffix into a new prefix. Repeat this until
we get long enough text, or some other heuristic tells us to stop.

As for our generator, the basic idea still stands, but how we choose the best
word needs a bit explaining. We have the word frequency for each prefix and we
can use this frequency by repeating each of those words $f$ times. Therefore if
there is a word that has been seen $10$ times versus a word that has been seen
$5$ times, there is double the chance to get the word with frequency $10$.

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


"that trees bordered the park every day. they called at various times of the
bequest as to admit them in those hours of transport we shall exactly know what
mrs. gardiner began to imagine that you should take it in whatever manner he
immediately began playing again. lady catherine one"
