---
title: Lunchtime coding: Unigram calculator
tags: code, haskell, snippets
---

After unfulfillingly completed a unigram probability skeleton during
demonstrations, I decided to port the python version to Haskell. I'm not going
to show the python version, as it's not mine, but the Haskell version came out
nice.

As some of you may know, I'm studying in the university of Turku. In this
period I'm taking a course called 'Johdatus kielenkäsittelyn perusteisiin',
loosely translated as 'Introduction to basics of natural language processing'.
Last week, we were lectured about
[N-grams](https://en.wikipedia.org/wiki/N-gram), and how they can be used to
calculate probabilities.

For calculating the probabilities, we can use the equation
$P(W_1^n)=\prod_{k=1}^nP(w_k | w_{k-N+1}^{k-1}$, or more generally for unigrams
$P(w_1^n)=\prod_{k=1}^nP(w_k)$. The latter equation is the equation used in the
template python code, and we were supposed to modify it so that it uses add one
smoothing. For add one smoothing, we assume that for every word, the word count
is $w+1$. This helps us in the situation where we haven't seen the word at all,
making the word count $1$. The equation then turns into
$P(w)=C(w)+1/\sum_iC(w_i)+V$ where V is the vocabulary.

> module Main where

> import qualified Data.Map as M
> import qualified Data.Text.Lazy as T
> import Data.Text.Lazy (Text)
> import qualified Data.Text.Lazy.IO as TI
> import Data.List (foldl')
> import Data.Maybe (maybe)
> import System.Environment (getArgs)
> import Control.Applicative

> type Unigram = M.Map Text Int
> type Corpus = Text

Training is simple with unigrams, you only need the word histograms. Nice and
easy with `M.insertWith' (+) w 1 m`. On top of that I filter out all the words
that are less than three characters. You can think of it as a cheap stop word
list.

> train :: Corpus -> Unigram
> train = foldl' go M.empty . filter (\x -> T.length x > 3) . T.words
>   where go m w = M.insertWith' (+) w 1 m

Calculating the score is a little bit more involved. For the vocabulary we can
just take the number of unique words. For the total number of words, we can
just sum the word counts. Another important note is how we are calculating the
product. We use the logarithm rules $\log{a/b}=\log{a}-\log{b}$ and
$\log{(a*b)}=\log{a}+\log{b}$. I'm not entirely clear for the reasons for this,
but another student said that "it's better when accumulating products".

> score :: Unigram -> Corpus -> Double
> score unigram = exp . foldl' go 0 . T.words
>   where
>     vocabulary = length $ M.keys unigram
>     total = M.fold (+) 0 unigram
>     divisor = log (fromIntegral $ total + vocabulary)
>     go acc token = let count = maybe 1 (+1) $ M.lookup token unigram
>                    in acc + log (fromIntegral count) - divisor

> main = do
>   let trainfile = "data7_1.txt"
>       files = ["data7_test1.txt", "data7_test2.txt"]
>   unigram <- (train . T.toLower) <$> TI.readFile trainfile
>   scores <- map (score unigram . T.toLower) <$> mapM TI.readFile files
>   print scores

