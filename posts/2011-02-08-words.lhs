---
title: Hangman solver
---

Hangman is a game where people try to guess a word. A word is picked at random
and is then hidden with underscores. People then try to guess letters that
might fit the word and from wrong letters, you gradually start to draw the
gallows and eventually a stick figure hanging from it. If the drawing is
complete before the word is guessed, the round has been lost. If the letter was
in the original word, all places where the letter would be are filled.

Many Linux distributions comes with a file in `/usr/share/dict/words`
containing a dictionary for the selected language, sometimes multiple. We are
lucky as this file is quite big and can help us test algorithms with a big
dataset and this time we're going to create an "ai" for guessing the letters in
hangman.

By collecting what we know, we can devise an algorithm that can somewhat
intelligently guess the letters. The basic idea is that we're going to create a
histogram of all the letters and choosing the best from among them. The dataset is quite big (almost 500 000 words on Fedora), so we need to prune it somehow.

Since the original word can be seen as underscores, also the length can be
calculated. By pruning by length as the first stage we can shrink the effective
dataset a bit. Considering that within 500 000 words there is not much deviation with lengths, we shouldn't stop here.

When doing the first guess pruning by length is the best we can do, but after
doing the first guess we have either found our first letters in the word, or
the first wrong word. Therefore the next step is obviously pruning the words
containing the failed letters and pruning the words that do not match the
target word. For example if we have failed with letter 'a', we should no longer
try words "car" or "bat". Likewise if we have succesfully guessed with letters
'a' and 'r' and our word is currently "_ar" we should prune the word "rat".

The next program shows that this algorithm isn't enough. During the endgame the
histogram might be of length 1 for all the letters. If this happens the program
just checks linearly every possible letter until it either finds the right
letter or goes to the end. A better algorithm might be choosing a letter at
random, favouring those with higher rank. See the [previous
post](/posts/2011-02-05-hellovector.html) or the [probability
package](http://hackage.haskell.org/package/probability) for how to do this.

> import qualified Data.List.Stream as L
> import Data.List
> import qualified Data.Set as S
> import Data.Set (Set)
> import System.Environment
> import Control.Applicative

The `wordFilter` function filters first by length, then by target word and then
by failed letters. The `Set` is used for keeping track of the failed letters.

> wordFilter :: Set Char -> String -> [String] -> [String]
> wordFilter used t xs =
>   let byLength = L.filter (\w' -> length w' == length t) xs
>       byWord = L.filter (L.all (\(a,b) -> (a == b) || (a == '_')) . L.zip t ) byLength
>       byFail = L.filter (\x -> not (L.any (\y -> y `S.member` used) x)) byWord
>   in byFail

The `wordFreq` function calculates the letter frequencies. First we take the
unique characters of the target word which are then filtered out later when
calculating frequencies. The basic idea is that we concatenate the filtered
words, sort the characters, group them and calculate their lengths.

> wordFreq :: Set Char -> String -> [String] -> [(Int, Char)]
> wordFreq used t xs =
>   let filtered = wordFilter used t xs
>       u = nub $ L.filter (/= '_') t
>	assocs = L.map (liftA2 (,) L.length L.head) $
>		    L.group $ L.sort $ L.filter
>		    (not . flip L.elem u) $ L.concat filtered
>   in assocs

The `bestGuess` function takes the head of the sorted frequency list and
returns the character it is associated with.

> bestGuess :: [(Int, Char)] -> Char
> bestGuess x =
>   let (_, best) = L.head $ L.sortBy (\(a,_) (b,_) -> compare b a) x
>   in best

The `tryGuess` function tries to fit the letter into the word. If the letter
exists in the word it the word is iterated with the original word. If the
letters exist in the original word, they are inserted into the word being
guessed.

> tryGuess :: Char -> String -> String -> (Bool, String)
> tryGuess g origin guess | g `L.elem` origin =
>   let updateWord = L.map (\(a,b) -> if a == g then a else b) $ L.zip origin guess
>   in (True, updateWord)
> 			| otherwise = (False, guess)

The next function glues everything together. It loops through the dataset until
the word has been guessed, or errors out because of empty list when no word is
found. This is a prime location to implement more game logic such as limiting
the false tries.

> ai :: String -> [String] -> IO ()
> ai x dict =
>   let originalSearch = L.map (\_ -> '_') x
>       originalUsed = S.empty
>   in go x originalSearch originalUsed dict
>   where go origin guess used dict | origin /= guess = do
> 	  putStrLn guess
> 	  let g = bestGuess freq
> 	      freq = wordFreq used guess dict
> 	      (match, new) = tryGuess g origin guess
> 	      newUsed = if match then used else (S.insert g used)
> 	  putStrLn $ "Trying '" ++ [g] ++ "'"
> 	  putStrLn $ "The resulting word is: " ++ new
> 	  go origin new newUsed dict
> 				  | otherwise = return ()

> main = do
>   x <- fmap target getArgs
>   dict <- words `fmap` readFile "/usr/share/dict/words"
>   ai x dict
>   where target (x:xs) = x
>	  target [] = "television"
