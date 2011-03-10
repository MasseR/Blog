---
title: Concatenating bytes
tags: haskell,binary
---

In an assignment from school we are supposed to do Huffman
encoding/decoding. Basically it works by creating a histogram of symbols
and creating a binary tree so that the most frequent symbols have the
shortest path from root. The path becomes the new value for the symbol

For example if we have a byte 1011, and the symbol would be the first right
node from the root. The new byte would be 1. I came up with a simple and
beautiful, albeit slow solution for concatenating these paths into complete
8-bit wide bytes.

> {-# LANGUAGE BangPatterns #-}
> import Data.Word
> import Data.Bits
> import Data.List (foldl')

> concatBit :: [Word8] -> [Word8]
> concatBit = reverse . step []
>   where
>     step !binary [] = binary
>     step !binary !x =
>       let !byte = {-# SCC "byte" #-} snd $ foldl' (\(s,a) x -> (s-1, a .|. shift x s)) (7,0) $ take 8 (x ++ repeat 0)
>           !appended = {-# SCC "append" #-} byte : binary
>           rest = drop 8 x
>       in {-# SCC "step" #-} step appended rest

Like I said before however, this is slow and I was thinking of having a map
with symbols as keys and incomplete bytes as values. These incomplete bytes
would then be concatenated into complete bytes. I had a couple of solutions
for this concatenation, but I wasn't happy with any of them. They all
seemed way too complex and error prone.

I will write a follow-up when I have thought up a solution.
