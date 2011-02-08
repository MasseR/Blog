---
title: Hangman solver
---
> import qualified Data.List.Stream as L
> import Data.List
> import qualified Data.Map as M
> import Data.Map (Map)
> import qualified Data.Set as S
> import Data.Set (Set)
> import System.Environment

> wordFilter :: Set Char -> String -> [String] -> [String]
> wordFilter used t xs =
>   let w = L.filter (\w' -> length w' == length t) xs
>       w' = L.filter (L.all (\(a,b) -> (a == b) || (a == '_')) . L.zip t ) w
>       w'' = L.filter (\x -> not (L.any (\y -> y `S.member` used) x)) w'
>   in w''

> wordFreq :: Set Char -> String -> [String] -> [(Char, Int)]
> wordFreq used t xs =
>   let filtered = wordFilter used t xs
>       u = nub $ L.filter (/= '_') t
>       xs' = M.unionsWith (+) $ L.map (L.foldl' (add u) (M.empty)) filtered
>       xs'' = M.filterWithKey (\k _ -> not $ k `S.member` used) xs'
>   in M.assocs xs'
>   where
>     add u a b | not (b `elem` u) = M.insertWith' (\_ o -> o+1) b 1 a
> 	      | otherwise = a

> bestGuess :: [(Char, Int)] -> Char
> bestGuess x =
>   let sorted = L.reverse $ L.sort $ L.map swap x
>   in snd $ head sorted
>   where swap (a,b) = (b,a)

> tryGuess :: Char -> String -> String -> (Bool, String)
> tryGuess g origin guess | g `L.elem` origin =
>   let updateWord = L.map (\(a,b) -> if a == g then a else b) $ L.zip origin guess
>   in (True, updateWord)
> 			| otherwise = (False, guess)

> --ai :: String -> IO ()
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
>   [x] <- getArgs
>   dict <- words `fmap` readFile "/usr/share/dict/words"
>   ai x dict
