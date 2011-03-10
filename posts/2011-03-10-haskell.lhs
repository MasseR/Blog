---
title: Haskell
tags: haskell
---

I once read a great metaphor for functional programs. It said that
functional programs are like conveyor belts, taking data, modifying it and
returning it. Yesterday evening I felt like manipulating some files with
structured data and it was an excellent situation for pure functions.

The file I was molding was the history file from [uzbl](http://uzbl.org)
browser. It is a simple plaintext format with date, url and title. I wanted
to read the file, list the url histogram, list the domain histogram and
print some statistics.

> {-# LANGUAGE OverloadedStrings #-}
> import Data.List (foldl', sort)
> import Data.Text.Lazy (Text)
> import qualified Data.ByteString.Lazy as BL (ByteString(..), readFile)
> import qualified Data.Map as M (assocs, insertWith', empty)
> import qualified Data.Text.Lazy as T (words, lines, unpack, pack, append)
> import qualified Data.Text.Lazy.Encoding as TE (decodeUtf8)
> import qualified Data.Text.Lazy.IO as TI (putStrLn)
> import Network.URI(parseURI, uriAuthority, uriRegName)

This was just a quick script so I hardcoded the file. The proper path is
`$XDG_DATA_HOME/uzbl/history` but this is fine enough for now.

> file ::  FilePath
> file = "history"

"2011-03-10 22:24:21 http://localhost:8000/posts/2011-03-10-haskell.html
blog :: MasseR -> IO (Haskell)" is an example of a line in the history
file. If separated by words, the url is the third word. Therefore we take
the `ByteString`, decode it into `Text`, separate it to lines, and map with
a lambda function which turns each line into words, drops the first two
words and takes the third.

> urls ::  BL.ByteString -> [Text]
> urls = map (head . drop 2 . T.words) . T.lines . TE.decodeUtf8

The next function takes a list of `Texts` which contain urls, and reduces
those into a single `Map`. The map contains a histogram of each url, but as
there is nothing url specific, it would work with any `Text`. This map is
then given to `assocs` which returns an association list. The next function
flips the association for each text. These are then sorted and returned.

> histogram ::  [Text] -> [(Int, Text)]
> histogram = sort . map (uncurry (flip (,))) . M.assocs . foldl' (\m w -> M.insertWith' (+) w 1 m) M.empty

The `path'` function works in a `Maybe` monad with do-notation. The
parseURI and uriAuthority both returns values wrapped in a `Maybe`, which
makes a suitable use of a monad.

> path' :: Text -> Maybe Text
> path' url = do
>   uri <- parseURI $ T.unpack url
>   authority <- uriAuthority uri
>   return $ T.pack $ uriRegName authority

This function just maps through every url with the function `path`'
(separating domains), filters out invalid urls and then lifts the values as
pure values.

> paths ::  [Text] -> [Text]
> paths = map fromJust . filter (isJust) . map path'

Both of these functions are found in Data.Maybe, but were simple enough for
me to writer write them myself than import them.

> fromJust ::  Maybe t -> t
> fromJust (Just x) = x

> isJust ::  Maybe t -> Bool
> isJust (Just _) = True
> isJust _ = False

The `formatHistogram` function takes a `(Int, Text)` and tab-separates them
suitable for printing out. For example `(100, "www.reddit.com")` would turn
into "100 www.reddit.com".

> formatHistogram :: (Int, Text) -> Text
> formatHistogram (a,b) = T.pack (show a) `T.append` "\t" `T.append` b

> main :: IO ()
> main = do
>   c <- BL.readFile file
>   let uniqueurls = histogram $ urls c
>       uniquepaths = histogram $ paths $ urls c
>   mapM_ (TI.putStrLn . formatHistogram) uniquepaths
>   TI.putStrLn $ "Unique urls: " `T.append` (T.pack $ show (length uniqueurls))
>   TI.putStrLn $ "Unique domains: " `T.append` (T.pack $ show (length uniquepaths))
>   return ()

