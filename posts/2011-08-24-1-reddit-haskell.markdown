---
title: Wallpapers in Haskell
tags: haskell,network,io,binary
---

# What are we going to do?

I began learning Clojure and thought it would be feasible to start a series of
posts, seeing how different languages fare against some common problems. I'll
be writing a program that sets a random wallpaper from reddit. The advantages
of this problem is that it touches the following aspects:

- HTTP requests
- Saving files
- Binary IO
- System commands

We're downloading a random wallpaper from reddit. More precisely from
[r/wallpaper](http://www.reddit.com/r/wallpaper.json). Go ahead and `curl
http://www.reddit.com/r/wallpaper.json` to see it. The posts are quite deep in
there, but I've seen worse.

# Implementation

I'm starting with Haskell as it is the language I have most experience
(functional). The full code can be seen
[here](https://github.com/MasseR/RedditWallpaper).

I'll start with imports. It's normal in Haskell to have modules that do some
thing but does it well. This means that the modules are usually short and
concise, but that shows in import statements, as we have 17 import lines.


~~~~{.sourceCode .haskell}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (mzero)
import System.Directory (copyFile, removeFile)
import System.FilePath (splitFileName, takeExtension)
import System.Random (randomRIO)
import System.IO hiding (withFile)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EI
import Network.HTTP.Enumerator
import Control.Exception
import qualified System.Process as P
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Attoparsec.Enumerator
import Data.Enumerator (Iteratee)
import Data.ByteString (ByteString)
import Control.Applicative
import Data.List (isSuffixOf)
~~~~

Next I'm going to create the post datatype. A post consists of `over_18` tag,
`url` tag and `title` tag. Actually it has more fields, but these are somewhat
interesting. The `over_18` tag we need for filtering. The `url' tag we need for
downloading the image and the `title` tag was handy when first writing it.


~~~~{.sourceCode .haskell}
data Post = Post {
    nsfw :: Bool
  , uri :: String
  , title :: String
  } deriving Show
newtype Posts = Posts {getPosts :: [Post]} deriving Show
~~~~

For more information about json parsing in Haskell, see
[aeson](http://hackage.haskell.org/package/aeson) documentation.

The gist of this is that we're going to create instances for `FromJSON` type
class of our post and posts types. The type class describes how the aeson json
representation parses into our types. As you can see, we're using applicative
functors to do the parsing for us. Together with `.:` function, we get a nice
syntax for our parser.

The json has at some depth a `children` object, which contains a list of posts.
The posts have some metadata and the real post object is contained in the
`data` object. The `.:` has a signature of `FromJSON a => Object -> Text ->
Parser a` which means it can dynamically invoke the `FromJSON Post` instance
`parseJSON` function in the `Posts <$> v' .: "children"` line. Likewise when
building the post type, the calls to `.:` automatically get parsed to the
correct type.

~~~~{.sourceCode .haskell}
instance FromJSON Post where
  parseJSON (Object v) = do
    v' <- v .: "data"
    Post <$>
      v' .: "over_18" <*>
      v' .: "url" <*>
      v' .: "title"
  parseJSON _ = mzero

instance FromJSON Posts where
  parseJSON (Object v) = do
    v' <- v .: "data"
    Posts <$> v' .: "children"
  parseJSON _ = mzero
~~~~~

The next function is not strictly necessary, but it provides some extra
protection. The function writes first to a temporary file, which is then moved
to the correct path. In case something fails in the middle of writing, we don't
get invalid files in the real location. But this is as far as we go with
securing the program.

~~~~{.sourceCode .haskell}
withFile :: FilePath -> (Handle -> IO ()) -> IO ()
withFile path f = bracket
  (openTempFile "/tmp" "img.png")
  (\(p,h) -> hClose h >> copyFile p path >> removeFile p)
  (\(_,h) -> f h)
~~~~~

We're using [enumerator](http://hackage.haskell.org/package/enumerator) based
IO, which provides us a safer alternative for downloading the files. With
iteratees, we can be sure when the downloads have finished and can force the
downloading and writing happen at the same time. The `runIt` function takes a
request and an iteratee.

~~~~{.sourceCode .haskell}
runIt r iteratee = E.run_ . httpRedirect r iteratee
~~~~

The `download` function is where the downloads happen. First we parse the
filename out of the url. Then we create the request out of the url, and begin
the download. We now use the `withFile` that we wrote earlier by giving it the
filename as the first argument and the anonymous download function. The
anonymous function takes a handle and runs the request within a `withManager`
function. The `withManager` function does some IO magic behind the scenes.

~~~~{.sourceCode .haskell}
download :: String -> IO String
download url =
  let (_, filename) = splitFileName url
  in do
    request <- parseUrl url
    withFile filename $ \h -> withManager (runIt request (\_ _ -> EI.iterHandle h))
    return filename
~~~~~

The next function is the one that really changes the background. This I believe
is one of the pain points in Haskell. The `System.Process` module has all sorts
of functions to interact with the system, but their signatures and behaviour
change a lot. One moment we have a lazy function doing nothing and the next we
have a strict doing a lot and blocking at the same time. It might be just me,
but I hate the functions in there.

The `readProcess` takes a program name and a list of arguments. By having the
argument list as a list, helps preventing shell injections.

~~~~{.sourceCode .haskell}
makeBackground :: String -> IO ()
makeBackground path = P.readProcess "feh" ["--bg-scale", path] "" >> return ()
~~~~

The aeson parser is not easy to use. You need to manually call the attoparsec
parsers to parse the json into aeson json representation. The `iterParser`
function is from
[attoparsec-enumerator](http://hackage.haskell.org/package/attoparsec-enumerator)
which proves an enumerator based parser. So we basically just wrap the `json`
parser from aeson within an enumerator.

~~~~{.sourceCode .haskell}
json' :: (Monad m) => Iteratee ByteString m Value
json' = iterParser json
~~~~

The `posts` function downloads the json and parses it. It is similar to the
`download` function in many ways. It creates the request from the passed url
and then downloads the json using `withManager`, but this time the iteratee is
the `json'` parser.

~~~~{.sourceCode .haskell}
posts :: String -> IO (Result Posts)
posts url = do
  request <- parseUrl url
  fromJSON <$> withManager (runIt request (\_ _ -> json'))
~~~~~

As mentioned earlier, we're going to filter out all the posts that are marked
as nsfw. The `sfw` is a curried function that filters those that are not nsfw.

~~~~{.sourceCode .haskell}
sfw = filter (not . nsfw)
~~~~

I took the lazy way out, and didn't bother creating parsers for imgur, flickr,
tinypic etc. but instead, we're going to filter out all those links that are
not direct images. We accept only png, bmp and jpg images.

~~~~{.sourceCode .haskell}
img = filter ((`elem` filetypes) . takeExtension . uri)

filetypes :: [String]
filetypes = [".png", ".bmp", ".jpg"]
~~~~

Then comes the main. We fetch the posts, filter images, filter sfw and pluck
out the urls. If everything is okay at this point, we take a random integer
between 0 .. length - 1. This random integer is our index to the file we're
going to download and set as a background.

~~~~{.sourceCode .haskell}
main :: IO ()
main = do
  p <- fmap (map uri . sfw . img . getPosts) <$> posts "http://www.reddit.com/r/wallpaper.json"
  case p of
       Error x -> putStrLn x
       Success x -> do
         r <- randomRIO (0, length x - 1)
         let img = x !! r
         download img >>= makeBackground
~~~~~

# Thoughts

I wrote this a couple of months ago and since then I've written a lot of PHP
(too much?), looked into erlang and read a clojure book (well almost.). What
are my thoughts of this program now?

I also tried to write the program the right way. I set the mentality that I'm
going to use enumerators as much as possible. The advantages from the iteratee
based IO might not be apparent, but this is still the probably the smallest
experiment where overlapping IO is useful.

Also the [http-enumerator](http://hackage.haskell.org/package/http-enumerator)
is probably the best http client library for Haskell out there. Altough it's
highly unlikely I'm going to hit into situations with this program where
SSL/TLS support is needed.

I love the type system of Haskell, but it is clumsy when it comes to parsing
json. Haskell can't have generic lists/maps (I'm lying through my teeth here.
It is possible but not recommended and requires GHC extensions) so the json
notation doesn't fit naturally. The json types consist of objects and values
(and other types), where object is `Map Object Value` and Value is something
like `JSONString`, `JSONNum` etc. We get the type safety, and in theory could
do the parsing by pattern matching, but it's still difficult. To help this we
have the `FromJSON ` type class which handles conversions from Haskell json
representation to Haskell representation. This works and is not too difficult
(the queries map to other languages quite well), but is never as nice as a
syntax from a dynamic language can provide.

The system calls in this program work nicely, but the module has burned me
many, many times.

Other than that, the functional aspect is nice. The functions are nice, concice
and clear. Haskell's IO separation nicely separates the side-effectful code
from the pure code.

See also the [next](/posts/2011-08-24-2-reddit-clojure.html) post.
