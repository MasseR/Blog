import Text.Hakyll (hakyll)
import Control.Monad.Trans(liftIO)
import Text.Hakyll.Render(css, renderChain, static)
import Text.Hakyll.File(directory, getRecursiveContents)
import Control.Monad(liftM)
import Data.List.Stream
import Prelude hiding (reverse, map, take)
import Text.Hakyll.CreateContext(createPage, createListing, combine)
import Control.Monad(forM_)


main = hakyll "http://users.utu.fi/~machra" $ do
  postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
  let index = createListing "index.html" ["templates/postitem.html"]
	(take 3 postPages) [("title", Left "Home")]
      postPages = map createPage postPaths
      posts = createListing "posts.html" ["templates/postitem.html"]
	postPages [("title", Left "Home")]
      about = createPage "about.markdown"
  renderChain ["index.html", "templates/default.html"] index
  renderChain ["index.html", "templates/default.html"] posts
  renderChain ["templates/default.html"] about
  forM_ postPages $ renderChain [ "templates/post.html", "templates/default.html"]
  directory css "css"
  directory static "static"
