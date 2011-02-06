import Text.Hakyll (hakyll)
import Control.Monad.Trans(liftIO)
import Text.Hakyll.Render(css)
import Text.Hakyll.File(directory, getRecursiveContents)
import Control.Monad(liftM)
import Data.List.Stream
import Prelude hiding (reverse, map, take)
import Text.Hakyll.CreateContext(createPage)
import Text.Hakyll.Render(renderChain)
import Text.Hakyll.CreateContext(createListing)
import Control.Monad(forM_)
import Text.Hakyll.Render(static)


main = hakyll "http://users.utu.fi/~machra" $ do
  postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
  let index = createListing "index.html" ["templates/postitem.html"]
	(take 3 postPages) [("title", Left "Home")]
      postPages = map createPage postPaths
      posts = createListing "posts.html" ["templates/postitem.html"]
	postPages [("title", Left "Home")]
  renderChain ["index.html", "templates/default.html"] index
  renderChain ["index.html", "templates/default.html"] posts
  forM_ postPages $ renderChain [ "templates/post.html", "templates/default.html"]
  directory css "css"
  directory static "static"
