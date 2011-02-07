import Control.Monad(forM_)
import Control.Monad(liftM)
import Control.Monad.Trans(liftIO)
import Data.List.Stream
import Prelude hiding (reverse, map, take)
import Text.Hakyll (hakyll)
import Text.Hakyll.ContextManipulations (renderDate)
import Text.Hakyll.CreateContext(createPage, createListing, combine)
import Text.Hakyll.File(directory, getRecursiveContents)
import Text.Hakyll.Render(css, renderChain, static)
import Control.Arrow ((>>>))


renderPostList url title posts = do
  let list = createListing url ["templates/postitem.html"] posts [("title", Left title)]
  renderChain ["index.html", "templates/default.html"] list

postManipulation = renderDate "date" "%B %e, %Y" "Unknown date"
main = hakyll "http://users.utu.fi/~machra" $ do
  postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
  queuePaths <- getRecursiveContents "queue"
  let
      renderablePosts = map ((>>> postManipulation) . createPage) postPaths
      renderableQueue = map createPage queuePaths
      about = createPage "about.markdown"
  renderChain ["templates/default.html"] about
  renderPostList "index.html" "Home" (take 3 renderablePosts)
  renderPostList "posts.html" "All posts" renderablePosts
  renderPostList "queue.html" "Queue" renderableQueue
  forM_ renderablePosts $ renderChain [ "templates/post.html", "templates/default.html"]
  forM_ renderableQueue $ renderChain [ "templates/post.html", "templates/queue.html", "templates/default.html"]
  directory css "css"
  directory static "static"
