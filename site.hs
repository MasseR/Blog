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
import Text.Hakyll.Paginate
import Control.Arrow ((>>>))


main = hakyll "http://users.utu.fi/machra" $ do
  -- CSS
  route "css/*" idRoute
  compile "css/*" compressCssCompiler
  -- JS
  route "static/js/*/**" idRoute
  compile "static/js/*/**" copyFileCompiler
  -- Images
  route "static/img/*/**" idRoute
  compile "static/img/*/**" copyFileCompiler
  -- Posts
  route "posts/*" $ setExtension ".html"
  compile "posts/*" $ pageCompiler
    >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
    >>> applyTemplateCompiler "templates/post.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler
  -- Index
  route "index.html" idRoute
  compile "index.html" $ constA mempty
    >>> arr (setField "title" "Home")
    >>> requireAllA "posts/*" (id *** arr (take 3 . reverse . sortByBaseName) >>> addPostList)
    >>> applyTemplateCompiler "templates/index.html"
    >>> applyTemplateCompiler "templates/default.html"
  -- Posts list
  route "posts.html" idRoute
  compile "posts.html" $ constA mempty
    >>> arr (setField "title" "Posts")
    >>> requireAllA "posts/*" addPostList
    >>> applyTemplateCompiler "templates/index.html"
    >>> applyTemplateCompiler "templates/default.html"
  compile "templates/*" templateCompiler
  where
    addPostList = setFieldA "posts" $
      arr (reverse . sortByBaseName)
	>>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
	>>> arr mconcat
	>>> arr pageBody
