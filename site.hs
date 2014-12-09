{-# Language OverloadedStrings #-}

import Hakyll
import Data.Monoid
import Control.Applicative

main :: IO ()
main = hakyll $ do
  match "templates/*" $ do
    compile templateCompiler
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  match "static/*/**" $ do
    route idRoute
    compile copyFileCompiler
  match "posts/*" $ do
    route $ setExtension ".html"
    compile $
      pandocCompiler >>=
      loadAndApplyTemplate "templates/post.html" defaultContext >>=
      loadAndApplyTemplate "templates/default.html" defaultContext >>=
      relativizeUrls
  match "about.markdown" $ do
    route $ setExtension ".html"
    compile $
      pandocCompiler >>=
      loadAndApplyTemplate "templates/default.html" defaultContext >>=
      relativizeUrls
  create ["posts.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let ctx = listField "posts" defaultContext (return posts)
                <> constField "title" "Posts"
                <> defaultContext
      makeItem "" >>=
        loadAndApplyTemplate "templates/posts.html" ctx >>=
        loadAndApplyTemplate "templates/default.html" ctx >>=
        relativizeUrls
  create ["index.html" ]$ do
    route idRoute
    compile $ do
      posts <- take 3 <$> (recentFirst =<< loadAll "posts/*")
      let ctx = constField "title" "Home"
                <> listField "posts" defaultContext (return posts)
                <> defaultContext
      makeItem []
        >>= loadAndApplyTemplate "templates/index.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
