{-# LANGUAGE OverloadedStrings, Arrows #-}

module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((***), (>>>), arr)
import Data.Monoid
import Data.List
import Hakyll.Main
import Hakyll.Core.Routes
import Hakyll.Core.Configuration (defaultHakyllConfiguration, deployCommand)
import Hakyll.Core.Util.Arrow
import Hakyll.Core.Rules
import Hakyll.Core.Compiler
import Hakyll.Core.Writable.CopyFile
import Hakyll.Web.CompressCss
import Hakyll.Web.Template
import Hakyll.Web.RelativizeUrls
import Hakyll.Web.Page
import Hakyll.Web.Page.List
import Hakyll.Web.Page.Metadata


main = hakyllWith config $ do
  -- CSS
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  -- JS
  match "static/js/*" $ do
    route idRoute
    compile copyFileCompiler
  -- JS-lang
  match "static/js/lang/*" $ do
    route idRoute
    compile copyFileCompiler
  -- Images
  match "static/img/*" $ do
    route idRoute
    compile copyFileCompiler
  -- About
  match "about.markdown" $ do
    route $ setExtension "html"
    compile $ pageCompiler
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler
  -- Posts
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pageCompiler
      >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
      >>> applyTemplateCompiler "templates/post.html"
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler
  -- Index
  match "index.html" $ route idRoute
  create "index.html" $ constA mempty
    >>> arr (setField "title" "Home")
    >>> setFieldPageList (take 5 . recentFirst) "templates/postitem.html" "posts" "posts/*"
    >>> applyTemplateCompiler "templates/index.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler
  -- Posts list
  match "posts.html" $ route idRoute
  create "posts.html" $ constA mempty
    >>> arr (setField "title" "Posts")
    >>> setFieldPageList (recentFirst) "templates/postitem.html" "posts" "posts/*"
    >>> applyTemplateCompiler "templates/posts.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler
  match "templates/*" $ compile templateCompiler
  where
    config = defaultHakyllConfiguration { deployCommand = "rsync --checksum --delete -avz -e ssh _site/ machra@linux.utu.fi:/www/users/m/machra/ --exclude ostoslista" }
