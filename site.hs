{-# LANGUAGE OverloadedStrings, Arrows #-}

module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((***), (>>>), arr)
import Data.Monoid
import Hakyll.Main
import Hakyll.Core.Routes
import Hakyll.Core.Util.Arrow
import Hakyll.Core.Rules
import Hakyll.Core.Compiler
import Hakyll.Core.Writable.CopyFile
import Hakyll.Web.CompressCss
import Hakyll.Web.Template
import Hakyll.Web.RelativizeUrls
import Hakyll.Web.Page
import Hakyll.Web.Page.Metadata

main = hakyll $ do
  -- CSS
  route "css/*" idRoute
  compile "css/*" compressCssCompiler
  -- JS
  route "static/js/*" idRoute
  compile "static/js/*" copyFileCompiler
  -- JS-lang
  route "static/js/lang/*" idRoute
  compile "static/js/lang/*" copyFileCompiler
  -- Images
  route "static/img/*" idRoute
  compile "static/img/*" copyFileCompiler
  -- About
  route "about.markdown" $ setExtension ".html"
  compile "about.markdown" $ pageCompiler
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler
  -- Posts
  route "posts/*" $ setExtension ".html"
  compile "posts/*" $ pageCompiler
    >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
    >>> applyTemplateCompiler "templates/post.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler
  -- Index
  route "index.html" idRoute
  create "index.html" $ constA mempty
    >>> arr (setField "title" "Home")
    >>> requireAllA "posts/*" (id *** arr (take 3 . reverse . sortByBaseName) >>> addPostList)
    >>> applyTemplateCompiler "templates/index.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler
  -- Posts list
  route "posts.html" idRoute
  create "posts.html" $ constA mempty
    >>> arr (setField "title" "Posts")
    >>> requireAllA "posts/*" addPostList
    >>> applyTemplateCompiler "templates/index.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler
  compile "templates/*" templateCompiler
  where
    addPostList = setFieldA "posts" $
      arr (reverse . sortByBaseName)
	>>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
	>>> arr mconcat
	>>> arr pageBody
