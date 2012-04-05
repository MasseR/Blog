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
import Hakyll.Core.Identifier.Pattern (list)
import Hakyll.Core.Rules
import Hakyll.Core.Compiler
import Hakyll.Core.Writable.CopyFile
import Hakyll.Web.CompressCss
import Hakyll.Web.Template
import Hakyll.Web.Urls.Relativize
import Hakyll.Web.Feed
import Hakyll.Web.Page
import Hakyll.Web.Page.List
import Hakyll.Web.Page.Metadata
import Text.HTML.TagSoup


main = hakyllWith config $ do
  let defaultTemplate = "templates/rs19.html"
  -- CSS
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  match "static/**/*" $ do
    route idRoute
    compile copyFileCompiler
  match "intro.markdown" $ compile pageCompiler
  match (list ["contact.markdown", "about.markdown"]) $ do
    route $ setExtension "html"
    compile $ pageCompiler
      >>> introCompiler
      >>> applyTemplateCompiler defaultTemplate
      >>> relativizeUrlsCompiler
  -- Posts
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pageCompiler
      >>> introCompiler
      >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
      >>> arr (renderDateField "month" "%B" "Date unknown")
      >>> arr (renderDateField "day" "%d" "Date unknown")
      >>> arr (copyBodyToField "description")
      >>> arr (renderField "description"  "excerpt" excerpt)
      >>> applyTemplateCompiler "templates/rs19post.html"
      >>> applyTemplateCompiler defaultTemplate
      >>> relativizeUrlsCompiler
  match (list ["index.html", "posts.html"]) $ route idRoute
  -- Index
  match "index.html" $ route idRoute
  create "index.html" $ constA mempty
    >>> top5 "templates/rs19teaser.html"  "posts"
    >>> arr (setField "title" "Home")
    >>> postCompiler "templates/index.html" defaultTemplate
  -- Posts list
  match "posts.html" $ route idRoute
  create "posts.html" $ constA mempty
    >>> setFieldPageList (recentFirst) "templates/rs19teaser.html" "posts" "posts/*"
    >>> arr (setField "title" "Posts")
    >>> postCompiler "templates/rs19posts.html" defaultTemplate
  match "templates/*" $ compile templateCompiler
  match "rss.xml" $ route idRoute
  create "rss.xml" $ requireAll_ "posts/*" >>> renderRss feedConfiguration
  where
    top5 template key = setFieldPageList (take 5 . recentFirst) template key "posts/*"
    introCompiler = requireA "intro.markdown" (setFieldA "intro" $ arr pageBody)
    postCompiler tpl roottpl = introCompiler
      >>> applyTemplateCompiler tpl
      >>> applyTemplateCompiler roottpl
      >>> relativizeUrlsCompiler
    config = defaultHakyllConfiguration { deployCommand = "rsync --checksum --delete -avz -e ssh _site/ machra@linux.utu.fi:/www/users/m/machra/ --exclude ostoslista" }
    feedConfiguration = FeedConfiguration {
        feedTitle         = "Masse's blog"
        , feedDescription = "Blog about functional programming and all around it"
        , feedAuthorName  = "Mats Rauhala"
        , feedRoot        = "http://users.utu.fi/machra"
      }

excerpt :: String -> String
excerpt = renderTags . tail .
  takeWhile (~/= TagClose ("p" :: String)) .
    dropWhile (~/= TagOpen ("p" :: String) []) .
      parseTags
