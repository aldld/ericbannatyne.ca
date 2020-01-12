--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Monoid (mappend)
import Hakyll
import Data.List (isSuffixOf)
import System.FilePath.Posix (takeBaseName, takeDirectory, (</>))
import Text.Pandoc.Options

--------------------------------------------------------------------------------

-- Route for "nice" URLs omitting the .html suffix.
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
      takeDirectory p </> takeBaseName p </> "index.html"
      where p = toFilePath ident

-- Route to remove "pages/" prefix from the URL.
removePagesRoute :: Routes
removePagesRoute = gsubRoute "pages/" (const "")

-- Replace URLs in content with clean URLs.
cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
  where
    pattern = "/index.html"
    replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
  | idx `isSuffixOf` url = take (length url - length idx) url
  | otherwise            = url
  where idx = "index.html"

customPandocCompiler :: Compiler (Item String)
customPandocCompiler = do
  csl <- load $ fromFilePath "assets/acm-sig-proceedings-long-author-list.csl"
  bib <- load $ fromFilePath "assets/citations.bib"
  fmap write (getResourceString >>= read csl bib)
  where
    read = readPandocBiblio defaultHakyllReaderOptions
    write = writePandocWith writerOptions

compressScssCompiler :: Compiler (Item String)
compressScssCompiler =
  getResourceString
    >>= withItemBody (unixFilter "sass" ["--stdin", "--style=compressed"])
    >>= return . fmap compressCss

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax "" }

main :: IO ()
main = hakyllWith config $ do

  -- Static files
  match ("images/**" .||. "files/**") $ do
    route idRoute
    compile copyFileCompiler

  -- Bibliography management
  match "assets/*.bib" $ compile biblioCompiler
  match "assets/*.csl" $ compile cslCompiler

  -- CSS
  match "css/*.css" $ do
    route idRoute
    compile compressCssCompiler

  -- SCSS
  match "css/*.scss" $ do
    route $ setExtension "css"
    compile compressScssCompiler

  -- JavaScript
  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  -- Blog posts
  match "posts/*" $ do
    let pandocOptions = defaultHakyllWriterOptions {
      writerHTMLMathMethod = MathJax ""
    }

    route $ cleanRoute
    compile $ customPandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/blog.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls
      >>= cleanIndexUrls

  -- Blog post drafts
  match "drafts/*" $ do
    let pandocOptions = defaultHakyllWriterOptions {
      writerHTMLMathMethod = MathJax ""
    }

    route $ cleanRoute
    compile $ customPandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/blog.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls
      >>= cleanIndexUrls

  -- Blog index
  create ["blog.html"] $ do
    route cleanRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let
        blogIndexCtx = listField "posts" postCtx (return posts) `mappend`
          constField "title" "Eric Bannatyne's Blog" `mappend`
          globalCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/blog-index.html" blogIndexCtx
        >>= loadAndApplyTemplate "templates/blog.html" blogIndexCtx
        >>= loadAndApplyTemplate "templates/default.html" blogIndexCtx
        >>= relativizeUrls
        >>= cleanIndexUrls

  -- Home page
  match "pages/index.html" $ do
    --route idRoute
    route removePagesRoute
    compile $ do
      let indexCtx = constField "title" "Eric Bannatyne" <> globalCtx

      getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= applyAsTemplate indexCtx
        >>= relativizeUrls
        >>= cleanIndexUrls

  -- About page
  match "pages/about.html" $ do
    route $ removePagesRoute `composeRoutes` cleanRoute
    compile $ do
      let
        aboutCtx = constField "title" "About &ndash; Eric Bannatyne"
          <> globalCtx

      getResourceBody
        >>= loadAndApplyTemplate "templates/page.html" aboutCtx
        >>= loadAndApplyTemplate "templates/default.html" aboutCtx
        >>= applyAsTemplate aboutCtx
        >>= relativizeUrls
        >>= cleanIndexUrls

  -- Misc page
  match "pages/misc.md" $ do
    route $ removePagesRoute `composeRoutes` cleanRoute
    compile $ do
      let
        miscCtx = constField "title" "Misc &ndash; Eric Bannatyne" <> globalCtx

      pandocCompiler
        >>= loadAndApplyTemplate "templates/page.html" miscCtx
        >>= loadAndApplyTemplate "templates/default.html" miscCtx
        >>= applyAsTemplate miscCtx
        >>= relativizeUrls
        >>= cleanIndexUrls

  -- 404 page - *without* relativization.
  match "pages/404.html" $ do
    route removePagesRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html" globalCtx
      >>= loadAndApplyTemplate "templates/default.html" globalCtx

  -- Template files
  match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------

globalCtx :: Context String
globalCtx = defaultContext

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext

--------------------------------------------------------------------------------

-- Deploy configuration
config :: Configuration
config = defaultConfiguration
  { deployCommand =
    "netlify deploy --dir=_site --prod --message='Hakyll deploy'"
  }

