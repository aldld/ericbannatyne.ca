--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Data.List (isSuffixOf)
import           System.FilePath.Posix (takeBaseName, takeDirectory, (</>), splitFileName)
import           Text.Pandoc.Options


--------------------------------------------------------------------------------

-- Route for "nice" URLs omitting the .html suffix.
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
    where createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
            where p = toFilePath ident

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
    where
        idx = "index.html"

main :: IO ()
main = hakyllWith config $ do

    -- Static files
    match ("favicon.ico" .||. "images/**" .||. "files/**") $ do
        route idRoute
        compile copyFileCompiler

    -- CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Blog posts
    match "posts/*" $ do
        let pandocOptions = defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax "" }
        route $ cleanRoute
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/blog.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

    -- Blog index
    create ["blog.html"] $ do
        route cleanRoute
        compile $ do
            posts <- fmap (take 3) . recentFirst =<< loadAll "posts/*"
            let blogIndexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Eric Bannatyne's Blog"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog-index.html" blogIndexCtx
                >>= loadAndApplyTemplate "templates/blog.html"    blogIndexCtx
                >>= loadAndApplyTemplate "templates/default.html" blogIndexCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    -- Blog archive
    create ["archive.html"] $ do
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archive"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/blog.html"    archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
                >>= cleanIndexUrls


    -- Home page
    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx =
                    constField "title" "Eric Bannatyne"                `mappend`
                    defaultContext

            getResourceBody
                >>= loadAndApplyTemplate "templates/page.html"    indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= applyAsTemplate indexCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    -- 404 page - *without* relativization.
    match "404.html" $ do
        route idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext

    -- Template files
    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

--------------------------------------------------------------------------------

-- Deploy configuration
config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --checksum -arv _site/* \
            \aldld@aldld.webfactional.com:webapps/ericbannatyne_app"
    }
