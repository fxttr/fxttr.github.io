{-# LANGUAGE OverloadedStrings  #-}

module Main (main) where

import Hakyll
import Config (config)
import Lib (postCtx, xelatex, pdfToPng, feedConfiguration, feedCtx)

main :: IO ()
main = hakyllWith config $ do
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match ("images/*.jpg" .||. "images/*.png") $ do
        route idRoute
        compile copyFileCompiler

    match "images/*.tex" $ do
        route   $ setExtension "png"
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/formula.tex" defaultContext
            >>= xelatex >>= pdfToPng

    match "css/*" $ compile compressCssCompiler
    create ["style.css"] $ do
        route idRoute
        compile $ do
            csses <- loadAll "css/*.css"
            makeItem $ unlines $ map itemBody csses

    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ do
            ((pandocCompiler
                >>= saveSnapshot "content") >>= loadAndApplyTemplate "templates/post.html" (postCtx tags) . fmap demoteHeaders)
                >>= loadAndApplyTemplate "templates/content.html" defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let ctx = constField "title" "Posts" <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/content.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/content.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

        version "rss" $ do
            route   $ setExtension "xml"
            compile $ loadAllSnapshots pattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss (feedConfiguration title) feedCtx

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 4) . recentFirst =<< loadAll "posts/*"
            let indexContext = listField "posts" (postCtx tags) (return posts) <> field "tags" (\_ -> renderTagList tags) <> defaultContext
            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/content.html" indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    match (fromList pages) $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/content.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "404.html" $ do
        route idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/content.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
        
    where
        pages =
            [ "contact.markdown" ]