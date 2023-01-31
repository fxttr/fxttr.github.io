{-# LANGUAGE OverloadedStrings  #-}

module Main (main) where

import Hakyll
import Config (config)
import Lib (postCtx, xelatex, pdfToPng)

main :: IO ()
main = hakyllWith config $ do
    match "images/*.jpg" $ do
        route idRoute
        compile copyFileCompiler

    match "images/*.tex" $ do
        route   $ setExtension "png"
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/formula.tex" defaultContext
            >>= xelatex >>= pdfToPng

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ do
            ((pandocCompiler
                >>= saveSnapshot "content") >>= loadAndApplyTemplate "templates/post.html" (postCtx tags) . fmap demoteHeaders)
                >>= loadAndApplyTemplate "templates/content.html" defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

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