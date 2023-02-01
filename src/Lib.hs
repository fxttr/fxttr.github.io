module Lib (postCtx, feedCtx, feedConfiguration, xelatex, pdfToPng) where
import Hakyll
import qualified System.Process as Process
import System.FilePath (replaceExtension, takeDirectory)

postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , Context $ \key -> case key of
        "title" -> unContext (mapContext escapeHtml defaultContext) key
        _       -> unContext mempty key
    , defaultContext
    ]

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , Context $ \key -> case key of
        "title" -> unContext (mapContext escapeHtml defaultContext) key
        _       -> unContext mempty key
    , defaultContext
    ]

xelatex :: Item String -> Compiler (Item TmpFile)
xelatex item = do
    TmpFile texPath <- newTmpFile "xelatex.tex"
    let tmpDir  = takeDirectory texPath
        pdfPath = replaceExtension texPath "pdf"

    unsafeCompiler $ do
        writeFile texPath $ itemBody item
        _ <- Process.system $ unwords ["xelatex", "-halt-on-error",
            "-output-directory", tmpDir, texPath, ">/dev/null", "2>&1"]
        return ()

    makeItem $ TmpFile pdfPath

pdfToPng :: Item TmpFile -> Compiler (Item TmpFile)
pdfToPng item = do
    let TmpFile pdfPath = itemBody item
        pngPath         = replaceExtension pdfPath "png"
    unsafeCompiler $ do
        _ <- Process.system $ unwords
            ["convert", "-density", "150", "-quality", "90", pdfPath, pngPath]
        return ()
    makeItem $ TmpFile pngPath

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "fxttr - " ++ title
    , feedDescription = "Personal blog of fxttr"
    , feedAuthorName  = "Florian Buestgens"
    , feedAuthorEmail = "fb@fx-ttr.de"
    , feedRoot        = "https://fx-ttr.de"
    }