{-# LANGUAGE OverloadedStrings #-}

import Data.Char (toLower)
import Data.Functor.Identity (runIdentity)
import qualified Data.List as L
import qualified Data.Text as T
import Numeric (showFFloat)
import System.Directory (doesFileExist, getFileSize)
import System.FilePath (takeExtension, takeFileName)

import Text.HTML.TagSoup (Tag(TagOpen))
import Text.Pandoc.Definition (Inline(..), Pandoc)
import Text.Pandoc.Highlighting (Style, pygments, styleToCss)
import Text.Pandoc.Options
    ( ReaderOptions(readerExtensions),
      WriterOptions(writerHTMLMathMethod, writerHighlightMethod,
                    writerNumberSections, writerTableOfContents,
                    writerTOCDepth,       writerTemplate),
      HighlightMethod(Skylighting),
      Extension(Ext_latex_macros, Ext_tex_math_single_backslash,
                Ext_tex_math_double_backslash, Ext_tex_math_dollars),
      extensionsFromList,
      HTMLMathMethod(MathJax) )
import Text.Pandoc.Templates (compileTemplate)
import Text.Pandoc.Walk (walkM)

import Hakyll

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  , previewHost = "0.0.0.0"
  }

main :: IO ()
main = hakyllWith config $ do
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match (fromList ["hackage.markdown","contact.markdown"]) $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "posts/*" $ do
    route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "html"
    compile $ myPandocCompiler
      >>= relocateToc
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= targetBlankLinks
      >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts) `mappend`
            defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

  create ["css/syntax.css"] $ do
    route idRoute
    compile $ makeItem $ styleToCss pandocCodeStyle

  create ["CNAME"] $ do
    route idRoute
    compile $ makeItem ("nbos.ca" :: String)

  match "res/**" $ do
    version "copy" $ do
        route idRoute
        compile copyFileCompiler

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

---------------------------------------------------
-- Pandoc compiler with download-link annotation --
---------------------------------------------------

pandocCodeStyle :: Style
pandocCodeStyle = pygments

myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
    pandocCompilerWithTransformM readerOpts writerOpts addDownloadInfo
  where
    tocTmpl = case runIdentity $ compileTemplate "" tmpl of
                Left  e -> error e
                Right t -> t

    tmpl = "$if(table-of-contents)$\n\
           \<!--TOC_START-->\n\
           \<div class=\"toc\">\n\
           \<div class=\"toc-title\">Contents</div>\n\
           \$table-of-contents$\n\
           \</div>\n\
           \<!--TOC_END-->\n\
           \$endif$\n\
           \$body$"

    readerOpts = defaultHakyllReaderOptions
      { readerExtensions = readerExtensions defaultHakyllReaderOptions
                           <> extensionsFromList
                                [ Ext_tex_math_single_backslash
                                , Ext_tex_math_double_backslash
                                , Ext_tex_math_dollars
                                , Ext_latex_macros ]
      }

    writerOpts = defaultHakyllWriterOptions
      { writerHighlightMethod = Skylighting pandocCodeStyle
      , writerHTMLMathMethod  = MathJax ""
      , writerNumberSections  = True
      , writerTableOfContents = True
      , writerTOCDepth        = 4
      , writerTemplate        = Just tocTmpl
      }

------------------------------
-- Download-link annotation --
------------------------------

-- | Extensions browsers typically render inline (i.e. NOT downloads).
--   Everything else is assumed to trigger a download.
browserInlineExts :: [String]
browserInlineExts =
  -- web pages
  [ ".html", ".htm", ".xhtml", ".shtml", ".php", ".asp", ".aspx", ".jsp"
  -- images
  , ".png", ".jpg", ".jpeg", ".gif", ".svg", ".webp", ".bmp", ".ico", ".avif"
  -- video
  , ".mp4", ".webm", ".ogv", ".ogg", ".mov"
  -- audio
  , ".mp3", ".wav", ".flac", ".aac", ".opus"
  -- documents shown in-browser
  , ".pdf"
  -- plain text / data browsers display as text
  , ".txt", ".xml", ".json", ".yaml", ".yml"
  -- code / style the browser executes or applies
  , ".js", ".mjs", ".css"
  -- fonts (loaded by CSS, never "downloaded" via a click)
  , ".woff", ".woff2", ".ttf", ".otf", ".eot"
  ]

-- | True when a link URL looks like a local file that would be downloaded.
isDownloadUrl :: T.Text -> Bool
isDownloadUrl url =
  let s    = T.unpack url
      path = takeWhile (\c -> c /= '?' && c /= '#') s   -- strip query/fragment
      ext  = map toLower (takeExtension path)
  in  not (isExternal s)          -- not http(s)://…
      && not (null ext)            -- has an extension at all
      && ext `notElem` browserInlineExts

-- | Pretty-print a byte count with binary prefixes.
formatSize :: Integer -> String
formatSize bytes
  | d >= gib  = showFFloat (Just 2) (d / gib) " GiB"
  | d >= mib  = showFFloat (Just 2) (d / mib) " MiB"
  | d >= kib  = showFFloat (Just 2) (d / kib) " KiB"
  | otherwise = show bytes <> " B"
  where
    d   = fromIntegral bytes :: Double
    kib = 1024
    mib = 1024 * 1024
    gib = 1024 * 1024 * 1024

-- | Walk every [Inline] list in the Pandoc AST and annotate download links.
addDownloadInfo :: Pandoc -> Compiler Pandoc
addDownloadInfo = walkM processInlines

processInlines :: [Inline] -> Compiler [Inline]
processInlines [] = return []
processInlines (link@(Link attr _ target@(url, _)) : rest)
  | isDownloadUrl url = do
      let fp   = takeWhile (\c -> c /= '?' && c /= '#') (T.unpack url)
          name = takeFileName fp
      exists <- unsafeCompiler $ doesFileExist fp
      rest'  <- processInlines rest
      if exists
        then do
          size <- unsafeCompiler $ getFileSize fp
          let newLink  = Link attr [Code ("", [], []) (T.pack name)] target
              sizeNote = Str (T.pack (" (" <> formatSize size <> ")"))
          return $ newLink : sizeNote : rest'
        else
          -- file not found → leave original link untouched
          return $ link : rest'
processInlines (x : rest) = (x :) <$> processInlines rest

--------------------------------
-- External links in new tabs --
--------------------------------

targetBlankLinks :: Item String -> Compiler (Item String)
targetBlankLinks item = return $ fmap addExternalAttributes item
  where
    addExternalAttributes :: String -> String
    addExternalAttributes = withTagList (map modifyTag)

    modifyTag :: Tag String -> Tag String
    modifyTag tag@(TagOpen "a" attrs) =
        case lookup "href" attrs of
            Just url | isExternal url ->
                TagOpen "a" (updateAttributes attrs)
            _ -> tag
    modifyTag tag = tag

    updateAttributes :: [(String, String)] -> [(String, String)]
    updateAttributes attrs =
        ("target", "_blank") : ("rel", "noopener") :
        filter (\(name, _) -> name /= "target" && name /= "rel") attrs

--------------------
-- TOC relocation --
--------------------

relocateToc :: Item String -> Compiler (Item String)
relocateToc = return . fmap moveToc

moveToc :: String -> String
moveToc html =
    case extractMarkedBlock "<!--TOC_START-->" "<!--TOC_END-->" html of
        Nothing -> html
        Just (before, tocBlock, after) ->
            case breakOnFirstHeading after of
                Nothing   -> before <> tocBlock <> after
                Just (intro, rest) ->
                    before <> intro <> tocBlock <> rest

extractMarkedBlock :: String -> String -> String
                   -> Maybe (String, String, String)
extractMarkedBlock startMark endMark s = do
    (before, rest1) <- breakOnSubstring startMark s
    let afterStart = drop (length startMark) rest1
    (between, rest2) <- breakOnSubstring endMark afterStart
    let after = drop (length endMark) rest2
    return (before, between, after)

breakOnSubstring :: String -> String -> Maybe (String, String)
breakOnSubstring _      []         = Nothing
breakOnSubstring needle hay@(c:cs)
    | needle `L.isPrefixOf` hay = Just ([], hay)
    | otherwise = case breakOnSubstring needle cs of
        Nothing          -> Nothing
        Just (pre, post) -> Just (c : pre, post)

breakOnFirstHeading :: String -> Maybe (String, String)
breakOnFirstHeading []       = Nothing
breakOnFirstHeading s@(c:cs)
    | isHeadingOpen s = Just ([], s)
    | otherwise = case breakOnFirstHeading cs of
        Nothing          -> Nothing
        Just (pre, post) -> Just (c : pre, post)

isHeadingOpen :: String -> Bool
isHeadingOpen ('<':'h':n:c:_) = n `L.elem` ("123456" :: String) && (c == ' ' || c == '>')
isHeadingOpen _               = False
