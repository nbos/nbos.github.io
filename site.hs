{-# LANGUAGE OverloadedStrings #-}

import Data.Functor.Identity (runIdentity)
import qualified Data.List as L
import Text.Pandoc.Templates (compileTemplate)

import Text.HTML.TagSoup (Tag(TagOpen))
import Text.Pandoc.Options
    ( ReaderOptions(readerExtensions),
      WriterOptions(writerHTMLMathMethod, writerHighlightMethod,
                    writerNumberSections, writerTableOfContents,
                    writerTemplate),      HighlightMethod(Skylighting),
      HighlightMethod(Skylighting),
      Extension(Ext_latex_macros, Ext_tex_math_single_backslash,
                Ext_tex_math_double_backslash, Ext_tex_math_dollars),
      extensionsFromList,
      HTMLMathMethod(MathJax) )
import Text.Pandoc.Highlighting (Style, pygments, styleToCss)

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
      >>= targetBlankLinks -- open external links in new tabs
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

-- FIXME: this is a dressed up String -> String function
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

pandocCodeStyle :: Style
pandocCodeStyle = pygments

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = pandocCompilerWith readerOpts writerOpts
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
      , writerTemplate        = Just tocTmpl
      }

-- | Move the TOC from the top of the Pandoc output to just before
--   the first section heading, so it sits between the introduction
--   and the first numbered section (Wikipedia-style).
relocateToc :: Item String -> Compiler (Item String)
relocateToc = return . fmap moveToc

moveToc :: String -> String
moveToc html =
    case extractMarkedBlock "<!--TOC_START-->" "<!--TOC_END-->" html of
        Nothing -> html                          -- no TOC, pass through
        Just (before, tocBlock, after) ->
            case breakOnFirstHeading after of
                Nothing   -> before <> tocBlock <> after   -- no heading, keep at top
                Just (intro, rest) ->
                    before <> intro <> tocBlock <> rest     -- intro → TOC → sections

-- | Extract the text between two unique markers and return
--   (before-start, between, after-end).
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

-- | True for opening tags <h1 …>, <h2>, etc. but not <header>, <hr>, …
isHeadingOpen :: String -> Bool
isHeadingOpen ('<':'h':n:c:_) = n `L.elem` ("123456" :: String) && (c == ' ' || c == '>')
isHeadingOpen _               = False
