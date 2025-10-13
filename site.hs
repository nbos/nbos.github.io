{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup (Tag(TagOpen))
import Text.Pandoc.Options
    ( ReaderOptions(readerExtensions),
      WriterOptions(writerHTMLMathMethod, writerHighlightMethod),
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
myPandocCompiler = pandocCompilerWith
  defaultHakyllReaderOptions
  { readerExtensions = readerExtensions defaultHakyllReaderOptions
                       <> extensionsFromList [ Ext_tex_math_single_backslash
                                             , Ext_tex_math_double_backslash
                                             , Ext_tex_math_dollars
                                             , Ext_latex_macros ]
  }

  defaultHakyllWriterOptions
  { writerHighlightMethod = Skylighting pandocCodeStyle
  , writerHTMLMathMethod = MathJax "" }
