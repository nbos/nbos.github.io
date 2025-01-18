{-# LANGUAGE OverloadedStrings #-}

import Data.List (sortBy)
import Data.Monoid (mappend)
import Data.Ord (comparing)
import Data.Time.Clock
import Data.Time.Format
import Hakyll
import Text.HTML.TagSoup
import Hakyll.Web.Html (withTagList, isExternal)

configuration :: Configuration
configuration = defaultConfiguration {destinationDirectory = "docs"}

main :: IO ()
main = hakyllWith configuration $ do
  match "posts/*.md" $ do
    route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "html"
    compile $ do
      pandocCompiler
        >>= saveSnapshot "content" -- Save the content snapshot
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= externalLinksInNewTabs
        >>= relativizeUrls

  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*.md"
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/index.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "CNAME" $ do
    create ["CNAME"] $ do
      route idRoute
      compile $ makeItem ("nbos.ca" :: String)

  -- match "images/tess.svg" $ do
  --   route $ constRoute "favicon.svg"
  --   compile copyFileCompiler

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` excerptField
    `mappend` defaultContext

excerptField :: Context String
excerptField = field "preview" $ \item -> do
    body <- loadSnapshot (itemIdentifier item) "content"
    let content = itemBody body
        excerpt = unwords $ take 60 $ words content
    return $ excerpt ++ "..."

-- TODO: this is a dressed up String -> String function
externalLinksInNewTabs :: Item String -> Compiler (Item String)
externalLinksInNewTabs item = return $ fmap addExternalAttributes item
  where
    addExternalAttributes :: String -> String
    addExternalAttributes = withTagList modifyExternalLinks

    modifyExternalLinks :: [Tag String] -> [Tag String]
    modifyExternalLinks = map modifyTag

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
