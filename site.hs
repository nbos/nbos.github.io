{-# LANGUAGE OverloadedStrings #-}

import Data.List (sortBy)
import Data.Monoid (mappend)
import Data.Ord (comparing)
import Data.Time.Clock
import Data.Time.Format
import Hakyll

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

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` excerptField
    `mappend` defaultContext

-- Add this function
excerptField :: Context String
excerptField = field "preview" $ \item -> do
  body <- loadSnapshot (itemIdentifier item) "content"
  let content = unwords . take 60 . words $ itemBody body -- Takes first 60 words
  return $ content ++ "..."
