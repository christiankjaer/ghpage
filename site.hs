--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["index.md", "about.rst"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "articles/*" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/article.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    create ["articles.html"] $ do
        route $ idRoute
        compile $ do
            articles <- recentFirst =<< loadAll "articles/*"
            let overviewCtx =
                    listField "articles" defaultContext (return articles) `mappend`
                    constField "title" "Articles" `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/articles.html" overviewCtx
                >>= loadAndApplyTemplate "templates/default.html" overviewCtx
                >>= relativizeUrls
        

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
articleCtx :: Context String
articleCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
