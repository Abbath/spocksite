{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Lib
    ( app, MySession(..), MyAppState(..)
    ) where

import           Control.Monad
import           Control.Monad.Trans
import           Data.List
import qualified Data.Text                     as TS
import qualified Data.Text.IO                  as TSIO
import qualified Data.Text.Lazy                as TL
import           Network.Wai.Middleware.Static
import           System.Directory
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              ((!))
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as HA
import           Text.Markdown
import           Web.Spock

data MySession = EmptySession
data MyAppState = EmptyState

posts :: IO [(TS.Text, TS.Text)]
posts = do
    filenames <- sortBy (flip compare) <$> listDirectory "./posts"
    zip (map TS.pack filenames) <$> mapM (TSIO.readFile . ("./posts/"++)) filenames

app :: SpockM () MySession MyAppState ()
app = do
    middleware (staticPolicy (addBase "static"))
    get root $ do
        psts <- liftIO posts
        html $ TL.toStrict $ index psts
    get (root <//> var) $ \post_id -> do
        let filename = "./posts/" ++ TS.unpack post_id
        fileExists <- liftIO $ doesFileExist filename
        if fileExists
            then do
                fileContent <- liftIO $ TSIO.readFile filename
                html . TL.toStrict . renderHtml . Lib.page $ Lib.post fileContent post_id
            else redirect "/"

convertToData :: TS.Text -> TS.Text
convertToData post_id = let year = TS.take 4 post_id
                            month = TS.take 2 . TS.drop 5 $ post_id
                            day = TS.take 2 . TS.drop 8 $ post_id
                        in TS.concat [day, ".", month, ".", year]

index :: [(TS.Text, TS.Text)] -> TL.Text
index s = renderHtml $ Lib.page $
        forM_ s (\(post_id, x) -> do
            Lib.post x post_id
            H.br)

post x post_id = do
    (H.div . markdown def . TL.fromStrict) x
    H.div $ do
        H.toHtml $ TS.append (convertToData post_id) " "
        H.a ! HA.href (H.textValue $ TS.append "/" post_id) $ "Link"

page new = H.docTypeHtml ! HA.lang "en" $ do
    H.head $ do
        H.meta ! HA.charset "utf-8"
        H.meta ! HA.httpEquiv "x-ua-compatible" ! HA.content "ie=edge"
        H.meta ! HA.name "viewport" ! HA.content "width=device-width, initial-scale=1"
        H.title "Abbath's Corner"
        H.link ! HA.rel "stylesheet" ! HA.href "default.css"
        H.link ! HA.rel "icon" ! HA.type_ "image/png" ! HA.href "peka.png"
    H.body $ do
        Lib.header
        new
        Lib.footer

header = H.header $ H.div ! HA.class_ "logo" $ do
    H.a ! HA.href "/" $ "Abbath's Corner"
    H.br

footer = H.footer $ do
    H.div ! HA.style "float: left" $ "Abbath © 2017"
    H.div $ do
        H.toHtml @TS.Text "Powered by "
        H.a ! HA.href "https://spock.li" $ "Spock"
