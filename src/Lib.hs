{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( app, MySession(..), MyAppState(..)
    ) where

import           Control.Monad
import           Control.Monad.Trans
import           Data.List
import qualified Data.Text                     as TS
import qualified Data.Text.IO                  as TSIO
import qualified Data.Text.Lazy                as TL
import           Data.Time.Clock
import           Data.Time.Format
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

posts = do
    files <- (sortBy (flip compare)) <$> listDirectory "./posts"
    zip (map TS.pack files) <$> mapM (TSIO.readFile . ("./posts/"++)) files

app :: SpockM () MySession MyAppState ()
app = do
    middleware (staticPolicy (addBase "static"))
    get root $ do
        time <- liftIO getCurrentTime
        psts <- liftIO posts
        html $ TL.toStrict $ index psts time
    get (root <//> "favicon") $ file "favicon" "./static/peka.png"
    get (root <//> var) $ \post_id -> do
        file <- liftIO $ TSIO.readFile ("./posts/" ++ TS.unpack post_id)
        time <- liftIO getCurrentTime
        html . TL.toStrict . renderHtml . Lib.header $ Lib.post file post_id

convertToData post_id = let year = TS.take 4 post_id
                            month = TS.take 2 . TS.drop 5 $ post_id
                            day = TS.take 2 . TS.drop 8 $ post_id
                        in TS.concat [day, ".", month, ".", year]

index :: [(TS.Text, TS.Text)] -> UTCTime -> TL.Text
index s time = renderHtml $ Lib.header $
        forM_ s (\(post_id, x) -> do
            H.br
            Lib.post x post_id)

post x post_id = do
    (H.div . markdown def . TL.fromStrict) x
    H.div $ do
        H.toHtml $ TS.append (convertToData post_id) " "
        H.a ! HA.href (H.textValue $ TS.append "/" post_id) $ "Link"

header new = H.docTypeHtml ! HA.lang "en" $ do
    H.head $ do
        H.meta ! HA.charset "utf-8"
        H.meta ! HA.httpEquiv "x-ua-compatible" ! HA.content "ie=edge"
        H.meta ! HA.name "viewport" ! HA.content "width=device-width, initial-scale=1"
        H.title "Abbath's Corner - $title$"
        H.link ! HA.rel "stylesheet" ! HA.href "default.css"
        H.link ! HA.rel "icon" ! HA.type_ "image/png" ! HA.href "peka.png"
    H.body $ do
        H.header $ do
            H.div ! HA.class_ "logo" $ H.a ! HA.href "/" $ "Abbath's Corner"
            H.nav $ H.a ! HA.href "/" $ "Home"
        new


