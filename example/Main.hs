{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.ByteString.Char8 as C8
import           Database.InfluxDB
import           Snap
import           Snap.Snaplet.InfluxDB

------------------------------------------------------------------------------
data App = App
    { _influx :: Snaplet InfluxState
    }

makeLenses ''App

instance HasInfluxPool (Handler b App) where
    getInfluxPool = with influx getInfluxPool

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/"   , writeText "hello")
         , ("test", fooHandler)
         ]

fooHandler :: Handler App App ()
fooHandler = do

    -- If you want full control over the type of post, precision,
    -- maybe even the db to post to...

    runInflux $ \c -> do
        post c "db" $ writeSeries "myseries" "somevalue"

    -- OR if you want to use the database you've configured with a
    -- default precision of Seconds...

    runInfluxPost $ writeSeries "myseries" "somevalue"

    modifyResponse $ setResponseCode 204


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    m <- nestSnaplet "influx" influx $ initInflux
    addRoutes routes
    return $ App m

main :: IO ()
main = serveSnaplet defaultConfig app
