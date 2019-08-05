{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Atidot.Anonymizer.Server where

import "base"           Control.Monad.IO.Class (liftIO)
import "text"           Data.Text (Text)
import "warp"           Network.Wai.Handler.Warp ( Settings
                                                 , defaultSettings
                                                 , setPort
                                                 , setTimeout
                                                 )
import "warp-tls"       Network.Wai.Handler.WarpTLS (tlsSettings, runTLS)
import "servant"        Servant.API
import "servant-server" Servant.Server
import                  Atidot.Anonymizer.API
import                  Atidot.Anonymizer.Types

anonymizeHandler :: Text -> Request -> Handler Text
anonymizeHandler _key Request{..} = undefined


pathsHandler :: Text -> Handler [Path]
pathsHandler file = do
    liftIO $ print file
    return [["a"], ["b"], ["c"]]

anonymizedPathsHandler :: Text -> Handler [Path]
anonymizedPathsHandler _file = do
    liftIO $ print "here3"
    return [["a"], ["b"], ["c"]]


server :: Server (AnonymizeAPI ())
server = anonymizeHandler
    :<|> pathsHandler
    :<|> anonymizedPathsHandler


application :: Application
application = serve anonymizeAPI server


runServer :: Int -> FilePath -> FilePath -> IO ()
runServer port sslCrt sslKey = do
    let defaultTLS = tlsSettings sslCrt sslKey
    runTLS defaultTLS settings' $ application
    where
        settings' :: Settings
        settings'
            = setPort port
            $ setTimeout 1800
            $ defaultSettings
