{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Atidot.Anonymizer.Server where

import "base"           Control.Monad.IO.Class (liftIO)
import "text"           Data.Text (Text, pack, unpack)
import qualified "text" Data.Text.IO as T
import "warp"           Network.Wai.Handler.Warp ( Settings
                                                 , defaultSettings
                                                 , setPort
                                                 , setTimeout
                                                 )
import "warp-tls"       Network.Wai.Handler.WarpTLS (tlsSettings, runTLS)
import "servant"        Servant.API
import "servant-server" Servant.Server
import "filepath"       System.FilePath
import "time"           Data.Time
import                  Atidot.Anonymizer.API
import                  Atidot.Anonymizer.Types
import                  Atidot.Anonymizer.Load
import                  Atidot.Anonymizer.Run
import                  Atidot.Anonymizer.Monad (getAllPaths, getAllAnonPaths)

import qualified "bytestring" Data.ByteString.Lazy.Char8 as BL8


anonymizeHandler :: Text -> Request -> Handler Text
anonymizeHandler hk Request{..} = liftIO $ do
    tempPath <- makeTempFile requestData
    script <- loadScript $ Just $ unpack requestScript
    pack . BL8.unpack . snd <$> run hk tempPath script


pathsHandler :: Text -> Handler [Path]
pathsHandler reqData = liftIO $ do
    tempPath <- makeTempFile reqData
    fst <$> run "" tempPath getAllPaths


anonymizedPathsHandler :: Text -> Handler [Path]
anonymizedPathsHandler reqData = liftIO $ do
    tempPath <- makeTempFile reqData
    fst <$> run "" tempPath getAllAnonPaths


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


makeTempFile :: Text -> IO FilePath
makeTempFile reqData = do
    now <- getCurrentTime
    let replaceSpace ' ' = '_'
        replaceSpace c = c
        tempName = map replaceSpace $ takeWhile ('.' /=) $ show now
        tempPath = "/tmp" </> tempName <.> "xml"
    T.writeFile tempPath reqData
    return tempPath
