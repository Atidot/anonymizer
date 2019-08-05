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
import "temporary"      System.IO.Temp
import                  Atidot.Anonymizer.API
import                  Atidot.Anonymizer.Types
import                  Atidot.Anonymizer.Load
import                  Atidot.Anonymizer.Run
import                  Atidot.Anonymizer.Monad (getAllPaths, getAllChangedPaths)

import qualified "bytestring" Data.ByteString.Lazy.Char8 as BL8
import qualified "text" Data.Text as T (pack, unpack)


anonymizeHandler :: Request -> Handler Text
anonymizeHandler Request{..} = liftIO $ do
    tempPath <- makeTempFile requestData
    script <- loadScript $ Just $ T.unpack requestScript
    T.pack . BL8.unpack . snd <$> run requestKey tempPath script


pathsHandler :: Text -> Handler [Path]
pathsHandler reqData = liftIO $ do
    tempPath <- makeTempFile reqData
    fst <$> run "" tempPath getAllPaths


anonymizedPathsHandler :: Text -> Handler [Path]
anonymizedPathsHandler reqData = liftIO $ do
    tempPath <- makeTempFile reqData
    fst <$> run "" tempPath getAllChangedPaths


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
makeTempFile = writeTempFile "/tmp" "anonymizer" . T.unpack
    -- do
    -- now <- getCurrentTime
    -- let replaceSpace ' ' = '_'
    --     replaceSpace c = c
    --     tempName = map replaceSpace $ takeWhile ('.' /=) $ show now
    --     tempPath = "/tmp" </> tempName <.> "xml"
    -- T.writeFile tempPath reqData
    -- return tempPath
