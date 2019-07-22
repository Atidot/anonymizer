{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Atidot.Anonymizer.Swagger where

import "lens"            Control.Lens
import "bytestring"      Data.ByteString.Lazy.Char8 as BL8 (putStrLn)
import "swagger2"        Data.Swagger
import "aeson-pretty"    Data.Aeson.Encode.Pretty (encodePretty)
import "servant-swagger" Servant.Swagger
import                   Atidot.Anonymizer.API

anonymizerSwagger :: Swagger
anonymizerSwagger
    = toSwagger anonymizeAPI
    & info.title       .~ "Atidot Anonymizer"
    & info.version     .~ "1.0"
    & info.description ?~ "Atidot Anonymizer API"

printSwagger :: IO ()
printSwagger = do
    BL8.putStrLn $ encodePretty anonymizerSwagger
