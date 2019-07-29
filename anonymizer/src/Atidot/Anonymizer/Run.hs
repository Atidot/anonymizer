{-# LANGUAGE PackageImports #-}
module Atidot.Anonymizer.Run where

import "mtl"          Control.Monad.State (evalStateT)
import "data-default" Data.Default (def)
import "text"         Data.Text (Text)
import                Atidot.Anonymizer.Monad
import                Atidot.Anonymizer.CSV
import                Atidot.Anonymizer.JSON
import                Atidot.Anonymizer.XML
import                Atidot.Anonymizer.Utils

import qualified "bytestring"   Data.ByteString.Lazy as BL


run :: Text -> FilePath -> Anonymizer a -> IO (a, BL.ByteString)
run hk filepath script = do
    xml'  <- isXML filepath
    csv'  <- isCSV filepath
    json' <- isJSON filepath
    fileData <- BL.readFile filepath
    let run'
            | csv'  = flip evalStateT def{_csvState_hashKey = hk} $ runCSV script fileData
            | json' = flip evalStateT def $ runJSON script fileData
            | xml'  = flip evalStateT def{_xmlState_hashKey = hk} $ runXML script fileData
            | otherwise = undefined
    run'
