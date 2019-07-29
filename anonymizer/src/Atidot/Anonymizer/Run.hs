{-# LANGUAGE PackageImports #-}
module Atidot.Anonymizer.Run where

import "mtl"          Control.Monad.State (evalStateT)
import "data-default" Data.Default (def)
import                Atidot.Anonymizer.Monad
import                Atidot.Anonymizer.CSV
import                Atidot.Anonymizer.JSON
import                Atidot.Anonymizer.XML
import                Atidot.Anonymizer.Utils

import qualified "bytestring"   Data.ByteString.Lazy as BL


run :: Anonymizer a -> FilePath -> IO (a, BL.ByteString)
run script filepath = do
    xml'  <- isXML filepath
    csv'  <- isCSV filepath
    json' <- isJSON filepath
    fileData <- BL.readFile filepath
    let run'
            | csv'  = flip evalStateT def $ runCSV script fileData
            | json' = flip evalStateT def $ runJSON script fileData
            | xml'  = flip evalStateT def $ runXML script fileData
            | otherwise = undefined
    run'
