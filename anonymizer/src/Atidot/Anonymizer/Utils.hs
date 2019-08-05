{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleInstances #-}

module Atidot.Anonymizer.Utils where

import "base"           Data.List (isSuffixOf)
import "base"           Control.Exception
import "base"           System.IO.Error
import "directory"      System.Directory
import "cryptonite"     Crypto.Hash (hashWith)
import "cryptonite"     Crypto.Hash.Algorithms (SHA256(..))
import "text"           Data.Text (Text)
import                  Atidot.Anonymizer.Types (Path)

import qualified "text"       Data.Text as T
import qualified "regex-tdfa" Text.Regex.TDFA as RGX
import qualified "bytestring" Data.ByteString.Char8 as B8

type Salt = Text

class Anonymized a where
    anonymize :: Salt -> a -> a

instance Anonymized String where
    anonymize salt
        = show
        . hashWith SHA256
        . B8.pack
        . (<> T.unpack salt)

isXML :: FilePath -> IO Bool
isXML = return . isSuffixOf ".xml"

isCSV :: FilePath -> IO Bool
isCSV = return . isSuffixOf ".csv"

isJSON :: FilePath -> IO Bool
isJSON = return . isSuffixOf ".json"

(=~) :: Path -> [String] -> Bool
(=~) _  [] = True
(=~) [] _  = False
(=~) (x:xs) (r:rs) = if T.unpack x RGX.=~ r
    then xs =~ rs
    else xs =~ (r:rs)

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e
