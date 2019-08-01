{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Atidot.Anonymizer.Run where

import "mtl"          Control.Monad.State (evalStateT)
import "data-default" Data.Default (def)
import "text"         Data.Text (Text)
import "filepath"     System.FilePath
import                Atidot.Anonymizer.Monad
import                Atidot.Anonymizer.CSV
import                Atidot.Anonymizer.JSON
import                Atidot.Anonymizer.XML hiding (run)
import                Atidot.Anonymizer.Utils

import qualified "bytestring" Data.ByteString.Lazy as BL
import qualified "bytestring" Data.ByteString.Lazy.Char8 as BL8

runExample :: FilePath -> Anonymizer a -> IO a
runExample filepath script = runNotebook "hashkey" filepath script Nothing

runNotebook ::  Text -> FilePath -> Anonymizer a -> Maybe FilePath -> IO a
runNotebook hk filepath script mOutDir = do
    (res,resBS) <- run hk filepath script
    case mOutDir of
        Just outDir -> do
            let fExt = takeExtension filepath
                fName = takeBaseName filepath
                outFile = outDir </> fName <.> "anon" <.> fExt -- TODO: add date as numerical value
            BL.writeFile outFile resBS
        Nothing -> return ()
    BL8.putStrLn resBS
    return res

runSingleFile :: Text -> FilePath -> Anonymizer a -> Maybe FilePath -> IO a
runSingleFile hk filepath script mOutDir = do
    (res,resBS) <- run hk filepath script
    case mOutDir of
        Just outDir -> do
            let fExt = takeExtension filepath
                fName = takeBaseName filepath
                outFile = outDir </> fName <.> "anon" <.> fExt -- TODO: add date as numerical value
            BL.writeFile outFile resBS
        Nothing -> BL8.putStrLn resBS
    return res


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
