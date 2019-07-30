{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Atidot.Anonymizer.Load where

import "text"     Data.Text (Text)
import "filepath" System.FilePath (takeBaseName)
import "hint"     Language.Haskell.Interpreter
import "base"     Control.Monad
import            Atidot.Anonymizer.Monad (Anonymizer)
import qualified "haskell-src-exts" Language.Haskell.Exts as H


dynamicLoad :: FilePath -> IO (Either InterpreterError (Anonymizer Text))
dynamicLoad path = do
    verifySafeImports path
    runInterpreter load_
    where
        load_ :: Interpreter (Anonymizer Text)
        load_ = do
            loadModules [path]
            setTopLevelModules [takeBaseName path]
            interpret "script" (as :: Anonymizer Text)


verifySafeImports :: FilePath -> IO ()
verifySafeImports path = do
    parseResult <- H.parseFile path
    let (importedModules, modulePragma) = case parseResult of
            H.ParseOk (H.Module _ _ pragma importDecls _)  -> ( importDecls
                                                              , pragma
                                                              )
            H.ParseFailed _ errorReason -> error errorReason
            _ -> error "not a valid haskell module"
    let importedModulesNames =  map H.importModule importedModules
    mapM_ isValidImport importedModulesNames
    mapM_ isValidExtension modulePragma
    where
        isValidImport :: Show l => H.ModuleName l -> IO ()
        isValidImport (H.ModuleName _ nm) =
            unless (nm `elem` allowedImports) $ error $ unlines $
                [ "Unallowed import: " ++ nm
                , "Allowed imports are:"
                ] ++ allowedImports

        allowedImports =
            [ "Prelude"
            , "Control.Monad.Free"
            , "Data.Text"
            , "Atidot.Anonymizer.Monad"
            , "Data.List"
            ]

        isValidExtension :: Show l => H.ModulePragma l -> IO ()
        isValidExtension (H.LanguagePragma _ nms) = forM_ nms $ \case
            H.Ident _ nm -> unless (nm `elem` allowedExts) $ error $ unlines $
                [ "Unallowed language extension: " ++ nm
                , "Allowed d language extensions are:"
                ] ++ allowedExts
            H.Symbol _ nm -> error $ "unexpected symbol: " ++ nm
        isValidExtension mp = error $ "Pragma not supported: " ++ show mp

        allowedExts =
            [ "PackageImports"
            , "OverloadedStrings"
            , "LambdaCase"
            ]


load :: FilePath -> IO (Anonymizer Text)
load scriptPath = do
    interpreterResult <- dynamicLoad scriptPath
    case interpreterResult of
        Left err -> case err of
            WontCompile errors -> do
                print errors
                undefined
            x -> do
                print x
                undefined
        Right script ->
            return script
