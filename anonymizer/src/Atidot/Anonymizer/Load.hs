{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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



-- TODO: can be eithered, and we take care of the errors outside of this scope
verifySafeImports :: FilePath -> IO ()
verifySafeImports path = do
    parseResult <- H.parseFile path
    let moduleNames = case parseResult of
            H.ParseOk (H.Module _ _ _ importDecls _)  -> map H.importModule importDecls  -- Right
            H.ParseFailed _ errorReason -> error errorReason -- Left errorReason
            _ -> error "not a valid haskell module" -- Left $
    mapM_ checkSafety moduleNames
    where
        checkSafety :: Show l => H.ModuleName l -> IO ()
        checkSafety (H.ModuleName _ nm) =
            unless (nm `elem` allowedImports) $ error $ unlines $
                ["Unallowed import: " ++ nm
                ,"allow imports are:"
                ] ++ allowedImports
        allowedImports =
            [ "Prelude"
            , "Control.Monad.Free"
            , "Data.Text"
            , "Atidot.Anonymizer.Monad"
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
