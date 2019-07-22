{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Atidot.Anonymizer.Load where

import "text"     Data.Text
import "filepath" System.FilePath (takeBaseName)
import "hint"     Language.Haskell.Interpreter
import            Atidot.Anonymizer.Monad (Anonymizer)

dynamicLoad :: FilePath -> IO (Either InterpreterError (Anonymizer Text))
dynamicLoad path = runInterpreter load_
    where
        load_ :: Interpreter (Anonymizer Text)
        load_ = do
            loadModules [path]
            setTopLevelModules [takeBaseName path]
            interpret "script" (as :: Anonymizer Text)

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
