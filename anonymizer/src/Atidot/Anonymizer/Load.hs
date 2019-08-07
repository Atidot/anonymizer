{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Atidot.Anonymizer.Load where

import "text"     Data.Text (Text)
import "filepath" System.FilePath (takeBaseName)
import "hint"     Language.Haskell.Interpreter
import "base"     Control.Monad
import            Atidot.Anonymizer.Monad (Anonymizer, testScript)
import            Atidot.Anonymizer.Error
import qualified "haskell-src-exts" Language.Haskell.Exts as H

verifySafeImports :: H.ParseResult (H.Module H.SrcSpanInfo) -> Either AnonymizerError ()
verifySafeImports parsedResult = do
    (importedModules, modulePragma) <- case parsedResult of
        H.ParseOk (H.Module _ _ pragma importDecls _)  -> return ( importDecls, pragma )
        H.ParseFailed _ errorReason -> anonymizerError errorReason
        _ -> anonymizerError "not a valid haskell module"
    mapM_ (isValidImport . H.importModule) importedModules
    mapM_ isValidExtension modulePragma
    where
        assertElem validElems invalidElemErr el =
            unless (el `elem` validElems) $ anonymizerError $ unlines $ invalidElemErr

        isValidImport :: Show l => H.ModuleName l -> Either AnonymizerError ()
        isValidImport (H.ModuleName _ nm) =
            let errStr = [ "Unallowed import: " ++ nm
                         , "Allowed imports are:"
                         ] ++ allowedImports
            in assertElem allowedImports errStr nm

        allowedImports =
            [ "Prelude"
            , "Control.Monad.Free"
            , "Data.Text"
            , "Atidot.Anonymizer.Monad"
            , "Data.List"
            ]

        isValidExtension :: Show l => H.ModulePragma l -> Either AnonymizerError ()
        isValidExtension (H.LanguagePragma _ nms) = forM_ nms $ \case
            H.Ident _ nm ->
                let errStr = [ "Unallowed language extension: " ++ nm
                             , "Allowed d language extensions are:"
                             ] ++ allowedExts
                in assertElem errStr allowedExts nm
            H.Symbol _ nm -> anonymizerError $ "unexpected symbol: " ++ nm
        isValidExtension mp = anonymizerError $ "Pragma not supported: " ++ show mp

        allowedExts =
            [ "PackageImports"
            , "OverloadedStrings"
            , "LambdaCase"
            ]

dynamicLoad :: FilePath -> IO (Either InterpreterError (Anonymizer Text))
dynamicLoad path = do
    throwAnonymizerError =<< verifySafeImports <$> H.parseFile path
    runInterpreter $ do
        loadModules [path]
        setTopLevelModules [takeBaseName path]
        interpret "script" (as :: Anonymizer Text)


load :: FilePath -> IO (Anonymizer Text)
load scriptPath =
    dynamicLoad scriptPath >>=
        either
        (throwAnonymizerError' (return "") . anonymizerError . show) -- todo -- construct error script from error and return it as a default value to throwAnonymizerError'
        return

 -- TODO: testscript should roll the error
loadScript :: Maybe FilePath -> IO (Anonymizer Text)
loadScript = maybe (return testScript) load