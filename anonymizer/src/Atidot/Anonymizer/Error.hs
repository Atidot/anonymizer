{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Atidot.Anonymizer.Error where

data AnonymizerError = AnonymizerError String

anonymizerError :: String -> Either AnonymizerError b
anonymizerError = Left . AnonymizerError

throwAnonymizerError :: Either AnonymizerError b -> IO ()
throwAnonymizerError (Left (AnonymizerError err)) = putStrLn err
throwAnonymizerError Right{} = return ()

throwAnonymizerError' :: b -> Either AnonymizerError b -> IO b
throwAnonymizerError' def (Left (AnonymizerError err)) = putStrLn err >> return def
throwAnonymizerError' _ (Right x) = return x
