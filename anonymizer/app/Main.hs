{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           "base"                 Data.Semigroup ((<>))
import           "ekg"                  System.Remote.Monitoring (forkServer)
import           "optparse-applicative" Options.Applicative
import qualified "atidot-anonymizer"    Atidot.Anonymizer.Main as Anonymizer (main)

main :: IO ()
main = do
    forkServer "localhost" 8000
    Anonymizer.main
    return ()
