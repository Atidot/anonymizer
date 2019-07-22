{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Atidot.Anonymizer.Monad where

import "base"        Data.Typeable (Typeable)
import "free"        Control.Monad.Free
import "free"        Control.Monad.Free.TH
import "monad-loops" Control.Monad.Loops (whileM_)
import "text"        Data.Text (Text)
import               Atidot.Anonymizer.Types.Types

data AnonymizerCmd a
    = Current         (Path -> a)
    | Next            (Bool -> a)
    | Hash            a
    | Encrypt         a
    --
    | Paths           ([Path] -> a)
    | ChangedPaths    ([Path] -> a)
    deriving (Typeable, Functor)

type Anonymizer = Free AnonymizerCmd

makeFree ''AnonymizerCmd

whileNext :: Free AnonymizerCmd a -> Free AnonymizerCmd ()
whileNext = whileM_ next


testScript :: Anonymizer Text
testScript = return "Test Script"

getAllPaths :: Anonymizer [Path]
getAllPaths = do
    whileNext $ return ()
    paths

anonymizeAll :: Anonymizer Text
anonymizeAll = do
    whileNext hash
    return ""
