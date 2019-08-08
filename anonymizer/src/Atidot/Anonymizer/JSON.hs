{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
module Atidot.Anonymizer.JSON where

import "base"           Control.Monad.IO.Class (MonadIO)
import "data-default"   Data.Default
import "mtl"            Control.Monad.State.Class (MonadState)
import "exceptions"     Control.Monad.Catch (MonadMask, bracket)
import                  Atidot.Anonymizer.Monad

import qualified "bytestring"   Data.ByteString.Lazy as BL


data JSONState = JSONState
instance Default JSONState where
    def = JSONState

runJSON :: (MonadState JSONState m, MonadIO m, MonadMask m)
        => Anonymizer a
        -> BL.ByteString
        -> m (a, BL.ByteString)
runJSON _action _data
    = bracket init'
              fini
              body
    where
        init' = return ()

        fini _ = return ()

        body _ = anonymizerError "JSON format is not yet supported"
