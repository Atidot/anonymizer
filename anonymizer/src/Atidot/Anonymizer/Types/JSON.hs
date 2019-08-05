{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Atidot.Anonymizer.Types.JSON where

import           "aeson"             Data.Aeson
import           "aeson"             Data.Aeson.TH
import                               Atidot.Anonymizer.Types.Types

$(deriveJSON defaultOptions ''Request)
