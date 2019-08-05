{-# LANGUAGE PackageImports #-}
module Atidot.Anonymizer.Types.Swagger where

import "aeson"                Data.Aeson
import "swagger2"             Data.Swagger
import                        Atidot.Anonymizer.Types.Types

instance ToSchema Request where
    declareNamedSchema =
        genericDeclareNamedSchema $ fromAesonOptions defaultOptions
