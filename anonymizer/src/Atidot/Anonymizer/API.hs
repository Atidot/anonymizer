{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Atidot.Anonymizer.API where

import "base"           Data.Proxy (Proxy(..))
import "text"           Data.Text (Text)
import "servant"        Servant.API
import                  Atidot.Anonymizer.Types


type AnonymizeAPI authMethod
    = "anonymize" :> ReqBody '[JSON] Request
                  :> Post '[PlainText] Text

 :<|> "paths" :> ReqBody '[PlainText] Text
              :> Post '[JSON] [Path]

 :<|> "anonymizedPaths" :> ReqBody '[PlainText] Text
                        :> Post '[JSON] [Path]

anonymizeAPI :: Proxy (AnonymizeAPI ())
anonymizeAPI = Proxy
