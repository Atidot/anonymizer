{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}

module Atidot.Anonymizer.Types.Types where

import "text"                   Data.Text (Text)
import "base"                   GHC.Generics (Generic)

type Path = [Text]

data IsEdit = CanEdit | CannotEdit

data Request = Request { _requestScript :: Text
                       , _requestData :: Text
                       } deriving (Generic)
