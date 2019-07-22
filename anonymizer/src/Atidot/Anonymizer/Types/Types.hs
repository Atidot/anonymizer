{-# LANGUAGE PackageImports #-}
module Atidot.Anonymizer.Types.Types where

import "text" Data.Text (Text)

type Path = [Text]

data IsEdit = CanEdit | CannotEdit

