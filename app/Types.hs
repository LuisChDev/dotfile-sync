{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
module Types (Args(..), dotPath, dirs, add) where

import Lens.Micro.TH

data Args = Args
  { argsDotPath :: Text
  , argsAdd     :: Bool
  , argsDirs    :: [Text]
  }

makeFields ''Args
