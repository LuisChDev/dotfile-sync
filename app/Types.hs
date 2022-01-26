{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Types (Args(..), dotPath, dirs, add) where

import Lens.Micro.TH

data Args = Args
  { argsDotPath :: Text
  , argsAdd     :: Bool
  , argsDirs    :: [Text]
  }

makeFields ''Args
