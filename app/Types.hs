{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
module Types
  ( Args(..)
  , dotPath
  , dirs
  , add
  , INotify(..)
  , ShellE
  ) where

import           Lens.Micro.TH
import           Turtle as TR                        ( Shell, FilePath )

data Args = Args
  { argsDotPath :: Text
  , argsAdd     :: Bool
  , argsDirs    :: [Text]
  }
makeFields ''Args

data INotify = Create TR.FilePath | Delete TR.FilePath deriving Show

type ShellE a = ExceptT Text Shell a
