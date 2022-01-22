{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle as TR
    ( IsString(fromString),
      Applicative(pure),
      (<>),
      view,
      dot,
      find,
      echo,
      testdir,
      FilePath,
      MonadIO(liftIO),
      Shell,
      Text, star, noneOf)
import System.Environment (getEnv, lookupEnv)
import Data.Maybe (isNothing)
import Control.Monad.Except (throwError)

dotPath :: Text
dotPath  = "DOTFILE_DIR"

dotPath' :: String
dotPath' = toString dotPath

main' :: ExceptT Text Shell TR.FilePath
main' = do
  dotfilePath <- liftIO $ lookupEnv dotPath'
  path <- case dotfilePath of
    Nothing -> throwError $ "env var " <> dotPath <> " not defined."
    Just path_ -> pure path_

  let pathT = fromString path
      pathT' = toText path

  ifM (testdir pathT)
    (pure ())
    (throwError $ "directory indicated by path " <> pathT' <> " does not exist.")

  echo "syncing dotfiles with repo"
  lift $ flip TR.find pathT $ noneOf ".git"

main :: IO ()
main = do
  echo "work in progress"
  TR.view $ runExceptT main'
