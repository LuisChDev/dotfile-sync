{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Prelude                       as P

import           Control.Monad.Except           ( throwError )
import           System.Environment             ( lookupEnv
                                                )
import           Turtle                        as TR
import Options.Applicative as AP

data Args = Args
  { argsDotPath :: Text
  , argsAdd     :: Bool
  , argsDirs    :: [Text]
  }

args :: Parser Args
args = Args
  <$> strOption (long "dirpath" <> short 'd' <> help "The directory where you keep configuration files. Must be a git repo.")
  <*> AP.switch (long "add" <> short 'a' <> help "sync the files to the dotfile folder instead of the other way around (default)")
  <*> _

dotPath :: Text
dotPath = "DOTFILE_DIR"

dotPath' :: String
dotPath' = toString dotPath

pathToLine :: TR.FilePath -> Line
pathToLine = fromString . encodeString

lineToPath :: Line -> TR.FilePath
lineToPath = decodeString . show


main' :: ExceptT Text Shell Line
main' = do
  dotfilePath <- liftIO $ lookupEnv dotPath'
  path        <- case dotfilePath of
    Nothing    -> throwError $ "env var " <> dotPath <> " not defined."
    Just path_ -> pure path_

  let pathT  = fromString path
      pathT' = P.toText path
      lnArgs :: TR.FilePath -> Line
      lnArgs loc = pathToLine loc <> " " <> pathToLine outDir
        where
          -- | stripPrefix craps out if we want an absolute path
          outDir = (decodeString . drop (length pathT') . encodeString) loc

  ifM
    (testdir pathT)
    (pure ())
    (throwError $ "directory indicated by path " <> pathT' <> " does not exist."
    )

  ifM
    (shellStrict ("git -C " <> pathT' <> " rev-parse") empty >>= \case
     (ExitSuccess, _) -> pure True
     (_, _) -> pure False
    )
    (pure ())
    (throwError $ "directory indicated by path " <> pathT' <> "is not a git repository.")

  echo "syncing dotfiles with repo"
  fname <- lift $ flip TR.find pathT $ invert
    (contains ".git" <|> contains "google-chrome" <|> contains "chromium")
  True <- testfile fname

  lift $ flip inshell empty $ "ln " <> lineToText (lnArgs fname)


addFolder :: ExceptT Text Shell Line
addFolder = undefined


main :: IO ()
main = do
  echo "in progress"
  TR.view $ runExceptT main'
