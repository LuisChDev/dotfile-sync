module Main where

import           Prelude                       as P

import           Control.Monad.Except           ( throwError )
import           Data.Text                      ( unpack )
import           Options.Applicative           as AP
import           Turtle                        as TR
import           Types                          ( Args(..) )


pathToLine :: TR.FilePath -> Line
pathToLine = fromString . encodeString

args :: Parser Args
args =
  Args
    <$> strOption
          (  long "dirpath"
          <> short 'd'
          <> help
               "The directory where you keep configuration files. Must be a git repo."
          )
    <*> AP.switch
          (  long "add"
          <> short 'a'
          <> help
               "sync the files to the dotfile folder instead of the other way around (default)."
          )
    <*> many
          (strOption $ long "dir" <> short 'b' <> help
            "directories where you wish to sync dotfiles."
          )

main' :: Args -> ExceptT Text Shell Line
main' Args { argsDotPath, argsAdd, argsDirs } = do
  let pathT  = fromString $ unpack argsDotPath
      pathT' = P.toText argsDotPath
      lnArgs :: TR.FilePath -> Line
      lnArgs loc = pathToLine loc <> " " <> pathToLine outDir
        where outDir = (decodeString . drop (length pathT') . encodeString) loc
          {- stripPrefix craps out if we want an absolute path -}

  ifM
    (testdir pathT)
    (pure ())
    (throwError $ "directory indicated by path " <> pathT' <> " does not exist."
    )

  ifM
    (shellStrict ("git -C " <> pathT' <> " rev-parse") empty >>= \case
      (ExitSuccess, _) -> pure True
      (_          , _) -> pure False
    )
    (pure ())
    (  throwError
    $  "directory indicated by path "
    <> pathT'
    <> " is not a git repository."
    )

  if argsAdd
    then do
      echo "adding dotfiles in indicated folders to repo"
      addFolder pathT argsDirs lnArgs
    else do
      echo "syncing dotfiles with repo"
      syncConfig pathT lnArgs


syncConfig :: TR.FilePath -> (TR.FilePath -> Line) -> ExceptT Text Shell Line
syncConfig pathT lnArgs = do
  fname <- lift $ flip TR.find pathT $ invert
    (contains ".git" <|> contains "google-chrome" <|> contains "chromium")
  True <- testfile fname

  lift $ mktree $ directory fname
  lift $ flip inshell empty $ "ln " <> lineToText (lnArgs fname)


addFolder :: TR.FilePath -> [Text] -> (TR.FilePath -> Line) -> ExceptT Text Shell Line
addFolder pathT argsDirs lnArgs = undefined

main :: IO ()
main = do
  params <- execParser $ info args fullDesc
  TR.view
    $ map
        (\case
          Left  t    -> t
          Right line -> lineToText line
        )
    $ runExceptT
    $ main' params
