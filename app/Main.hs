module Main where

import           Prelude                       as P

import           Control.Monad.Except           ( throwError )
import           Data.Text                      ( unpack )
import           Options.Applicative           as AP
import           Turtle                        as TR
import           Types                          ( Args(..) )


pathToLine :: TR.FilePath -> Line
pathToLine = fromString . encodeString

-- | takes the path as given in the folder and returns its location in
--   the filesystem
-- we can't simply use stripPrefix as that does't work with absolute paths
sysLoc :: Text -> TR.FilePath -> TR.FilePath
sysLoc path = decodeString . drop (length path) . encodeString

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


-- | does most of the initial validation before dropping into the other routines.
main' :: Args -> ExceptT Text Shell Line
main' Args { argsDotPath, argsAdd, argsDirs } = do
  let pathT   = fromText argsDotPath
      sysLoc' = sysLoc argsDotPath

  ifM
    (testdir pathT)
    (pure ())
    (  throwError
    $  "directory indicated by path "
    <> argsDotPath
    <> " does not exist."
    )

  ifM
    (shellStrict ("git -C " <> argsDotPath <> " rev-parse") empty >>= \case
      (ExitSuccess, _) -> pure True
      (_          , _) -> pure False
    )
    (pure ())
    (  throwError
    $  "directory indicated by path "
    <> argsDotPath
    <> " is not a git repository."
    )

  if argsAdd
    then do
      echo "adding dotfiles in indicated folders to repo"
      addFolder pathT argsDirs sysLoc'
    else do
      echo "syncing dotfiles with repo"
      syncConfig pathT sysLoc'


syncConfig
  :: TR.FilePath -> (TR.FilePath -> TR.FilePath) -> ExceptT Text Shell Line
syncConfig pathT pathF = do
  fname <- lift $ flip TR.find pathT $ invert
    (contains ".git" <|> contains "google-chrome" <|> contains "chromium")
  True <- testfile fname

  lift $ mktree $ directory fname
  catch
    (lift $ flip inshell empty $ "ln " <> foldl'
      -- TODO toText can either return the exact path or an approximation.
      -- for now this gets ignored, but eventually we should send the user a warning
      -- or something idk
      (\t fl -> t <> either id id (TR.toText fl))
      ""
      [fname, " ", pathF fname]
    )
    (\(a :: ExitCode) -> do
      echo $ "Exception when running the program: " <> fromString (show a)
      throwError
        "There was a problem linking the dotfiles. please check errors above."
    )


addFolder
  :: TR.FilePath
  -> [Text]
  -> (TR.FilePath -> TR.FilePath)
  -> ExceptT Text Shell Line
addFolder pathT argsDirs sysLoc = undefined

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
