module Main where

import           Prelude                       as P

import           Control.Monad.Except           ( throwError )
import qualified Data.Text                     as T
import           Options.Applicative           as AP
import           Parse                          ( parseInotify )
import           Turtle                        as TR
import           Types                          ( Args(..)
                                                , INotify(..)
                                                , ShellE
                                                )



pathToLine :: TR.FilePath -> Line
pathToLine = fromString . encodeString

-- | takes the path as given in the folder and returns its location in
--   the filesystem
-- we can't simply use stripPrefix as that does't work with absolute paths
prependLocation' :: Text -> TR.FilePath -> TR.FilePath
prependLocation' path = decodeString . (<>) (T.unpack path) . encodeString

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
          (strOption $ long "adddir" <> short 'D' <> help
            "directories where you wish to sync dotfiles."
          )


-- | does most of the initial validation before dropping into the other routines.
main' :: Args -> ShellE Line
main' Args { argsDotPath, argsAdd, argsDirs } = do
  let dotfilesDir     = fromText argsDotPath
      prependLocation = prependLocation' argsDotPath

  ifM
    (testdir dotfilesDir)
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
      addFolder dotfilesDir argsDirs prependLocation
    else do
      echo "syncing dotfiles with repo"
      watchConfig dotfilesDir prependLocation
      -- syncConfig pathT prependLocation


syncFile fileSynced prependLocation = catch
  (lift $ flip inshell empty $ "ln " <> foldl'
    -- TODO toText can either return the exact path or an approximation.
    -- for now this gets ignored, but eventually we should send the user a warning
    -- or something idk
    (\t fl -> t <> either id id (TR.toText fl))
    ""
    [prependLocation fileSynced, " ", fileSynced]
  )
  (\(a :: ExitCode) -> do
    echo $ "Exception when running the program: " <> fromString (show a)
    throwError
      "There was a problem linking the dotfiles. please check errors above."
  )


syncConfig :: TR.FilePath -> (TR.FilePath -> TR.FilePath) -> ShellE Line
syncConfig dotfilesDir prependLocation = do
  fname <- lift $ flip TR.find dotfilesDir $ invert
    (contains ".git" <|> contains "google-chrome" <|> contains "chromium")
  True <- testfile fname

  mktree $ directory fname
  syncFile fname prependLocation

watchConfig :: TR.FilePath -> (TR.FilePath -> TR.FilePath) -> ShellE Line
watchConfig dotfilesDir prependLocation = do
  cd dotfilesDir
  echo =<< map pathToLine pwd
  event <- lift $ inshell "inotifywait -rm -e create,delete ." empty
  echo event

  case parseInotify $ lineToText event of
    Create path -> do
      echo "linking file to location in the filesystem"
      syncFile path prependLocation
    Delete path -> do
      echo "removing file"
      rm path
      pure "that's all folks"


addFolder
  :: TR.FilePath -> [Text] -> (TR.FilePath -> TR.FilePath) -> ShellE Line
addFolder pathT argsDirs prependLocation = do
  echo "adding folders"
  forM_ argsDirs $ \dir -> do
    echo $ "adding folder " <> unsafeTextToLine dir -- filenames don't contain newlines
  pure $ unsafeTextToLine "heh"


main :: IO ()
main = do
  params <- execParser $ info (args <**> helper) fullDesc
  TR.sh
    $ map
        (\case
          Left  t    -> t
          Right line -> lineToText line
        )
    $ runExceptT
    $ main' params
