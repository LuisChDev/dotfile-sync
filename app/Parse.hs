{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Parse
  ( parseInotify
  ) where

import           Data.Text                     as T
                                                ( drop )
import           Text.RE.TDFA.Text              ( (?=~)
                                                , Match
                                                , cp
                                                , re
                                                )
import           Text.RE.Tools                  ( (!$$) )
import           Turtle                         ( fromText )
import           Types                          ( INotify(..) )

parseInotify' :: Text -> Match Text
parseInotify' = (?=~ [re|(.*\/) (CREATE|DELETE)(,ISDIR)? (.*)|])

parseInotify :: Text -> INotify
parseInotify inString = constructor pathText
 where
  parsedString = parseInotify' inString
  constructor  = case parsedString !$$ [cp|2|] of
    "CREATE" -> Create
    "DELETE" -> Delete
    _ ->
      error
        $  "Hey, u forgot to update the iNotify constructors "
        <> show callStack
  pathText =
    fromText
      $  mappend ""
      $  T.drop 1
      $  (parsedString !$$ [cp|1|])
      <> (parsedString !$$ [cp|4|])
