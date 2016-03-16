{-# LANGUAGE OverloadedStrings #-}

module Text (lineSplit) where

------------------------------------------------------------------------------------
import           Data.Text                  (Text)
import qualified Data.Text                  as T
------------------------------------------------------------------------------------

-- | A line split function that preserves all character, unlike
-- the standard 'lines' function, where `lines "foo\n" == lines "foo"`.
lineSplit :: Text -> [Text]
lineSplit t = reverse $ r $ reverse $ map f z
    where s            = T.splitOn ("\n") t
          n            = length s
          z            = zip s (map (== n) [1..])
          f (x, False) = T.concat [x, "\n"]
          f (x, True)  = x
          r ("":xs)    = xs
          r xs         = xs
