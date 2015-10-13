{-# LANGUAGE OverloadedStrings #-}

module Parser (linesParser) where

------------------------------------------------------------------------------------
import qualified Data.Attoparsec.Combinator as P
import qualified Data.Attoparsec.Text       as P
import           Data.Text                  (Text)
------------------------------------------------------------------------------------

-- | Feed a list of text fragments to a parser until it is done, fail, or we get
-- end-of-list.
linesParser :: P.Parser r
               -> [Text]
               -> Either (Text, Maybe (P.IResult Text r)) (Int, P.IResult Text r)
linesParser _ [] = Left ("eof", Nothing)
linesParser f (x:xs) = case P.parse f x of
          r@(P.Partial _) -> pf 1 r xs
          r@_             -> Right (1, r)
    where pf c partial (x':xs') =  case P.feed partial x' of
                                       (P.Partial _) -> pf (c + 1) partial xs'
                                       r@_           -> Right (c + 1, r)
          pf _ partial [] = Left ("eof", Just partial)
