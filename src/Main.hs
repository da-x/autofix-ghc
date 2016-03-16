{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

------------------------------------------------------------------------------------
import           Control.Monad              (foldM, forM_, when)
import qualified Data.Attoparsec.Combinator as P
import qualified Data.Attoparsec.Text       as P
import           Data.List                  (groupBy, intersperse, sort)
import           Data.Maybe                 (catMaybes)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           System.FilePath            ((</>))
import           System.IO.Temp             (withSystemTempDirectory)
import           Text.Regex.TDFA            ((=~))
import           Text.Regex.TDFA.Text       ()
import qualified Options.Applicative        as O
import           Options.Applicative        ((<>), (<**>))
----
import           Parser                     (linesParser)
import           Process                    (readProcess'')
import           Text                       (lineSplit)
------------------------------------------------------------------------------------

warningRegex :: Text
warningRegex = "([^:]+):([0-9]+):([0-9]+): +Warning:\n"

type Path = Text
type Line = Int
type ModName = Text
type FileContent = Text

data Problem = Problem Path DescInt
    deriving (Show, Eq, Ord)

data DescInt = DescInt Int Desc
    deriving (Show, Eq, Ord)

data Desc
    = RedundantPart Text ModName
    | RedundantImport ModName     -- TODO: implement fix
    | TopLevelBind Text Text      -- TODO: implement fix
    deriving (Show, Eq, Ord)

parseLine :: forall a. (Num a, Read a) => Text -> a
parseLine line = (read $ T.unpack line) - 1

redundantPart :: FileContent -> [Problem]
redundantPart t = catMaybes $ map f ((t =~ redundantPartRegEx) :: [[Text]])
   where f [_, name, line, _, stuff, mods] =
             Just $ Problem name $ DescInt (parseLine line) $ RedundantPart stuff mods
         f _ = Nothing
         redundantPartRegEx :: Text
         redundantPartRegEx = T.concat
             [ warningRegex
             , " +The import of [‘]([^’]+)[’][ \n]+from module [‘]([^’]+)[’] is redundant\n"
             ]

redundantImport :: FileContent -> [Problem]
redundantImport t = catMaybes $ map f ((t =~ redundantImportRegEx) :: [[Text]])
   where f [_, name, line, _, mods] =
             Just $ Problem name $ DescInt (parseLine line) $ RedundantImport mods
         f _ = Nothing
         redundantImportRegEx :: Text
         redundantImportRegEx = T.concat
            [warningRegex
            , " +The qualified import of ‘([^’]+)’[ \n]+is[ \n]+redundant"
            ]

topLevelBind :: FileContent -> [Problem]
topLevelBind t = catMaybes $ map f ((t =~ topLevelBindRegEx) :: [[Text]])
   where f [_, name, line, _, funcname, typedef, _] =
             Just $ Problem name $ DescInt (parseLine line) $ TopLevelBind funcname typedef
         f _ = Nothing
         topLevelBindRegEx :: Text
         topLevelBindRegEx = T.concat
             [ warningRegex
             , "[ \t]+Top-level binding with no type signature:\n"
             , "[ \t]+([a-zA-Z_'0-9]+) +:: +(([^\n]+\n)*)\n"
             ]

data Import = Import
    { _whiteSpace1 :: Text
    , impModName   :: ModName
    , _whiteSpace2 :: Text
    , impList      :: [Imp]
    } deriving Show

data ImpKind = Op | Reg
    deriving Show

data Imp = Imp
    { impKind    :: ImpKind
    , impText    :: Text
    , impConstrs :: (Maybe Text)
    } deriving Show

importGenerator :: Import -> Text
importGenerator (Import ws1 modname ws2 implist) =
    T.concat $ [untilPartList] ++ (intersperse ", " (map part implist)) ++ [")"]
    where untilPartList = T.concat ["import", ws1, modname, ws2, "("]
          part (Imp Reg t c) = T.concat [t, constToText c]
          part (Imp Op t c) = T.concat ["(", t, ")", constToText c]
          constToText Nothing = ""
          constToText (Just x) = T.concat ["(", x, ")"]

importParser :: P.Parser Import
importParser = Import <$> (P.string "import" *> takespace1) <*> modName
                <*> takespace <*> importParts
    where importParts = P.string "(" *> (part `P.sepBy` (P.string ",")) <* P.string ")"
          part        = skipspace *> P.choice [
                             Imp <$> (P.string "(" *> (pure Op)) <*> (P.takeWhile ops <* P.string ")")
                                                <*> constructors' <* skipspace
                           , Imp <$> (pure Reg) <*> idName <*> constructors' <* skipspace
                        ]
          constructors' = skipspace *> P.option Nothing constructors
          constructors  = P.string "(" *> skipspace *> (Just <$> idName) <* skipspace <* P.string ")"
          takespace     = P.takeWhile space
          takespace1    = P.takeWhile1 space
          skipspace     = P.skipMany (P.satisfy space)
          space ' '     = True
          space '\n'    = True
          space '\t'    = True
          space _       = False
          ops   '.'     = True
          ops   '*'     = True
          ops   '/'     = True
          ops   '<'     = True
          ops   '>'     = True
          ops   '?'     = True
          ops   '^'     = True
          ops   '&'     = True
          ops    _      = False
          idName = charAndRest idNameStart idChar
          modName = charAndRest idNameStart idModChar
          charAndRest f1 f2 = do x <- P.satisfy f1
                                 y <- P.takeWhile f2
                                 return $ T.concat [T.pack [x], y]
          idNameStart c | c >= 'A' && c <= 'Z' = True
                        | c >= 'a' && c <= 'z' = True
                        | c == '_'             = True
                        | otherwise            = False
          idChar c      | idNameStart c        = True
                        | c >= '0' && c <= '9' = True
                        | c == '\''            = True
                        | otherwise            = False
          idModChar c   | idNameStart c        = True
                        | c >= '0' && c <= '9' = True
                        | c == '.'             = True
                        | otherwise            = False

type Count = Int
type FirstLine = Int
type Changes = [(FirstLine, Count, [Text])]

-- | Take a file (splitted to lines), a problem description, and return the
-- changes that need to happen in the file in order to fix the problem.
--
-- Each change tells the zero-based index of the place to modify, how many
-- lines to remove from there, and the new lines to put in.
fixProblem :: [Text] -> DescInt -> Changes
fixProblem t di = fixImportProblem di
    where fixImportProblem (DescInt linenum (RedundantPart removals _)) =
              case linesParser importParser (drop linenum t) of
                   Right (nrlines, P.Done _ imp) ->
                       [(linenum, nrlines, [T.concat
                            [importGenerator (filterImp removals imp), "\n"]])]
                   _ -> err
          fixImportProblem _      = err

          err                     = [] -- [(linenum'', 0, [T.concat ["-- ", T.pack $ show d', "\n"]])]
          filterImp  removals imp = imp { impList = filter filterPart (impList imp) }
              where
                  filterPart part = not ((impText part) `elem` removalList)
                  removalList     = filter (/= "") $ T.splitOn " " $ T.map
                                      (\case '\n' -> ' '; ',' -> ' '; x -> x) removals

data Opts = Opts {
      optDiff        :: Bool
    , optFix         :: Bool
    } deriving (Show)

optsParse :: O.Parser Opts
optsParse = Opts
     <$> O.switch
         ( O.long "diff"
        <> O.short 'd'
        <> O.help "Whether to show a diff" )
     <*> O.switch
         ( O.long "fix"
        <> O.short 'f'
        <> O.help "Whether to fix in-place" )

main :: IO ()
main = do
    Opts {..} <- O.execParser (O.info (optsParse <**> O.helper) O.idm)

    t <- T.getContents
    let allProblems = concat $ [redundantImport, topLevelBind, redundantPart] <*> [t]
        allSortedProblems =
            map (\xs@(Problem a _:_) -> (a, map (\(Problem _ b) -> b) xs))
            $ groupBy (\(Problem a _) (Problem b _) -> a == b)
            $ sort allProblems

    withSystemTempDirectory "autofix-ghc" $ \tempDir -> do
        forM_ allSortedProblems $ \(filename, descints') -> do
            content' <- fmap lineSplit $ T.readFile $ T.unpack filename
            let fixOne content [] = return content
                fixOne content (x:xs) = do
                    let changes = fixProblem content x
                    let moddesc linenum diff (DescInt linenum' desc)
                           | linenum' >= linenum = DescInt (linenum' + diff) desc
                           | otherwise           = DescInt linenum' desc

                    (mcontent, xs') <- flip (flip foldM (content, xs)) changes $
                        \(content'', xs') (pos, count, newContent)-> do
                            return $ ((take pos content'') ++
                                      newContent ++ (drop (pos + count) content''),
                                      map (moddesc pos (length newContent - count)) xs')

                    fixOne mcontent xs'

            modifiedLines <- fixOne content' descints'
            let tempfile = tempDir </> "f"
                newContent = T.concat modifiedLines

            when (not optFix) $ do
                T.putStrLn $ T.concat ["autofix-ghc can fix: ", filename]

            when optDiff $ do
                T.writeFile tempfile newContent
                (_, diff, _) <- readProcess'' "diff" ["-urN", filename, T.pack tempfile,
                                                      "--label", filename, "--label", filename] ""
                T.putStrLn $ T.concat ["diff -urN ", filename, " ", filename]
                T.putStr diff

            when optFix $ do
                T.putStrLn $ T.concat ["autofix-ghc fixed: ", filename]
                T.writeFile (T.unpack filename) newContent
