-- Sokoban Highscore Utils

module SokobanHighscore 
    ( highScoreEntriesPerLevel
    , Highscore (..)
    , Highscores
    , levelHighscores
    , addHighscore
    , createUnnamedScoreForNow
    , setHighscoreName
    , loadHighscoresFromFile
    , saveHighscoresToFile
    ) where

import Prelude hiding (Either(..))
import System.Directory
import Data.Function(on)
import Data.Time
import Data.List (concatMap, sortBy, insertBy, deleteBy)
import Control.Monad(liftM)
import Sokoban

highScoreEntriesPerLevel :: Int
highScoreEntriesPerLevel = 5

type LevelNr = Int
data Highscore = Highscore { hsLevel :: LevelNr
                           , hsTime  :: LocalTime
                           , hsSteps :: Int
                           , hsName  :: String
                           } deriving (Show, Read)

data Highscores = HScs [(LevelNr, [Highscore])]

levelHighscores :: LevelNr -> Highscores -> [Highscore]
levelHighscores lvl = validateHighscores . filterLevel lvl

addHighscore :: Highscore -> Highscores -> Highscores
addHighscore h hs =
    let lvl = hsLevel h
        HScs hscs = hs
        levelScores = levelHighscores lvl hs
        updatedScores = validateHighscores $ replaceOn hsTime h levelScores
        newEntry = (lvl, updatedScores) in
        HScs $ replaceOn fst newEntry hscs

replaceOn :: Ord b => (a -> b) -> a -> [a] -> [a]
replaceOn m itm ls =
    let remList = deleteBy ((==) `on` m) itm ls in
        insertBy (compare `on` m) itm remList

validateHighscores :: [Highscore] -> [Highscore]
validateHighscores = take highScoreEntriesPerLevel . (sortBy (compare `on` hsSteps))

filterLevel :: LevelNr -> Highscores -> [Highscore]
filterLevel lvl (HScs hs) = concatMap snd $ filter ((==lvl) . fst) hs

createUnnamedScoreForNow :: LevelNr -> Int -> IO Highscore
createUnnamedScoreForNow lvlNr st = do
    tz <- getCurrentTimeZone
    now <- (liftM $ utcToLocalTime tz) getCurrentTime
    return Highscore { hsLevel = lvlNr, hsSteps = st, 
                       hsTime = now, hsName = "Unknown"}

setHighscoreName :: String -> Highscore -> Highscore
setHighscoreName n hs = hs { hsName = n }

loadHighscoresFromFile :: String -> IO Highscores
loadHighscoresFromFile path = do
    fileEx <- doesFileExist path
    if fileEx 
        then (liftM $ HScs . read) $ readFile path
        else do return $ HScs []

saveHighscoresToFile :: String -> Highscores -> IO ()
saveHighscoresToFile path (HScs hs) = writeFile path $ show hs
