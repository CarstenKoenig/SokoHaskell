-- Sokoban Highscore Utils

module SokobanHighscore 
    ( highScoreEntriesPerLevel
    , Highscore (..)
    , Highscores
    , LevelNr
    , levelHighscores
    , addHighscore
    , createUnnamedScoreForNow
    , setHighscoreName
    , loadHighscoresFromFile
    , saveHighscoresToFile
    , showHighscoreDialog
    ) where

import Prelude hiding (Either(..))
import System.Directory
import Data.Function(on)
import Data.Time
import Data.List (concatMap, sortBy, insertBy, deleteBy)
import Control.Monad(liftM)
import Sokoban

import Graphics.UI.Gtk hiding (on)
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.ModelView as Model

highScoreEntriesPerLevel :: Int
highScoreEntriesPerLevel = 5

highscoreFilePath :: String
highscoreFilePath = "Highscores.txt"

type LevelNr = Int
data Highscore = Highscore { hsLevel :: LevelNr
                           , hsTime  :: LocalTime
                           , hsSteps :: Int
                           , hsName  :: String
                           } deriving (Show, Read, Eq)

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

showHighscoreDialog :: LevelNr -> IO ()
showHighscoreDialog lvl = do
    --hs <- (liftM $ levelHighscores lvl) $ loadHighscoresFromFile highscoreFilePath
    hs <- loadHighscoresFromFile highscoreFilePath
    dummy <- (liftM $ setHighscoreName "*****") $ createUnnamedScoreForNow lvl 42
    let highscore = levelHighscores lvl $ addHighscore dummy hs

    builder <- builderNew
    builderAddFromFile builder "Highscore.glade"

    dialog <- builderGetObject builder castToDialog "dialogHighscore"
    treeView <- builderGetObject builder castToTreeView "listHighscore"

    populateHighscoreStore highscore treeView

    --closeButton <- builderGetObject builder castToButton "buttonOk"
    --onClicked closeButton $ do widgetDestroy dialog

    dlgRes <- dialogRun dialog
    widgetDestroy dialog

populateHighscoreStore :: [Highscore] -> TreeView -> IO ()
populateHighscoreStore hs treeView = do
    store  <- listStoreNew $ zip [1..] hs

    addColumn store "nr. "   (\(i,_) -> show i)
    addColumn store "steps " (\(_,v) -> show $ hsSteps v)
    addColumn store "name "  (\(_,v) -> hsName v)
    addColumn store "date "  (\(_,v) -> show $ hsTime v)

    Model.treeViewSetModel treeView store

    where 
        addColumn store title getValueText = do
            column <- Model.treeViewColumnNew
            Model.treeViewColumnSetTitle column title
            cell <- Model.cellRendererTextNew
            Model.cellLayoutPackStart column cell True
            Model.cellLayoutSetAttributes column cell store $ 
                \row -> [ Model.cellText := getValueText row ]
            Model.treeViewAppendColumn treeView column

