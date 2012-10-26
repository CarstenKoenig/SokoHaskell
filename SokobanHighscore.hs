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
import Data.List (concatMap, sort, insertBy, deleteBy, maximum)
import Control.Monad(liftM, when)
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
                           } deriving (Show, Read)

data Highscores = HScs [(LevelNr, [Highscore])]

instance Eq Highscore where
    a == b = (hsLevel a == hsLevel b) && (hsTime a == hsTime b)

instance Ord Highscore where
    compare a b
        | hsLevel a < hsLevel b = LT
        | hsLevel a > hsLevel b = GT
        | hsSteps a < hsSteps b = LT
        | hsSteps a > hsSteps b = GT
        | hsTime a < hsTime b   = LT
        | hsTime a > hsTime b   = GT
        | otherwise             = EQ

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

isHighscore :: Highscore -> Highscores -> Bool
isHighscore h hs = 
    length lvlHs < highScoreEntriesPerLevel
    || maxSteps > hsSteps h
    where lvlHs = levelHighscores (hsLevel h) hs
          maxSteps = maximum . (map hsSteps) $ lvlHs

replaceOn :: Ord b => (a -> b) -> a -> [a] -> [a]
replaceOn m itm ls =
    let remList = deleteBy ((==) `on` m) itm ls in
        insertBy (compare `on` m) itm remList

validateHighscores :: [Highscore] -> [Highscore]
validateHighscores = take highScoreEntriesPerLevel . sort

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
    highscore <- (liftM $ levelHighscores lvl) $ loadHighscoresFromFile highscoreFilePath
    displayHighscoreDialog highscore False

enterHighscoreDialog :: LevelNr -> Int -> IO ()
enterHighscoreDialog lvl steps = do
    hs <- loadHighscoresFromFile highscoreFilePath
    newScore <- (liftM $ setHighscoreName "**YOU**") $ createUnnamedScoreForNow lvl steps
    let highscore = levelHighscores lvl $ addHighscore newScore hs

    displayHighscoreDialog highscore (isHighscore newScore hs)

displayHighscoreDialog :: [Highscore] -> Bool -> IO ()
displayHighscoreDialog highscores newHighscore = do
    builder <- builderNew
    builderAddFromFile builder "Highscore.glade"

    dialog <- builderGetObject builder castToDialog "dialogHighscore"

    treeView <- builderGetObject builder castToTreeView "listHighscore"
    populateHighscoreStore highscores treeView

    enterNameBox <- builderGetObject builder castToVBox "vboxEnterName"
    when (not newHighscore) $ do widgetHide enterNameBox

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

