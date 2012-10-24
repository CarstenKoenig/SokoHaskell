-- Sokoban - managing the Games State

module SokobanState (
    GameState (currentLevel), 
    startGame, restartLevel, nextLevel, updateLevel,
    levelNr,
    loadLevelsFromFile, loadLevelsFromArgs
    ) where

import Prelude hiding (Either(..))
import System.Environment (getArgs)

import Sokoban

import Data.List (sort, delete)
import Data.Char (toLower)

import Control.Monad (liftM, forM)

data GameState = GameState { gameLevels :: [Level]
                           , currentLevelIndex :: Int
                           , currentLevel :: Level
                           }

startGame :: [Level] -> GameState
startGame lvls = GameState { gameLevels = lvls, currentLevelIndex = 0, currentLevel = lvls!!0 }

restartLevel :: GameState -> GameState 
restartLevel state = state { currentLevel = (gameLevels state) !! (currentLevelIndex state) }

updateLevel :: GameState -> (Level -> Level) -> GameState
updateLevel state upd = state { currentLevel = upd $ currentLevel state }

nextLevel :: GameState -> GameState
nextLevel state = 
    state { currentLevelIndex = i, currentLevel = (gameLevels state)!!i }
    where i = currentLevelIndex state + 1 `mod` (length $ gameLevels state)

levelNr :: GameState -> Int
levelNr state = currentLevelIndex state + 1

loadLevelsFromArgs :: IO [Level]
loadLevelsFromArgs = do
    args <- getArgs
    if length args > 0
        then loadLevelsFromFile $ args!!0
        else return $ [parseLevel testLevelSource]

loadLevelsFromFile :: String -> IO [Level]
loadLevelsFromFile path = do
    source <- liftM lines $ readFile path
    return $ parseLevels source

parseLevels :: [String] -> [Level]
parseLevels = (map  parseLevel) . (split [])
	where 
		split :: [String] -> [String] -> [[String]]
		split acc []     = if length acc > 0 then [reverse acc] else []
		split acc ([]:r) = (reverse acc):(split [] r)
		split acc (h:r)  = split (h:acc) r

parseLevel :: [String] -> Level
parseLevel input = foldl parseLevelLine emptyLevel ls
	where ls = zip [0..] input

parseLevelLine :: Level -> (Int, [Char])-> Level
parseLevelLine lvl (lnr, cs) = foldl parseLevelElement lvl itms
	where itms = zip [(x, lnr) | x <- [0..]] cs

parseLevelElement :: Level -> (Coord, Char) -> Level
parseLevelElement l (c, e)
	| e == '@' = l { worker  = c, width = updateWidth l c, height = updateHeight l c }
	| e == 'o' = l { crates  = c:crates l, width = updateWidth l c, height = updateHeight l c }
	| e == '#' = l { walls   = c:walls l, width = updateWidth l c, height = updateHeight l c }
	| e == '.' = l { storages = c:storages l, width = updateWidth l c, height = updateHeight l c }
	| e == '*' = l { crates  = c:crates l, storages = c:storages l, width = updateWidth l c, height = updateHeight l c }
	| e == '+' = l { storages = c:storages l, worker = c, width = updateWidth l c, height = updateHeight l c }
	| e == ' ' = l { width = updateWidth l c, height = updateHeight l c }
	where 
		updateWidth l (x, _)  = max (x+1) $ width l
		updateHeight l (_, y) = max (y+1) $ height l

testLevelSource :: [String]
testLevelSource =
	["#####"
	,"#.o@#"
	,"#####"]   