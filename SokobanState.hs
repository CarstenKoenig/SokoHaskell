-- Sokoban - managing the Games State

module SokobanState (
    GameState (currentLevel), 
    startGame, restartLevel, nextLevel, 
    move, undoMove,
    levelNr, stepCount,
    loadLevelsFromFile, loadLevelsFromArgs
    ) where

import Prelude hiding (Either(..))

import Sokoban
import SokobanText
import SokobanHighscore (LevelNr)

data GameState = GameState { gameLevels :: [Level]
                           , currentLevelIndex :: LevelNr
                           , currentLevel :: Level
                           , moves :: [(Move, MovedWithCrate)]
                           }

startGame :: [Level] -> GameState
startGame lvls = GameState { gameLevels = lvls, currentLevelIndex = 0, currentLevel = lvls!!0, moves = [] }

restartLevel :: GameState -> GameState 
restartLevel state = state { currentLevel = (gameLevels state) !! (currentLevelIndex state), moves = [] }

move :: GameState -> Move -> GameState
move state mv =
    case (step mv $ currentLevel state) of
        Just (level, withCrate) ->
            if (not $ isFinished level)
                then state { currentLevel = level,
                             moves        = (mv, withCrate):(moves state) }
                else nextLevel state
        Nothing -> state

undoMove :: GameState -> GameState
undoMove state = undo $ moves state
    where undo []     = state
          undo (m:ms) = state { moves = ms, currentLevel = stepBack m $ currentLevel state }

nextLevel :: GameState -> GameState
nextLevel state = 
    state { currentLevelIndex = i, currentLevel = (gameLevels state)!!i, moves = [] }
    where i = currentLevelIndex state + 1 `mod` (length $ gameLevels state)

levelNr :: GameState -> LevelNr
levelNr state = currentLevelIndex state + 1

stepCount :: GameState -> Int
stepCount state = length $ moves state