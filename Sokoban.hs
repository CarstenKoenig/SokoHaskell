-- Sokoban - cloning the Sokobangame in Haskell

module Sokoban where

import Prelude hiding (Either(..))

import Data.List (sort, delete)

type Coord = (Int, Int)
type MovedWithCrate = Bool

data Level = Level { width    :: Int
                   , height   :: Int 
                   , worker   :: Coord
                   , walls    :: [Coord]
                   , storages :: [Coord]
                   , crates   :: [Coord]
                   }

data Move = Up | Down | Left | Right
			deriving (Show, Eq)

emptyLevel :: Level
emptyLevel = Level { width    = 0
                   , height   = 0
                   , worker   = (0,0)
                   , walls    = []
                   , storages = []
                   , crates   = []
                   }

step :: Move -> Level -> Maybe (Level, MovedWithCrate)
step mv lvl
	| isWallAt lvl nextCoord  = Nothing
	| isCrateAt lvl nextCoord =
		if isWallAt lvl nextCoord'
		   || isCrateAt lvl nextCoord'
	    then Nothing
	    else Just (lvl { crates = moveCrate, worker = nextCoord }, True)
	| otherwise               = Just (lvl { worker = nextCoord }, False)
	where nextCoord  = moveCoord (worker lvl) mv
	      nextCoord' = moveCoord nextCoord mv
	      moveCrate  = nextCoord':(delete nextCoord $ crates lvl)

stepBack :: (Move, MovedWithCrate) -> Level -> Level
stepBack (mv, withCrate) lvl
    | isWallAt lvl prevCoord = lvl
    | isCrateAt lvl prevCoord = lvl
    | isCrateAt lvl nextCoord && withCrate =
        lvl { crates = moveCrate, worker = prevCoord }
    | otherwise =
        lvl { worker = prevCoord }
    where nextCoord = moveCoord (worker lvl) mv
          prevCoord = moveCoord (worker lvl) (opposite mv)
          moveCrate = (worker lvl):(delete nextCoord $ crates lvl)

opposite :: Move -> Move
opposite r
    | r == Down  = Up
    | r == Up    = Down
    | r == Left  = Right
    | r == Right = Left

moveCoord :: Coord -> Move -> Coord
moveCoord (x, y) r
    | r == Up    = (x, y-1)
    | r == Down  = (x, y+1)
    | r == Left  = (x-1, y)
    | r == Right = (x+1,y)


isFinished :: Level -> Bool
isFinished lvl = (sort $ crates lvl) == (sort $ storages lvl)

isWallAt :: Level -> Coord -> Bool
isWallAt l c = elem c $ walls l

isCrateAt :: Level -> Coord -> Bool
isCrateAt l c = elem c $ crates l

isStorageAt :: Level -> Coord -> Bool
isStorageAt l c = elem c $ storages l

isWorkerAt :: Level -> Coord -> Bool
isWorkerAt l c = c == worker l
