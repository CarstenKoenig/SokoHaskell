-- Sokoban - cloning the Sokobangame in Haskell

module Sokoban where

import Prelude hiding (Either(..))

import Data.List (sort, delete)
import Data.Char (toLower)

import Control.Monad (liftM, forM)

type Coord = (Int, Int)
type MovedWithCrate = Bool

data Level = Level { width    :: Int
                   , height   :: Int 
                   , worker   :: Coord
                   , walls    :: [Coord]
                   , storages :: [Coord]
                   , crates   :: [Coord]
                   , steps    :: Int
                   } deriving (Show)

data Move = Up | Down | Left | Right
			deriving (Show, Eq)

emptyLevel :: Level
emptyLevel = Level { width    = 0
                   , height   = 0
                   , worker   = (0,0)
                   , walls    = []
                   , storages = []
                   , crates   = []
                   , steps    = 0
                   }

step :: Move -> Level -> Maybe (Level, MovedWithCrate)
step mv lvl
	| isWallAt lvl nextCoord  = Nothing
	| isCrateAt lvl nextCoord =
		if isWallAt lvl nextCoord'
		   || isCrateAt lvl nextCoord'
	    then Nothing
	    else Just (lvl { crates = moveCrate, worker = nextCoord, steps = 1+steps lvl }, True)
	| otherwise               = Just (lvl { worker = nextCoord, steps = 1+steps lvl }, False)
	where nextCoord  = moveCoord (worker lvl) mv
	      nextCoord' = moveCoord nextCoord mv
	      moveCrate  = nextCoord':(delete nextCoord $ crates lvl)

stepBack :: (Move, MovedWithCrate) -> Level -> Level
stepBack (mv, withCrate) lvl
    | isWallAt lvl prevCoord = lvl
    | isCrateAt lvl prevCoord = lvl
    | isCrateAt lvl nextCoord && withCrate =
        lvl { crates = moveCrate, worker = prevCoord, steps = steps lvl -1 }
    | otherwise =
        lvl { worker = prevCoord, steps = steps lvl -1 }
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

showLevel :: Level -> [String]
showLevel l = map showLine lns
	where lns = [0..height l - 1]
	      showLine y = map showCoord [(x,y) | x <- [0..width l - 1]]
	      showCoord c
	      	| isWallAt l c    = '#'
	      	| isWorkerAt l c && isStorageAt l c
	      	                  = '+'
	      	| isWorkerAt l c  = '@'
	      	| isCrateAt l c && isStorageAt l c
	      	                  = '*'
	      	| isCrateAt l c   = 'o'
	      	| isStorageAt l c = '.'
	      	| otherwise       = ' '