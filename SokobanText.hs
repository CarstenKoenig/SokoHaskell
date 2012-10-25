-- Sokoban tranformation of Levels from/into text

module SokobanText (
        showLevel, loadLevelsFromArgs, loadLevelsFromFile
    ) where

import Prelude hiding (Either(..))
import System.Environment (getArgs)

import Control.Monad (liftM)

import Sokoban

instance Show Level where
    show = unlines . showLevel

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