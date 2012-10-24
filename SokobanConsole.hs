-- Sokoban.Console application

module Sokoban.Console where

import Prelude hiding (Either(..))

import System.Environment (getArgs)
import System.IO(stdin, hSetEcho, getChar, hSetBuffering, BufferMode(..))

import Data.Char (toLower)
import Control.Monad (liftM)


import Sokoban

gameLoop :: Level -> IO ()
gameLoop lvl = do
	putStrLn $ unlines $ showLevel lvl
	if isFinished lvl
		then putStrLn "F I N I S H E D - congratulations!"
		else continue
	where continue = do
		inp <- getChar
		case toLower inp of
			'w'       -> gameLoop (step Up lvl)
			'a'       -> gameLoop (step Left lvl)
			's'       -> gameLoop (step Down lvl)
			'd'       -> gameLoop (step Right lvl)
			'q'       -> return ()
			otherwise -> gameLoop lvl

main :: IO()
main = do
	hSetEcho stdin False
	hSetBuffering stdin NoBuffering
	args <- getArgs
	level <- liftM parseLevel $ readFileOrDefault args
	gameLoop level
	putStrLn "bye"
	where readFileOrDefault args = do
		if length args > 0
			then liftM lines $ readFile $ args!!0
			else return testLevelSource