-- Sokoban.UI - UI application

module Main (main) where

import Prelude hiding (Either(..))

import Control.Monad (liftM, forM, when, guard)
import Control.Concurrent.MVar

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events(Event( Configure ), eventKeyChar)
import Graphics.Rendering.Cairo

import Sokoban
import SokobanState

data Tile = WorkerTile | CrateTile | StorageTile | WallTile
type ImageMap = Tile -> Surface

coords :: Level -> [Coord]
coords lvl = [ (x,y) | y <- [0..height lvl -1], x <- [0..width lvl -1]]

getTilesAt :: Level -> Coord -> [Tile]
getTilesAt lvl c = ws ++ ss ++ cs ++ w
	where ws = ifIn isWallAt WallTile
	      ss = ifIn isCrateAt CrateTile
	      cs = ifIn isStorageAt StorageTile
	      w = ifIn isWorkerAt WorkerTile
	      ifIn attr t = if attr lvl c then [t] else []

windowWidth = 600
windowHeight = 480

main :: IO ()
main= do
    levels <- loadLevelsFromArgs
    gameState <- newMVar $ startGame levels
    imageMap <- createImageMap

    initGUI
    window <- windowNew
    set window [windowTitle := "Sokoban",
                windowDefaultWidth := windowWidth, windowDefaultHeight := windowHeight,
                windowAllowGrow := True,
                containerBorderWidth := 0 ]

    windowSetGeometryHints 
       window (Just window)
       (Just (windowWidth, windowHeight)) 
       (Just (windowWidth, windowHeight)) 
       Nothing Nothing Nothing

    widgetShowAll window 
    drawin <- widgetGetDrawWindow window
    onExpose window (\x -> do renderWithDrawable drawin (renderUi imageMap gameState)
                              return True)

    onKeyPress window (keyInput gameState window)
   
    onDestroy window mainQuit
    mainGUI

keyInput :: MVar GameState -> Window -> Event -> IO Bool
keyInput gameState wnd key = do
	state <- takeMVar gameState
	let keyChar = eventKeyChar key
	case keyChar of
		Just 'q' -> do
			mainQuit 
			return True
		Just 'r' -> restart state
		Just 'n' -> nextLevelIfPossible state
		Just 'u' -> undo state
		Just 'w' -> moveFwd state Up
		Just 'a' -> moveFwd state Left
		Just 's' -> moveFwd state Down
		Just 'd' -> moveFwd state Right
		otherwise -> do
			putMVar gameState state
			return True
	where
		nextLevelIfPossible state = do
			let newState = 
				if (True || (isFinished $ currentLevel state))
				then nextLevel state
				else state
			putMVar gameState $ newState
			redraw wnd
			return True
		restart state = do
			putMVar gameState $ restartLevel state
			redraw wnd
			return True
		moveFwd state mv = do
			putMVar gameState $ move state mv
			redraw wnd
			return True
		undo state = do
			putMVar gameState $ undoMove state
			redraw wnd
			return True
		redraw wnd = do
			win <- widgetGetDrawWindow wnd
			drawWindowInvalidateRect win (Rectangle 0 0 windowWidth windowHeight) False

createImageMap :: IO ImageMap
createImageMap = do
	worker  <- imageSurfaceCreateFromPNG "Worker.png"
	crate   <- imageSurfaceCreateFromPNG "Crate.png"
	storage <- imageSurfaceCreateFromPNG "Storage.png"
	wall    <- imageSurfaceCreateFromPNG "Wall.png"
	return $ \tile ->
		case tile of
			WorkerTile  -> worker
			CrateTile   -> crate
			StorageTile -> storage
			WallTile    -> wall

renderUi :: ImageMap -> MVar GameState -> Render ()
renderUi imgMap mv = do
	state <- liftIO $ readMVar mv
	let level = currentLevel state
	let gridWidth = width level
	let gridHeight = height level
	let wallImg = imgMap WallTile

	imgWidth <- imageSurfaceGetWidth wallImg
	imgHeight <- imageSurfaceGetHeight wallImg
	let scaleX = fromIntegral windowWidth / fromIntegral (gridWidth * imgWidth)
	let scaleY = 2.0 * fromIntegral windowHeight / fromIntegral (gridHeight * imgHeight)

	save
	scale scaleX scaleY
	forM (coords level) (showTiles level)
	restore

	printText $ "steps: " ++ (show $ steps level)
	printLevel state

	when (isFinished level) printSucceeded

	return ()

	where 
		showTiles :: Level -> Coord -> Render()
		showTiles level c = do
			forM (getTilesAt level c) (showTile c)
			return ()

		showTile :: Coord -> Tile -> Render()
		showTile (x,y) tile = do
		    let tileImg = imgMap tile
		    imgWidth <- imageSurfaceGetWidth tileImg
		    imgHeight <- imageSurfaceGetHeight tileImg
		    setSourceSurface 
		    	tileImg 
		    	(fromIntegral (imgWidth * x)) 
		    	(fromIntegral (imgHeight * y) * 0.5 - (fromIntegral imgHeight) * 0.33)
		    paint
		printText :: String -> Render()
		printText text = do
			selectFontFace "Sans" FontSlantNormal FontWeightBold
			setFontSize 30.0
			setSourceRGBA 1 0 0 0.5
			setLineWidth 2.5
			textSize <- textExtents text
			let tW = textExtentsWidth textSize + 10
			let tH = textExtentsHeight textSize + 10
			moveTo (fromIntegral windowWidth - tW) tH
			showText text
			return ()
		printLevel :: GameState -> Render()
		printLevel state = do
			let text = "Level " ++ (show $ levelNr state)
			selectFontFace "Sans" FontSlantNormal FontWeightBold
			setFontSize 30.0
			setSourceRGBA 0 0 1 0.6
			setLineWidth 2.5
			textSize <- textExtents text
			let tW = textExtentsWidth textSize + 10
			let tH = textExtentsHeight textSize + 10
			moveTo 10 tH
			showText text
			return ()
		printSucceeded :: Render()
		printSucceeded = do
			let text = "YOU made it!"
			selectFontFace "Sans" FontSlantNormal FontWeightBold
			setFontSize 50.0
			setSourceRGBA 0.1 1 0.1 1
			setLineWidth 2.5
			textSize <- textExtents text
			let tW = (textExtentsWidth textSize + 10) / 2
			let tH = (textExtentsHeight textSize + 10) / 2
			moveTo (fromIntegral windowWidth / 2 - tW) (fromIntegral windowHeight / 2 - tH)
			showText text
			return ()

