import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events(Event( Configure ))
import Graphics.Rendering.Cairo
import Control.Monad

windowWidth = 300
windowHeight = 255

gridWidth = 10
gridHeight = 10

main :: IO ()
main= do
     initGUI
     window <- windowNew
     set window [windowTitle := "Hello Cairo",
                 windowDefaultWidth := windowWidth, windowDefaultHeight := windowHeight,
                 windowAllowGrow := True,
                 containerBorderWidth := 0 ]

     windowSetGeometryHints 
        window (Just window)
        (Just (windowWidth, windowHeight)) 
        (Just (windowWidth, windowHeight)) 
        Nothing Nothing Nothing

     frame <- frameNew
     containerAdd window frame
     canvas <- drawingAreaNew
     containerAdd frame canvas
     widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

     widgetShowAll window 
     drawin <- widgetGetDrawWindow canvas
     onExpose canvas (\x -> do renderWithDrawable drawin myDraw
                               return True)
   
     onDestroy window mainQuit
     mainGUI

myDraw :: Render ()
myDraw = do
    workerImg <- liftIO $ imageSurfaceCreateFromPNG "Worker.png"

    imgWidth <- imageSurfaceGetWidth workerImg
    imgHeight <- imageSurfaceGetHeight workerImg
    let scaleX = fromIntegral windowWidth / fromIntegral (gridWidth * imgWidth)
    let scaleY = 2.0 * fromIntegral windowHeight / fromIntegral (gridHeight * imgHeight)
    scale scaleX scaleY

    let walls = [(x,0) | x <- [0..gridWidth-1]] ++
                [(0,y) | y <- [1..gridHeight-2]] ++
                [(gridWidth-1,y) | y <- [1..gridHeight-2]] ++
                [(x,gridHeight-1) | x <- [0..gridWidth-1]]
    forM walls setWall

    showWorker (2, 3)

    where 
        setWall :: (Int, Int) -> Render ()
        setWall (x, y) = do
            wallImg <- liftIO $ imageSurfaceCreateFromPNG "Wall.png"
            imgWidth <- imageSurfaceGetWidth wallImg
            imgHeight <- imageSurfaceGetHeight wallImg
            setSourceSurface wallImg (fromIntegral (imgWidth * x)) (fromIntegral (imgHeight * y) * 0.5 - (fromIntegral imgHeight) * 0.33)
            paint
        showWorker :: (Int, Int) -> Render ()
        showWorker (x, y) = do
            workerImg <- liftIO $ imageSurfaceCreateFromPNG "Worker.png"
            imgWidth <- imageSurfaceGetWidth workerImg
            imgHeight <- imageSurfaceGetHeight workerImg
            setSourceSurface workerImg (fromIntegral (imgWidth * x)) (fromIntegral (imgHeight * y) * 0.5 - (fromIntegral imgHeight) * 0.33)
            paint

