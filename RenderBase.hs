module RenderBase where
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Events
import Control.Monad
import Base

setSourceRGBFromColor :: ColorSpec -> Render ()
setSourceRGBFromColor (ColorSpec r g b) = setSourceRGB r g b

renderCircle :: (Positional p) => p -> Double -> ColorSpec -> Maybe ColorSpec -> Render ()
renderCircle p r borderCS fillCS = do
	setSourceRGBFromColor borderCS
	arc (xCoord p) (yCoord p) r 0 (2*pi)
	closePath
	stroke
	maybe (return ()) (fillCircle p r) fillCS

fillCircle p r cs = do
	setSourceRGBFromColor cs
	arc (xCoord p) (yCoord p) r 0 (2*pi)
	fill

class Renderable r where
	render :: r -> Render ()
	click :: (Positional p) => r -> p -> IO ()
	click _ _ = return ()
	minDistance :: (Positional p) => r -> p -> Double
	contains :: (Positional p) => r -> p -> Bool
	contains r p = minDistance r p < 0

newWindow :: (Renderable r) => Int -> Int -> [r] -> IO ()
newWindow canvasW canvasH faces = do
	initGUI
	window	  <- windowNew
	drawingArea <- drawingAreaNew
	onButtonPress drawingArea (onClickHandler faces)
	containerAdd window drawingArea
	drawingArea `onExpose` (\_ -> renderScene drawingArea faces)
	window `onDestroy` mainQuit
	windowSetDefaultSize window canvasW canvasH
	widgetShowAll window
	mainGUI

renderScene :: (Renderable r) => DrawingArea -> [r] -> IO Bool
renderScene da rs = do
	dw <- widgetGetDrawWindow da
	renderWithDrawable dw $ do 
		setSourceRGBA 0.9 0.5 0.5 1.0
		mapM_ render rs
	return True

onClickHandler :: (Renderable r) => [r] -> Event -> IO Bool
onClickHandler faces e = do
	let p = XYCoord (eventX e) (eventY e)
	let faces' = filter (`contains` p) faces
	if length faces' > 0 then click (head faces') p else return ()
	return True

