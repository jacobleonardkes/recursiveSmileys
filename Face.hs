module Face where
--import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Control.Monad
import Base
import RandomUtils
import System.Random
import RenderBase
import Control.Monad.State

data FaceSpec = FaceSpecUndef | FaceSpec {
	eyes :: [FaceSpec],
	color :: ColorSpec,
	eyeArcRadius :: Double, -- relative to face radius of 1
	eyeArcRadians :: Double,
	eyeRadius :: Double,
	mouthRadius :: Double,
	mouthRadians :: Double,
	theta :: Double
} deriving (Show,Ord,Eq,Read)

defaultFaceSpec :: FaceSpec
defaultFaceSpec = FaceSpec {
	eyes = [FaceSpecUndef,FaceSpecUndef],
	color = colorYellow, --ColorSpec 1 1 0, -- yellow
	eyeArcRadius = 0.6,
	eyeArcRadians = 0.4 * pi,
	eyeRadius = 0.3,
	mouthRadius = 0.7,
	mouthRadians = 0.8 * pi,
	theta = 0
}

data FacePosition = FacePosition {
	xyCoord :: XYCoord,
	radius :: Double
} deriving (Show,Eq,Ord,Read)

defaultFacePosition :: Double -> Double -> FacePosition
defaultFacePosition width height = FacePosition{ 
	xyCoord = XYCoord (width/2) (height/2), 
	radius = min (width/2) (height/2)
}

instance Positional FacePosition where
	xCoord = xCoord . xyCoord
	yCoord = yCoord . xyCoord

data Face = Face FaceSpec FacePosition
	deriving (Show, Eq, Ord, Read)

defaultFace :: Double -> Double -> Int -> Face
defaultFace width height recursionDepth = 
	Face defaultFaceSpec (defaultFacePosition width height)

populateFaceSpec :: Int -> FaceSpec -> IO FaceSpec
populateFaceSpec 0 f = return f
populateFaceSpec n f | n > 0 = do
	newEyes <- mapM (\e -> case e of FaceSpecUndef -> mutateFaceSpec f; _ -> return e) $ eyes f
	newEyes' <- mapM (populateFaceSpec (n-1)) newEyes
	return $ f{ eyes = newEyes' }
populateFaceSpec _ _ = error "Can't populate to negative depth"

mutateFaceSpec FaceSpecUndef = mutateFaceSpec defaultFaceSpec
mutateFaceSpec fs = let rate = 0.2 in do
	numEyes <- randomJumpR rate (0, 3) $ length $ eyes fs
	color' <- randomIf 0.85 randomColorSpec (return $ color fs)
	eyeRadius' <- randomJumpR rate (0.05, 0.3) $ eyeRadius fs
	eyeArcRadius' <- randomJumpR rate (0.05, 1.0) $ eyeArcRadius fs
	-- need smarter logic for eyeArcRadians to avoid intersecting eyes
	eyeArcRadians' <- randomJumpR rate (pi/6, pi) $ eyeArcRadians fs
	mouthRadius' <- randomJumpR rate (0.2, 0.95) $ mouthRadius fs
	mouthRadians' <- randomJumpR rate (pi/6, pi) $ mouthRadians fs
	theta' <- randomJumpR rate (0, 2*pi) $ theta fs
	return FaceSpec{
		eyes = take numEyes $ repeat FaceSpecUndef,
		color = color',
		eyeRadius = eyeRadius',
		eyeArcRadius = eyeArcRadius',
		eyeArcRadians = eyeArcRadians',
		mouthRadius = mouthRadius',
		mouthRadians = mouthRadians',
		theta = theta'
	}

instance Renderable Face where
	render (Face FaceSpecUndef _) = return ()
	render f@(Face fs fp) = do
		let x = xCoord fp
		let y = yCoord fp
		renderCircle (xyCoord fp) (radius fp) colorBlack $ Just $ color fs
		setSourceRGBFromColor colorBlack
		arc x y (radius fp*mouthRadius fs) (pi/2-mouthRadians fs/2+theta fs) (pi/2+mouthRadians fs/2+theta fs)
		stroke
		mapM_ render $ zipWith Face (eyes fs) (eyePositions f)

	click (Face FaceSpecUndef _) _ = return ()
	click f@(Face fs _) p = do
		let eyeFaces = zipWith Face (eyes fs) (eyePositions f)
		let eyeFaces' = filter (`contains` p) eyeFaces
		if length eyeFaces' > 0
			then click (head eyeFaces') p
			else putStrLn $ show f

	minDistance (Face _ fp) p = distance fp p - radius fp

eyePositions :: Face -> [FacePosition]
eyePositions (Face FaceSpecUndef _) = []
eyePositions (Face fs fp) = let r = eyeArcRadius fs * radius fp in 
        let eyePosition c = FacePosition{ xyCoord=c, radius = radius fp * eyeRadius fs } in
        case length $ eyes fs of
        	0 -> []
	        1 -> [eyePosition $ fp .+ XYCoord 0 (-r)]
	        s -> [eyePosition $ (xyCoord fp) .+ (RTCoord r $ (theta fs) + eyeArcRadians fs * (i/s'-0.5) + (pi*1.5)) | i <- [0.0..s'] ] where s' = fromIntegral s - 1.0

newFacePositions :: Double -> Double -> Int -> IO [FacePosition]
newFacePositions canvasW canvasH n = sub [] n where
	sub fps 0 = return fps
	sub fps n | n > 0 = do
		nfp <- sub' fps
		sub (nfp:fps) (n-1)

	sub' fps = do
		-- trial and error becomes harmonic asymptotically, could be improved?
		x <- randomRIO (0.0, canvasW)
		y <- randomRIO (0.0, canvasH)
		let xy = XYCoord x y
		let spaces = map (\fp -> distance fp xy - radius fp) fps
		let spaces' = [x, y, canvasW - x, canvasH - y] ++ spaces
		if all (>0) spaces
			then return $ FacePosition{ xyCoord=xy, radius=minimum spaces'}
			else sub' fps

newFaceSpecs :: Int -> Int -> IO [FaceSpec]
newFaceSpecs num depth = do
	topLevel <- sequence $ replicate num $ mutateFaceSpec defaultFaceSpec
	mapM (populateFaceSpec depth) topLevel

newFaces :: Double -> Double -> Int -> Int -> IO [Face]
newFaces canvasW canvasH num depth = do
	positions <- newFacePositions canvasW canvasH num
	specs <- newFaceSpecs num depth
	return $ zipWith Face specs positions
	
