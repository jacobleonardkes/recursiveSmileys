module Base where
import System.Random

data ColorSpec = ColorSpec Double Double Double deriving (Show,Ord,Eq,Read)

randomColorSpec = do
	r <- randomRIO (0.0, 1.0)
	g <- randomRIO (0.0, 1.0)
	b <- randomRIO (0.0, 1.0)
	return $ ColorSpec r g b

colorBlack = ColorSpec 0 0 0
colorYellow = ColorSpec 1 1 0
colorRed = ColorSpec 1 0 0
colorWhite = ColorSpec 1 1 1
colorGreen = ColorSpec 0 1 0
colorBlue = ColorSpec 0 0 1
colorPurple = ColorSpec 1 0 1

-- |A Cartesian coordinate
data XYCoord = XYCoord Double Double
	deriving (Show,Eq,Ord,Read)

class Positional p where
	xCoord, yCoord :: p -> Double

toXYCoord :: (Positional p) => p -> XYCoord
toXYCoord p = XYCoord (xCoord p) (yCoord p)

(.+) :: (Positional p, Positional q) => p -> q -> XYCoord
(.+) p q = XYCoord (xCoord p + xCoord q) (yCoord p + yCoord q)

instance Positional XYCoord where
	xCoord (XYCoord x _) = x
	yCoord (XYCoord _ y) = y

data RTCoord = RTCoord Double Double
	deriving (Show, Eq, Ord, Read)

instance Positional RTCoord where
	xCoord (RTCoord r t) = r * cos t
	yCoord (RTCoord r t) = r * sin t

distance :: (Positional p, Positional q) => p -> q -> Double
distance p1 p2 = sqrt $ (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) where
	x1 = xCoord p1
	x2 = xCoord p2
	y1 = yCoord p1
	y2 = yCoord p2
