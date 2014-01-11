module RandomUtils where
import System.Random

randomIf :: Double -> IO r -> IO r -> IO r
randomIf p a b = do
	x <- randomRIO (0.0, 1.0) 
	if x < p then a else b

randomJump :: (Random r) => Double -> r -> IO r
randomJump p r = randomIf p randomIO (return r)

randomJumpR :: (Random r) => Double -> (r,r) -> r -> IO r
randomJumpR p bounds r = randomIf p (randomRIO bounds) (return r)
