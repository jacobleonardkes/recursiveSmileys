module Main where
import Face
import RenderBase
import System.Environment

main = do
	argv <- getArgs
	let canvasW = read $ argv !! 0
	let canvasH = read $ argv !! 1
	let numFaces = read $ argv !! 2
	let depth = read $ argv !! 3
	faces <- newFaces (fromIntegral canvasW) (fromIntegral canvasH) numFaces depth
	newWindow canvasW canvasH faces
	
