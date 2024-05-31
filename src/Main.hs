import Data.Matrix (Matrix, fromList, getElem, elementwise, nrows, ncols)

-- Define the original color
x, y :: Double
x = 0.54
y = 0.362

originalColor :: Matrix Double
originalColor = fromList 2 1 [x, y]

xw, yw :: Double
xw = 0.345 
yw = 0.352

whitePoint :: Matrix Double
whitePoint = fromList 2 1 [xw, yw]

-- Desaturation function
desaturate :: Double -> Matrix Double -> Matrix Double -> Matrix Double
desaturate alpha color white = customElementwise (+) (scaleMatrix alpha color) (scaleMatrix (1 - alpha) white)

-- Helper function to scale a matrix by a scalar
scaleMatrix :: Double -> Matrix Double -> Matrix Double
scaleMatrix scalar matrix = fromList 2 1 $ map (* scalar) (toList matrix)

-- Helper function to convert matrix to list
toList :: Matrix Double -> [Double]
toList mat = [ getElem i 1 mat | i <- [1..2] ]

-- Custom elementwise function to avoid conflict
customElementwise :: (Double -> Double -> Double) -> Matrix Double -> Matrix Double -> Matrix Double
customElementwise f m1 m2 = fromList (nrows m1) (ncols m1) $ zipWith f (toList m1) (toList m2)

-- Helper function to print matrix elements
printMatrix :: Matrix Double -> IO ()
printMatrix mat = do
    putStrLn $ "x: " ++ show (getElem 1 1 mat)
    putStrLn $ "y: " ++ show (getElem 2 1 mat)

-- Main function with detailed output
main :: IO ()
main = do
    let alpha = 0.75 -- 25% desaturation
    putStrLn "Original Color:"
    printMatrix originalColor
    putStrLn "\nWhite Point:"
    printMatrix whitePoint
    putStrLn $ "\nDesaturation Factor (alpha): " ++ show alpha

    let scaledOriginal = scaleMatrix alpha originalColor
    let scaledWhite = scaleMatrix (1 - alpha) whitePoint
    putStrLn "\nScaled Original Color:"
    printMatrix scaledOriginal
    putStrLn "\nScaled White Point:"
    printMatrix scaledWhite

    let desaturatedColor = desaturate alpha originalColor whitePoint
    putStrLn "\nDesaturated Color:"
    printMatrix desaturatedColor
